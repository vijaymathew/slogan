;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; A package system for Slogan.

(define (build-package pkg-path)
  (let* ((currdir (current-directory))
         (build-cmd (string-append "cd " pkg-path "; ./build; cd " currdir)))
    (let ((r (shell-command build-cmd)))
      (if (not (zero? r))
          (error "build-package - build failed -" build-cmd ", " r)
          #t))))

(define (install-git-package pkg-name pkg-url pkg-path)
  (let ((cmd (string-append "git clone " pkg-url " " pkg-path)))
    (let ((r (shell-command cmd)))
      (if (not (zero? r))
          (error "install-git-package - clone failed -" cmd ", " r)
          (build-package pkg-path))
      pkg-name)))

(define (copy-dir src dest)
  (let ((cmd (string-append "cp -R " src " " dest)))
    (let ((r (shell-command cmd)))
      (if (not (zero? r))
          (error "copy-dir - failed - " cmd ", " r)
          #t))))

(define (install-local-package pkg-name pkg-url pkg-path)
  (if (file-exists? pkg-url)
      (begin (copy-dir pkg-url pkg-path)
             (build-package pkg-path))
      (error "install-local-package - file not found - " pkg-url))
  pkg-name)

(define (load-package pkg-name compile-mode)
  (let ((pkg-init-path (string-append *pkg-root* pkg-name "/init")))
    (if (file-exists? (string-append pkg-init-path *obj-extn*))
        (load pkg-init-path)
        (if (file-exists? (string-append pkg-init-path *slgn-extn*))
            (if (compile pkg-init-path assemble: compile-mode)
                (if compile-mode
                    (load pkg-init-path)
                    (load (string-append pkg-init-path *scm-extn*)))
                (error "load-package - failed to compile " (string-append pkg-init-path *slgn-extn*)))
            (load pkg-init-path))))
  pkg-name)

(define (force-rm-dir path)
  (let ((r (shell-command (string-append "rm -rf " path))))
    (if (zero? r)
        #t
        (error "failed to remove directory - " path ", " r))))

(define (install_package pkg-name pkg-type pkg-url #!optional force)
  (if (not (file-exists? *pkg-root*))
      (create-directory *pkg-root*))
  (let* ((pkg-path (string-append *pkg-root* pkg-name))
         (pkg-path-old (string-append pkg-path ".bck")))
    (if (file-exists? pkg-path)
        (if (not force)
            (error "package already installed - " pkg-path)
            (rename-file pkg-path pkg-path-old)))
    (case pkg-type
      ((git) (install-git-package pkg-name pkg-url pkg-path))
      ((local) (install-local-package pkg-name pkg-url pkg-path))
      (else (error "install_package - type not supported -" pkg-type)))
    (if (file-exists? pkg-path-old)
        (force-rm-dir pkg-path-old))
    (load-package pkg-name #f)))

(define (uninstall_package pkg-name)
  (let ((pkg-path (string-append *pkg-root* pkg-name)))
    (if (file-exists? pkg-path)
        (if (force-rm-dir pkg-path)
            pkg-name
            #f)
        #f)))

(define (package-load? load-str)
  (if (string? load-str)
      (string-starts-with? load-str "pkg://")
      #f))

(define (package-name-from-load load-str)
  (substring load-str 6 (string-length load-str)))
