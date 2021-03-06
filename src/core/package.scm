;; Copyright (c) 2013-2019 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; A package system for Slogan.

(define *build-script* "build")
(define *build-slogan-script* "build.sn")

(define (touch-default-pkg-init pkg-name pkg-init-path)
  (call-with-output-file pkg-init-path
    (lambda (p)
      (scm-display (string-append "init_package(\"" pkg-name "\")") p))))

(define (touch-default-pkg-build pkg-name pkg-path)
  (let ((init-file-name (string-append pkg-path "/init.sn")))
    (if (not (file-exists? init-file-name))
        (touch-default-pkg-init pkg-name init-file-name))
    (call-with-output-file (string-append pkg-path "/" *build-slogan-script*)
      (lambda (p)
        (scm-display "compile(\"init\")" p)))))

(define (build-package pkg-name pkg-path)
  (let ((build? (file-exists? (string-append pkg-path "/" *build-script*)))
        (build-slogan? (file-exists? (string-append pkg-path "/" *build-slogan-script*))))
    (cond
     ((scm-not (or build? build-slogan?))
      (touch-default-pkg-build pkg-name pkg-path)
      (build-package pkg-name pkg-path))
     ((or build? build-slogan?)
      (let* ((currdir (current-directory))
             (c1 (string-append "cd " pkg-path "; "))
             (c2 (if build?
                     (string-append "./" *build-script*)
                     (string-append "slogan -e ./" *build-slogan-script*)))
             (build-cmd (string-append c1 c2)))
        (let ((r (shell-command build-cmd)))
          (shell-command (string-append "cd " currdir))
          (if (scm-not (zero? r))
              (scm-error "build-package - build failed" build-cmd r)
              #t))))
     (else
      (scm-display "Skipping build step as no build script found...")
      (scm-newline)
      #t))))

(define (install-git-package pkg-name pkg-url pkg-path)
  (let ((cmd (string-append "git clone " pkg-url " " pkg-path)))
    (let ((r (shell-command cmd)))
      (if (scm-not (zero? r))
          (scm-error "install-git-package - clone failed" cmd r)
          (build-package pkg-name pkg-path))
      pkg-name)))

(define (get-file-name-from-pkg-url url)
  (let loop ((paths (string-split url #\/)))
    (if (null? (scm-cdr paths))
        (scm-car paths)
        (loop (scm-cdr paths)))))

(define *untar-cmds* '((".gz" . "tar xzf") (".bz2" . "tar xjf") (".zip" . "unzip")))

(define (untar-cmd file-name)
  (let ((extn (path-extension file-name)))
    (let ((cmd (scm-assoc extn *untar-cmds*)))
      (if cmd
          (let ((in-file (string-append *pkg-root* file-name)))
            (scm-cons (string-append (scm-cdr cmd) " " in-file
                                 (if (string=? extn ".zip")
                                     " -d " " -C ")
                                 *pkg-root*)
                  in-file))
          (scm-error "cannot decompress package" file-name)))))

(define (untar&build-package pkg-name pkg-url pkg-path)
  (let ((file-name (get-file-name-from-pkg-url pkg-url)))
    (let ((cmd&infile (untar-cmd file-name)))
      (let ((r (shell-command (scm-car cmd&infile))))
        (delete_file (scm-cdr cmd&infile))
        (if (zero? r)
            (build-package pkg-name pkg-path)
            (scm-error "failed to decompress package" (scm-car cmd&infile) r)))))
  pkg-name)
          
(define (install-remote-package pkg-name pkg-url pkg-path)
  (let ((cmd (string-append "curl " pkg-url " -o " (string-append *pkg-root* (get-file-name-from-pkg-url pkg-url)))))
    (let ((r (shell-command cmd)))
      (if (scm-not (zero? r))
          (scm-error "install-remote-package failed" cmd r)
          (untar&build-package pkg-name pkg-url pkg-path))))
  pkg-name)

(define (copy-dir src dest)
  (let ((cmd (string-append "cp -R " src " " dest)))
    (let ((r (shell-command cmd)))
      (if (scm-not (zero? r))
          (scm-error "copy-dir failed" cmd r)
          #t))))

(define (install-local-package pkg-name pkg-url pkg-path)
  (if (file-exists? pkg-url)
      (if (string=? "" (path-extension pkg-url))
          (begin (copy-dir pkg-url pkg-path)
                 (build-package pkg-name pkg-path))
          (begin (copy-file pkg-url (string-append *pkg-root* (get-file-name-from-pkg-url pkg-url)))
                 (untar&build-package pkg-name pkg-url pkg-path)))
      (scm-error "install-local-package - file not found" pkg-url))
  pkg-name)

(define (load_package pkg-name)
  (let ((pkg-init-path (string-append *pkg-root* pkg-name "/init")))
    (if (scm-not (file-exists? pkg-init-path))
        (touch-default-pkg-init pkg-name pkg-init-path))
    (with-exception-catcher
     (lambda (e)
       (if (no-such-file-or-directory-exception? e)
           (scm-raise e)
           (show_exception e)))
     (lambda () (slgn-load pkg-init-path)))
    pkg-name))

(define (init_package pkg-name)
  (let ((load-path (string-append (read-slogan-root) "/packages/" pkg-name)))
    (begin (if (scm-not (file-exists? load-path))
               (set! load-path "."))
           (slgn-link (string-append load-path "/src/core")))))

(define (force-rm-dir path)
  (let ((r (shell-command (string-append "rm -rf " path))))
    (if (zero? r)
        #t
        (scm-error "failed to remove directory" path r))))

(define (install_package pkg-name pkg-type pkg-url #!optional force)
  (if (scm-not (file-exists? *pkg-root*))
      (create-directory *pkg-root*))
  (let* ((pkg-path (string-append *pkg-root* pkg-name))
         (pkg-path-old (string-append pkg-path ".bck")))
    (if (file-exists? pkg-path)
        (if (scm-not force)
            (scm-error "package already installed" pkg-path)
            (rename-file pkg-path pkg-path-old)))
    (with-exception-catcher
     (lambda (e)
       (force-rm-dir pkg-path-old)
       (force-rm-dir pkg-path)
       (scm-raise e))
     (lambda ()
       (case pkg-type
         ((git) (install-git-package pkg-name pkg-url pkg-path))
         ((remote) (install-remote-package pkg-name pkg-url pkg-path))
         ((local) (install-local-package pkg-name pkg-url pkg-path))
         (else (scm-error "install_package - type not supported" pkg-type)))))
    (if (file-exists? pkg-path-old)
        (force-rm-dir pkg-path-old))
    (load_package pkg-name)))

(define (uninstall_package pkg-name)
  (let ((pkg-path (string-append *pkg-root* pkg-name)))
    (if (file-exists? pkg-path)
        (if (force-rm-dir pkg-path)
            pkg-name
            #f)
        #f)))
