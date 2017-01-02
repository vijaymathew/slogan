;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; Utility functions for interacting with the host operating system.
;; These functions are specific to this implementation of Slogan.
;; Right now we have some of the most useful Gambit "Host environment" functions here.

(c-declare #<<c-declare-end

#include <unistd.h>
#include <signal.h>

c-declare-end
)

(define scm-getpid (c-lambda () int "getpid"))
(define scm-getppid (c-lambda () int "getppid"))
(define scm-kill (c-lambda (int int) int "kill")) 

(define (list_directory dirname #!optional (ignore_hidden #t))
  (let ((d (open-directory (scm-list path: dirname ignore-hidden: (if (symbol? ignore_hidden) 
                                                                  (slgn-symbol->scm-sym/kw ignore_hidden string->symbol) 
                                                                  ignore_hidden)))))
    (let ((ls (read-all d)))
      (close-input-port d)
      ls)))

(define (create_directory path #!optional (permissions #o777))
  (create-directory (scm-list path: path permissions: permissions)))

(define delete_directory delete-directory)
(define current_directory current-directory)

(define create_link create-link)
(define create_symbolic_link create-symbolic-link)
(define rename_file rename-file)
(define copy_file copy-file)
(define delete_file delete-file)

(define path_expand path-expand)
(define path_normalize path-normalize)

(define path_extension path-extension)
(define path_strip_extension path-strip-extension)
(define path_directory path-directory)
(define path_strip_directory path-strip-directory)
(define path_strip_trailing_directory_separator path-strip-trailing-directory-separator)
(define path_volume path-volume)
(define path_strip_volume path-strip-volume)

(define shell_command shell-command)
(define command_line command-line)

(define file_exists file-exists?)
(define file_info file-info)
(define is_file_info file-info?)

(define (file_info_type fi)
  (scm-symbol->slgn-sym (file-info-type fi)))

(define file_info_device file-info-device)
(define file_info_inode file-info-inode)
(define file_info_mode file-info-mode)
(define file_info_number_of_links file-info-number-of-links)
(define file_info_owner file-info-owner)
(define file_info_group file-info-group)
(define file_info_size file-info-size)

(define (file_info_last_access_time fi)
  (time->seconds (file-info-last-access-time fi)))

(define (file_info_last_modification_time fi)
  (time->seconds (file-info-last-modification-time fi)))

(define (file_info_last_change_time fi)
  (time->seconds (file-info-last-change-time fi)))

(define file_info_attributes file-info-attributes)

(define (file_info_creation_time fi)
  (time->seconds (file-info-creation-time fi)))

(define file_type file-type)
(define file_device file-device)
(define file_inode file-inode)
(define file_mode file-mode)
(define file_number_of_links file-number-of-links)
(define file_owner file-owner)
(define file_group file-group)
(define file_size file-size)

(define (file_last_access_time path)
  (time->seconds (file-last-access-time path)))

(define (file_last_modification_time path)
  (time->seconds (file-last-modification-time path)))

(define (file_last_change_time path)
  (time->seconds (file-last-change-time path)))

(define file_attributes file-attributes)

(define (file_creation_time path)
  (time->seconds (file-creation-time path)))

(define host_name host-name)
(define host_info host-info)
(define is_host_info host-info?)
(define host_info_name host-info-name)
(define host_info_aliases host-info-aliases)
(define host_info_addresses host-info-addresses)

(define (os_name)
  (with-exception-catcher
   (lambda (e) #f)
   (lambda ()
     (let ((p (open-pipe-stream "uname")))
       (let ((n (scm-read p)))
         (close_stream p)
         n)))))
