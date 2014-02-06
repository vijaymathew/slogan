;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

;; Utility functions for interacting with the host operating system.
;; These functions are specific to this implementation of Slogan.
;; Right now we have some of the most useful Gambit "Host environment" functions here.

(define current_directory current-directory)
(define path_expand path-expand)
(define path_normalize path-normalize)
(define path_extension path-extension)
(define path_strip_extension path-strip-extension)
(define path_directory path-directory)
(define path_strip_directory path-strip-directory)
(define path_strip_trailing_directory_separator path-strip-trailing-directory-separator)
(define path_volumn path-volume)
(define path_strip_volume path-strip-volume)

(define (create_directory path-or-settings)
  (create-directory (slgn-path/settings->scm-path/settings path-or-settings)))

(define (create_fifo path-or-settings)
  (create-fifo (slgn-path/settings->scm-path/settings path-or-settings)))

(define create_link create-link)
(define create_symbolic_link create-symbolic-link)
(define rename_file rename-file)
(define copy_file copy-file)
(define delete_file delete-file)
(define delete_directory delete-directory)

(define (directory_files path-or-settings)
  (directory-files (slgn-path/settings->scm-path/settings path-or-settings)))

(define shell_command shell-command)
(define command_line command-line)

(define current_time current-time)
(define is_time time?)
(define time_to_seconds time->seconds)
(define seconds_to_time seconds->time)
(define process_times process-times)
(define cpu_time cpu-time)
(define real_time real-time)

(define (benchmark expr-string)
  (let ((tokenizer (make-tokenizer (open-input-string expr-string))))
    (let ((expr (expression tokenizer)))
      (time (eval expr)))))

(define file_exists file-exists?)
(define file_info file-info)
(define is_file_info file-info?)
(define file_info_type file-info-type)
(define file_type file-type)
(define file_device file-device)
(define file_inode file-inode)
(define file_mode file-mode)
(define file_number_of_links file-number-of-links)
(define file_owner file-owner)
(define file_group file-group)
(define file_size file-size)
(define file_last_access_time file-last-access-time)
(define file_last_modification_time file-last-modification-time)
(define file_last_change_time file-last-change-time)
(define file_attributes file-attributes)
(define file_creation_time file-creation-time)



  