;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (task fn #!optional args name group)
  (if args 
      (task-with-args fn args name group)
      (task-with-no-args fn name group)))

(define (task-with-args fn args name group)
  (if group
      (make-thread (lambda () (apply fn args)) name group)
      (make-thread (lambda () (apply fn args)) name)))

(define (task-with-no-args fn name group)
  (if group
      (make-thread fn name)
      (make-thread fn)))

(define root_task make-root-thread)
(define is_task thread?)
(define current_task current-thread)
(define task_name thread-name)
(define task_data thread-specific)
(define task_set_data thread-specific-set!)
(define task_base_priority thread-base-priority)
(define task_set_base_priority thread-base-priority-set!)
(define task_quantum thread-quantum)
(define task_set_quantum thread-quantum-set!)
(define task_run thread-start!)
(define task_yield thread-yield!)
(define task_sleep thread-sleep!)
(define task_terminate thread-terminate!)
(define task_join thread-join!)
(define task_send thread-send)
(define task_receive thread-receive)
(define task_messages_next thread-mailbox-next)

(define (task_messages_rewind #!optional remove_last_read) 
  (if remove_last_read 
      (thread-mailbox-extract-and-rewind)
      (thread-mailbox-rewind)))

(define mutex make-mutex)
(define is_mutex mutex?)
(define mutex_data mutex-specific)
(define mutex_set_data mutex-specific-set!)
(define mutex_name mutex-name)
(define mutex_state mutex-state)
(define mutex_lock mutex-lock!)
(define mutex_unlock mutex-unlock!)

(define condition_variable make-condition-variable)
(define is_condition_variable condition-variable?)
(define condition_variable_name condition-variable-name)
(define condition_variable_data condition-variable-specific)
(define condition_variable_set_data condition-variable-specific-set!)
(define condition_variable_signal condition-variable-signal!)
(define condition_variable_broadcast condition-variable-broadcast!)

;; reactive or dataflow variables.
(define (rvar)
  (let ((cv (make-condition-variable)))
    (condition-variable-specific-set! cv '*unbound*)
    (cons cv (make-mutex))))

(define (rbind dfv value)
  (let ((cv (car dfv)))
    (if (unbound? (condition-variable-specific cv))
        (begin (condition-variable-specific-set! cv value)
               (condition-variable-broadcast! cv))
        (error "cannot rebind reactive variable."))))

(define (rget dfv)
  (let ((cv (car dfv))
        (m (cdr dfv)))
    (if (unbound? (condition-variable-specific cv))
        (mutex-unlock! m cv))
    (condition-variable-specific cv)))