;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (task fn #!key args suspended name group)
  (let ((t (if args 
               (task-with-args fn args name group)
               (task-with-no-args fn name group))))
    (if (scm-not suspended)
        (thread-start! t))
    t))

(define (root_task fn #!key args suspended name group 
                   (input_port (current-input-port)) 
                   (output_port (current-output-port)))
  (let ((t (if args 
               (root-task-with-args fn args name group input_port output_port)
               (root-task-with-no-args fn name group input_port output_port))))
    (if (scm-not suspended)
        (thread-start! t))
    t))

(define (task-with-args fn args name group)
  (if group
      (make-thread (lambda () (scm-apply fn args)) name group)
      (make-thread (lambda () (scm-apply fn args)) name)))

(define (task-with-no-args fn name group)
  (if group
      (make-thread fn name group)
      (if name 
          (make-thread fn name)
          (make-thread fn))))

(define (root-task-with-args fn args name group ip op)
  (if group
      (make-root-thread (lambda () (scm-apply fn args)) name group ip op)
      (make-root-thread (lambda () (scm-apply fn args)) name)))

(define (root-task-with-no-args fn name group ip op)
  (if group
      (make-root-thread fn name group ip op)
      (if name 
          (make-root-thread fn name)
          (make-root-thread fn))))

(define is_task thread?)
(define self current-thread)
(define task_name thread-name)
(define task_data thread-specific)
(define task_set_data thread-specific-set!)
(define task_base_priority thread-base-priority)
(define task_set_base_priority thread-base-priority-set!)
(define task_priority_boost thread-priority-boost)
(define task_set_priority_boost thread-priority-boost-set!)
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

(define task_state thread-state)
(define task_state_is_uninitialized thread-state-uninitialized?)
(define task_state_is_initialized thread-state-initialized?)
(define task_state_is_active thread-state-active?)
(define task_state_active_waiting_for thread-state-active-waiting-for)
(define task_state_active_timeout thread-state-active-timeout)
(define task_state_is_normally_terminated thread-state-normally-terminated?)
(define task_state_normally_terminated_result thread-state-normally-terminated-result)
(define task_state_is_abnormally_terminated thread-state-abnormally-terminated?)
(define task_state_abnormally_terminated_reason thread-state-abnormally-terminated-reason)

(define task_group make-thread-group)
(define is_task_group thread-group?)
(define task_group_name thread-group-name)
(define task_group_parent thread-group-parent)
(define task_group_resume thread-group-resume!)
(define task_group_suspend thread-group-suspend!)
(define task_group_terminate thread-group-terminate!)

(define mutex make-mutex)
(define is_mutex mutex?)
(define mutex_data mutex-specific)
(define mutex_set_data mutex-specific-set!)
(define mutex_name mutex-name)
(define mutex_lock mutex-lock!)
(define mutex_unlock mutex-unlock!)

(define (mutex_state mtx)
  (let ((state (mutex-state mtx)))
    (if (symbol? state)
        (scm-symbol->slgn-sym state)
        state)))

(define monitor make-condition-variable)
(define is_monitor condition-variable?)
(define monitor_name condition-variable-name)
(define monitor_data condition-variable-specific)
(define monitor_set_data condition-variable-specific-set!)
(define monitor_notify condition-variable-signal!)
(define monitor_broadcast condition-variable-broadcast!)

;; reactive or dataflow variables.
(define-structure reactive-var cv mtx)

(define (rvar)
  (let ((cv (make-condition-variable)))
    (condition-variable-specific-set! cv '*unbound*)
    (make-reactive-var cv (make-mutex))))

(define (rbind dfv value)
  (mutex-lock! (reactive-var-mtx dfv))
  (let ((cv (reactive-var-cv dfv))
	(err #f))
    (if (and (scm-not (unbound? (condition-variable-specific cv)))
             (scm-not (equal? value (condition-variable-specific cv))))
        (begin (mutex-unlock! (reactive-var-mtx dfv))
               (error "cannot rebind reactive variable to a new value."))
        (begin (condition-variable-specific-set! cv value)
               (condition-variable-broadcast! cv)
               (mutex-unlock! (reactive-var-mtx dfv))))))

(define (rget-with-timeout dfv timeout default)
  (call/cc
   (lambda (return)
     (cond ((reactive-var? dfv)
            (let ((mtx (reactive-var-mtx dfv))
                  (cv (reactive-var-cv dfv)))
              (mutex-lock! mtx)
              (if (unbound? (condition-variable-specific cv))
                  (if (not (mutex-unlock! mtx cv timeout))
                      (return default))
                  (mutex-unlock! mtx))
              (condition-variable-specific cv)))
           (else dfv)))))

(define (rget dfv #!optional timeout default)
  (if timeout
      (rget-with-timeout dfv timeout default)
      (cond ((reactive-var? dfv)
             (let ((mtx (reactive-var-mtx dfv))
                   (cv (reactive-var-cv dfv)))
               (mutex-lock! mtx)
               (if (unbound? (condition-variable-specific cv))
                   (mutex-unlock! mtx cv)
                   (mutex-unlock! mtx))
               (condition-variable-specific cv)))
            (else dfv))))

(set! scm-rvar rvar)
(set! scm-rbind rbind)
(set! scm-rget rget)
