;; Copyright (c) 2013-2017 by Vijay Mathew Pandyalakal, All Rights Reserved.

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
  (let ((t (make-thread (lambda () (scm-apply fn args)) name)))
    (if group
        (task_group-tasks-add! group t))
    t))

(define (task-with-no-args fn name group)
  (let ((t (if name
               (make-thread fn name)
               (make-thread fn))))
    (if group
        (task_group-tasks-add! group t))
    t))

(define (root-task-with-args fn args name group ip op)
  (let ((t (make-root-thread (lambda () (scm-apply fn args)) name)))
    (if group
        (task_group-tasks-add! group t))
    t))

(define (root-task-with-no-args fn name group ip op)
  (let ((t (if name
               (make-root-thread fn name)
               (make-root-thread fn))))
    (if group
        (task_group-tasks-add! group t))
    t))

(define (task-data t)
  (let ((d (thread-specific t)))
    (if d (scm-car d) d)))

(define (task-data-set! t new-data)
  (let ((d (thread-specific t)))
    (if d
        (set-car! d new-data)
        (thread-specific-set! t (scm-cons new-data #f)))))

(define (task-binding t var)
  (let ((d (thread-specific t)))
    (if d
        (let ((b (scm-cdr d)))
          (if b
              (table-ref b var *void*)
              *void*))
        *void*)))

(define (task-binding-set! t var val)
  (let ((d (thread-specific t)))
    (if d
        (let ((b (scm-cdr d)))
          (if b
              (table-set! b var val)
              (let ((b (make-table)))
                (table-set! b var val)
                (set-cdr! d b))))
        (let ((b (make-table)))
          (table-set! b var val)
          (thread-specific-set! t (scm-cons #f b))))))

(define (task-binding-remove! t var)
  (task-binding-set! t var *void*))

(define is_task thread?)
(define self current-thread)
(define task_name thread-name)
(define task_data task-data)
(define task_set_data task-data-set!)
(define task_base_priority thread-base-priority)
(define task_set_base_priority thread-base-priority-set!)
(define task_priority_boost thread-priority-boost)
(define task_set_priority_boost thread-priority-boost-set!)
(define task_quantum thread-quantum)
(define task_set_quantum thread-quantum-set!)
(define task_run thread-start!)
(define task_yield thread-yield!)
(define task_sleep thread-sleep!)
(define task_resume thread-resume!)
(define task_suspend thread-suspend!)
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

(define (task_group #!optional (name *void*) parent)
  (scm-cons (scm-cons '*task-group* (make-mutex)) (scm-list '() parent name)))

(define (task_group? obj)
  (and (pair? obj) (pair? (scm-car obj))
       (eq? (scm-caar obj) '*task-group*)))

(define is_task_group task_group?)

(define (task_group_name tg)
  (list-ref (scm-cdr tg) 2))

(define (task_group_parent tg)
  (list-ref (scm-cdr tg) 1))

(define (task_group-tasks tg)
  (list-ref (scm-cdr tg) 0))

(define (task_group-tasks-add! tg t)
  (let ((tasks (task_group-tasks tg))
        (mtx (scm-cdr (scm-car tg))))
    (mutex-lock! mtx)
    (set-car! (scm-cdr tg) (scm-cons t tasks))
    (mutex-unlock! mtx)))

(define (task_group-change-task-state! tg f!)
  (let loop ((tasks (list-ref (scm-cdr tg) 0)))
    (if (scm-not (null? tasks))
        (begin (f! (scm-car tasks))
               (loop (scm-cdr tasks))))))

(define (task_group_resume tg)
  (task_group-change-task-state! tg thread-resume!))

(define (task_group_suspend tg)
  (task_group-change-task-state! tg thread-suspend!))

(define (task_group_terminate tg)
  (task_group-change-task-state! tg thread-terminate!))

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
               (scm-error "cannot rebind reactive variable to a new value"))
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
                  (if (scm-not (mutex-unlock! mtx cv timeout))
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
