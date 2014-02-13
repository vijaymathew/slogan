;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (table #!key 
               (size #f)
               (init #f)
               (weak_keys #f)
               (weak_values #f)
               (test equal?)
               (hash #f)
               (min_load 0.45)
               (max_load 0.90))
  (cond (size
         (table-with-size size
                          init: init
                          weak-keys: weak_keys
                          weak-values: weak_values
                          test: test
                          hash: hash
                          min-load: min_load
                          max-load: max_load))
        (hash
         (make-table init: init
                     weak-keys: weak_keys
                     weak-values: weak_values
                     test: test
                     hash: hash
                     min-load: min_load
                     max-load: max_load))
        (else 
         (make-table init: init
                     weak-keys: weak_keys
                     weak-values: weak_values
                     test: test
                     min-load: min_load
                     max-load: max_load))))

(define (table-with-size size #!key 
                         (init #f)
                         (weak_keys #f)
                         (weak_values #f)
                         (test equal?)
                         (hash #f)
                         (min_load 0.45)
                         (max_load 0.90))
  (if hash
      (make-table size: size 
                  init: init
                  weak-keys: weak_keys
                  weak-values: weak_values
                  test: test
                  hash: hash
                  min-load: min_load
                  max-load: max_load)
      (make-table size: size 
                  init: init
                  weak-keys: weak_keys
                  weak-values: weak_values
                  test: test
                  min-load: min_load
                  max-load: max_load)))

(define is_table table?)      
(define table_length table-length)
(define table_at table-ref)
(define table_set table-set!)
(define table_search table-search)
(define table_for_each table-for-each)
(define table_to_list table->list)
(define table_copy table-copy)
