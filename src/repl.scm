;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (repl-exception-handler ex)
  (if (error-exception? ex)
      (display "error: "))
  (display-exception ex))

(current-exception-handler repl-exception-handler)
(repl (current-input-port))