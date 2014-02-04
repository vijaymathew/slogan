;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(current-exception-handler (lambda (ex) (display-exception ex) (newline)))
(repl (current-input-port))