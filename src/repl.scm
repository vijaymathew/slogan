;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(load (string-append (getenv "SLOGAN_PRELUDE_ROOT") "prelude"))

(repl (make-tokenizer (current-input-port)))