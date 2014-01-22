;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define *prelude-root* #f) ;; initialized in "consts".

(load (string-append (getenv "SLOGAN_PRELUDE_ROOT" "./prelude/") "consts"))
(load (string-append *prelude-root* "array"))
(load (string-append *prelude-root* "list"))
(load (string-append *prelude-root* "char"))
(load (string-append *prelude-root* "string"))
(load (string-append *prelude-root* "table"))
(load (string-append *prelude-root* "task"))
(load (string-append *prelude-root* "env"))
(load (string-append *prelude-root* "extn"))
(load (string-append *prelude-root* "util"))
(load (string-append *prelude-root* "tokenizer"))
(load (string-append *prelude-root* "parser"))
(load (string-append *prelude-root* "compiler"))
