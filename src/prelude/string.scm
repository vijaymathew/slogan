;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(define (string-endswith? s prefix)
  (let ((plen (string-length prefix))
        (slen (string-length s)))
    (if (and (not (zero? plen)) (not (zero? slen)) 
             (<= plen slen))
        (let ((subs (substring s (- slen plen) slen)))
          (string=? subs prefix))
        #f)))

(define string_ends_with? string-endswith?)