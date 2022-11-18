(declare (uses queens))

(import (chicken process-context))

(define (main args)
  (if (null? args)
      (print "Usage: __cmd__ number-of-queens")
      (let* ((val (car args))
             (n (string->number val)))
        (output-boards (queens n)))))

(main (command-line-arguments))
