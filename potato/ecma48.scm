(define-module (potato ecma48)
  #:export (underline
            reset))

(define (underline)
  (display (string #\escape #\[ #\4 #\m)))

(define (reset)
  (display (string #\escape #\[ #\0 #\m)))
         
