(define (scheme_entry)
  (add 0 1 2 3 4 5))

(define (add x y z w a b)
  (begin a (+ x (+ y (+ z (+ w (+ a b)))))))

(comment "ret 15")
