(global-fun scheme_entry)

(define (scheme_entry)
  (just (* 0 1) (* 1 1) (* (+ 1 1) (+ 2 2)) 3 (+ 5 5) (+ 5 6) (+ 1 (* 2 (+ 2 3)))))

(define (just x y z w a b c)
  (+ x (+ y (+ z (+ w (+ a (+ b c)))))))
