(define (scheme_entry)
  (add 0 9 9 9 0 1))

(define (add x0 b c d x x1)
  (+ (if (not (> x0 x1)) (+ x1 (* x1 x0)) (+ x1 (* x1 (+ x1 (* x0 x1)))))
     (+ (+ x1 (* x1 x0)) 
        (+ x1 (* x1 (+ x1 (* x0 x1)))))))

(comment "ret 1 or 2")
