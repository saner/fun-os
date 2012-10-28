(include "common.scm")
(include "c_fun_dec.scm")

(load "lib.scm")
(load "os.scm")

(define (test-closure c)
  (begin
    (c 2)))

(process (proc)
         (begin
           (let ((x 2))
                (test-closure (lambda (y) (begin (print-int 3) (print-int (+ x y)) (print-int 3)))))))

(process (proc2)
         (begin
           (let ((x 2))
                (print-int x))))

(process (proc3)
         (begin
                ((lambda (x) (print-int x)) 3)))

(global-fun user-code)
(define (user-code)
  (begin
    (enable-process (add-process proc3))))
