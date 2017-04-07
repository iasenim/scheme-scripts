(load  "mkprelude.scm")

(define seq
  (lambda (a b n p)
    (if (= n 1)
        (==  (cons a (cons b '())) p)
        (fresh (x y)
               (seq y b (- n 1) x)
               (== (cons a x) p)))))

(run* (q)
        (fresh (x)
               (seq 1 1 10 x)
               (== x q)))

(define bseq
  (lambda (b n s)
    (fresh (x)
           (if (= n 2)
               (== (cons b (cons x '())) s)
               (fresh (y)
                      (bseq y (- n 1) x)
                      (== (cons b x) s))))))
(run* (q)
      (fresh (x)
             (bseq 1 10 x)
             (== x q)))
(run* (q)
      (fresh (x)
             (seq 1 1 3 x)
             (conde 
              ((== '(1 1 1) x))
              ((== '(1 0 1) x))
              (else (== #t q)))))
