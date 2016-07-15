(library (counting)
  (export comb perm factorial)
  (import (chezscheme))
  
  (define comb
    (lambda (n k)
      (cond [(or (= k 0) (= k n)) 1]
            [(or (< k 0) (> k n) (< n 1)) (error "(comb n k)" "Invalid argument")]
            [else (let ([kp (if (> k (/ n 2.0))
                                (- n k)
                                k)])
                    (do ([i 1 (+ i 1)] [result 1 (* result (/ (- (+ n 1) i) i))])
                        ((> i kp) result)))])))
  
  (define factorial
    (lambda (n)
      (if (< n 0)
          (error "(factorial n)" "Invalid argument")
          (letrec ([fact-iter (lambda (m result)
                                (if (= m 0)
                                    result
                                    (fact-iter (- m 1) (* result m))))])
            (fact-iter n 1)))))

  (define perm
    (lambda (n k)
      (cond [(= k 0) 1]
            [(= k n) (factorial n)]
            [(or (< k 0) (> k n)) (error "(perm n k)" "Invalid argument") ]
            (else
             (letrec* ([n-k (- n k)]
                       [perm-iter (lambda (m result)
                                    (if (= m n-k)
                                        result
                                        (perm-iter (- m 1) (* result m))))])
               (perm-iter n 1))))))
  )
