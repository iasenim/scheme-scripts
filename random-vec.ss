(library (random-vec)
  (export
   make-list-random
   make-vector-random
   mean)
  (import (chezscheme))

  (define make-list-random
    (lambda (n r)
      (cond [(= n 0) '()]
            [(< n 0) (error "make-list-random" "Number of elements must be non-negative")]
            [else 
             (do ([i 0 (+ i 1)] [result '() (cons (r) result)])
                 ((= i n) result))])))
  
  (define make-vector-random
    (lambda (n r)
      (list->vector (make-list-random n r))))

  (define mean
    (lambda (items)
      (if (null? items)
          (error "mean" "List must be non-empty")
          (/ (fold-left + 0.0 items) (length items)))))
  
  ;;Example:
  ;; To make a list of 10 uniformly distributed number
  ;; (make-list-random 10 (lambda () (random 1.0)))
  )
