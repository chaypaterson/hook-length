; scheme program to compute the hook product of a partition, represented as a
; list of descending integers
; e.g. to find the hook length product of (4,3,1,1):
; (hook-prod '(4 3 1 1))
; returns 1680


(define (row-prod partn)
  (define (width partn)
    (let acc-width ((acc-total 0) (acc-partn partn))
      (if (null? acc-partn)
        acc-total
        (acc-width (+ acc-total (if (> (car acc-partn) 0) 1 0)) 
          (cdr acc-partn)))))
  (let acc-prod ((product 1) (acc-partn partn))
    (if (null? acc-partn)
      product
      (acc-prod 
        (* product 
          ((lambda (x) (if (> x 0) x 1)) 
            (- (+ (car acc-partn) (width acc-partn)) 1)))
        (cdr acc-partn)))))

(define (hook-prod partn)
  (define (shrink partn)
    (map (lambda (x) 
      (if (> x 0) (- x 1) 0))
      partn))
  (let acc-hook ((product 1) (acc-partn partn))
    (if (= 0 (car acc-partn))
      product
      (acc-hook (* product (row-prod acc-partn)) (shrink acc-partn)))))

; TODO function to check if a partition is valid
;(define (valid? partn)
;  (let temp-valid ((allok? #t) (temp-partn partn))
;    (if (null? temp-partn)
;      allok?
;      (temp-valid 
;        (and allok? (> (car temp-partn) (cadr temp-partn))) 
;        (cdr temp-partn)))))
