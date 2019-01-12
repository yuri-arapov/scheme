;; some more Scheme utils

;; take first n elements out of list ls.
;; n can be greater than length of the list.
(define (first-n ls n)
  (let loop ((k n) (ls ls) (res '()))
    (if (or (null? ls) (zero? k)) (reverse res)
      (loop (- k 1) (cdr ls) (cons (car ls) res)))))


;; remove first n elements out of list ls.
;; n can be greater than length of the list.
(define (but-first-n ls n)
  (if (or (zero? n) (null? ls)) ls
    (but-first-n (cdr ls) (- n 1))))


;; group elements of the list.
;; example:
;;   (group-by '(1 2 3 4 5) 2) -> ((1 2) (3 4) (5))
(define (group-by ls n)
  (let loop ((ls ls) (res '()))
    (if (null? ls) (reverse res)
      (loop (but-first-n ls n) (cons (first-n ls n) res)))))


;; end of file
