;; intersperse.scm
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;


;; Insert el element after each n-th element of list ls.
(define (intersperse el n ls)
  (if (zero? n) ls
    (let loop ((i   n)
               (ls  ls)
               (res '()))
      (cond ((null? ls) (reverse res))
            ((zero? i)  (loop n ls (cons el res)))
            (else       (loop (1- i) (cdr ls) (cons (car ls) res)))))))


;; end of file
;; vim: ts=4 sw=4 et
