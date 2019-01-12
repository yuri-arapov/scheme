;; Scheme utils.


;; (dotimes (var times) ...)
;; (dotimes (var from to) ...)
;; (dotimes (var from to step) ...)
(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (var times) body ...)
     (dotimes (var 0 (1- times)) body ...))

    ((dotimes (var from to) body ...)
     (dotimes (var from to 1) body ...))

    ((dotimes (var from to step) body ...)
     (let ((_to to) (_step step))
       (do ((var from (+ var _step)))
           ((> var _to))
           body ...)))))


;; (dotimes-rev (var times) ...)
;; (dotimes-rev (var from to) ...)
;; (dotimes-rev (var from to step) ...)
(define-syntax dotimes-rev
  (syntax-rules ()
    ((dotimes-rev (var times) body ...)
     (dotimes-rev (var (1- times) 0) body ...))

    ((dotimes-rev (var from to) body ...)
     (dotimes-rev (var from to 1) body ...))

    ((dotimes-rev (var from to step) body ...)
     (let ((_to to) (_step step))
       (do ((var from (- var _step)))
           ((< var _to))
           body ...)))))


;; Currying.
;; Lots of thanks goes to Maciej Pacula:
;;   https://github.com/mpacula/Scheme-Power-Tools/blob/master/misc-utils.scm
(define (curry fn . args)
  (lambda rest-args (apply fn (append args rest-args))))

(define (rcurry fn . args)
  (lambda first-args (apply fn (append first-args args))))


;; Identity.
(define (identity x) x)


;; Thanks to WJ from comp.lang.lisp/scheme (google groups)
;; Example:
;;   (define (decorate alist) (map (cut cons (random 1.0) <>) alist))
;;   (define (shuffle x) (map cdr (sort (decorate x) (with < car))))
;;   (shuffle (iota 10))
;;     -> (6 1 5 8 0 3 7 9 4 2)
(define (with f g) (lambda (a b) (f (g a) (g b))))


;; Thanks to WJ from comp.lang.lisp/scheme (google groups).
(define (hook> f g) (lambda (a b) (f a (g b))))


;; Compose list of functions into single one.
(define (compose . funcs)
  (lambda (x) 
    (fold-right (lambda (fn res) (fn res)) x funcs)))


;; Generate a list with elements produced by (elem-fn seed) until
;; it returns false.
;; Start seed with init, produce next seed by (next-fn seed).
;; Unlike `unfold' it uses just two functions.
(define (generate-list elem-fn next-fn init)
  (let loop ((seed init) (res '()))
    (let ((e (elem-fn seed)))
      (if (not e)
        (reverse res)
        (loop (next-fn seed) (cons e res))))))


;; call/cc wrapper.
(define-syntax with-return
  (syntax-rules ()
    ((with-return (name) body ...)
     (call/cc (lambda (name) body ...)))))


;; Read file line by line, return list of elements obtained by
;; apllying proc on each line.
(define (read-file-with file proc)
  (define (read-lines port)
    (let loop ((line (read-line port))
               (res '()))
      (if (eof-object? line)
        (reverse res)
        (loop (read-line port) (cons (proc line) res)))))
  (call-with-input-file file read-lines))


;; Read file line by line and return list of strings.
(define (read-file file)
  (read-file-with file (lambda (line) line)))


;; Increment value conditionally.
(define-syntax cond-inc
  (syntax-rules ()
    ((cond-inc condition value increment)
     (if condition (+ value increment) value))

    ((cond-inc condition value)
     (cond-inc condition value 1))

    ((cond-inc condition)
     (cond-inc condition 0 1))

    ((cond-inc)
     (error "bad COND-INC syntax: (COND-INC CONDITION [VALUE [INCREMENT]]) expected"))))


;; Open output file `file', bind port obtained as `port' 
;; and then run chunk of code `body ...'.
;; Example:
;;   (with-output-file o "out" (for-each (curry format o "~a~%") (iota 10)))
(define-syntax with-output-file
  (syntax-rules ()
    ((with-output-file (file port) body ...)
     (call-with-output-file 
       file
       (lambda (port) body ...)))))


;; Return undefined value.
(define (undefined)
  (if #f #F))


;; Make a function that evaluates each predicate until it returns true.
;; Example:
;;   (define negative-or-odd? (one-of negative? odd?))
;;   (negative-or-odd? 0) -> #f
;;   (negative-or-odd? 1) -> #t
(define (one-of . predicates)
  (if (null? predicates)
    (lambda (_) #f)
    (lambda (a) 
      (or ((car predicates) a) 
          ((apply one-of (cdr predicates)) a)))))


;; Read input file `file' (or stdin if `file' is #f) and invoke
;; `(proc line res)' for each line read (much like `fold' iterates over list).
;; Return result of last invocation of `proc'.
;; Stop and return #f when `proc' returns #f.
;; Example (lines counting):
;;   (iterate-input-file (lambda (line res) (1+ res)) 0 "some-file")
(define (iterate-input-file proc init file)
  (let ((port (if file (open-input-file file) (current-input-port))))

    (define (done res)
      (if file (close-input-port port))
      res)

    (let loop ((line (read-line port))
               (res init))
      (if (eof-object? line) (done res)
        (let ((x (proc line res)))
          (if (not x) (done x)
            (loop (read-line port) x)))))))


;; end of file
;; vim: ts=4 sw=4 et
