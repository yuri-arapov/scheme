#!/usr/local/bin/guile -s
!#


(use-modules (ice-9 rdelim))    ;; (read-line port)
(use-modules (ice-9 format))    ;; (format ...) that supports width specifier
(use-modules (srfi srfi-69))    ;; hash tables


;; Read input file `file' (or stdin if `file' is #f) and invoke
;; `(proc line res)' for each line read (much like `fold' iterates over list).
;; Return result of last invocation of `proc'.
;; Stop and return #f wten `proc' returns #f.
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


;; Split line into words.  Words in line are separated by space char(s).
;; Return list of words.
(define (line->words line)
  (filter
    (lambda (s) (not (string=? "" s)))
    (string-split line #\space)))



(define prog (car (command-line)))  ;; program name
(define args (cdr (command-line)))  ;; program arguments


;; Print help message.
(define (print-help)
  (format #t "~a is a program to format CDR per-type counts report~%" prog)
  (format #t "usage:~%")
  (format #t "   ~a [file ...]~%" prog)
  (format #t "   ~a -h~%" prog)
)


;; Print name value pair nicely.
(define (print-name-value name value)
  (format #t "~25a ~12@a~%" name
          (let ((n (if (number? value) value
                     (string->number value))))
            (if (number? n) (format #f "~:d" n)
              value))))


;; Check if user cries for help.
(if (or (member "-h" args)
        (member "-help" args)
        (member "--help" args))
  (begin
    (print-help)
    (exit 0)))


;; Hash table to accumulate summary (total counts).
(define summary (make-hash-table string=?))
(define (summary-add! key value)
  (hash-table-set! summary key
    (+ value (hash-table-ref summary key (lambda () 0)))))


;; Read input file and reformat each `name value' line so than numerical values
;; are pretty printed (comma added after each 3rd digit) to stdout.
;; Sum up values for each name.
(for-each

  (lambda (file)
    (iterate-input-file
      (lambda (line _res)
        (let ((words (line->words line)))
          (if (not (= 2 (length words))) 
            (format #t "~a~%" line) ;; pass line as it is
            (let* ((name   (car words))                 ;; name (string)
                   (svalue (cadr words))                ;; value (string)
                   (value  (string->number svalue)))    ;; value (number)
              (print-name-value name svalue)
              (if value (summary-add! name value))))))
      #t
      file))

  (if (null? args) 
    '(#f)   ;; the only source is stdin
    args))  ;; all the files from command line


(format #t "Total~%")
(for-each
  (lambda (e) (print-name-value (car e) (cdr e)))
  (sort (hash-table->alist summary) (lambda (x y) (string<? (car x) (car y)))))


;; end of file
;; vim: et ts=4 sw=4
