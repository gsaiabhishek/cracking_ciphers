#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
(map (lambda (z) (car z)) (cipmono1 (string->list ciphertext) '())))
(define (cipmono1 l1 l2)
  (if (null? l1) l2
     (if (char-alphabetic? (car l1)) (cipmono1 (cdr l1) (insertchar l2 (car l1))) (cipmono1 (cdr l1) l2))))
(define (insertchar l2 c)
  (if (contain? l2 c) (insertchar1 l2 c '())
      (append l2 (list (cons c 1)))))
(define (insertchar1 l2 c l3)
  (if (equal? (caar l2) c) (sortcharcdr (list (cons c (+ (cdar l2) 1))) (append l3 (cdr l2)) '())
      (insertchar1 (cdr l2) c (append l3 (list (car l2))))))
(define (sortcharcdr l1 l2 l3)
  (if (null? l2) (append l3 l1) (if (>= (cdar l1) (cdar l2)) (append l3 l1 l2)
      (sortcharcdr l1 (cdr l2) (append l3 (list (car l2)))))))
(define (contain? l2 c)
  (cond [(null? l2) #f]
        [(equal? (caar l2) c) #t]
        [else (contain? (cdr l2) c)]))
;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
 (map (lambda (z) (car z)) (cipbi cipher-word-list '())))
(define (cipbi l1 l2)
  (if (null? l1) l2
      (cipbi (cdr l1) (insertallbi (car l1) l2))))
(define (insertallbi s l2)
  (if (= (string-length s) 1) l2
      (insertallbi (substring s 1 (string-length s)) (insertchar l2 (substring s 0 2)))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
(cond [(equal? mode 'predecessor) (cunp cipher-bigrams-list '())]
      [(equal? mode 'successor) (cuns cipher-bigrams-list '())]
      [(equal? mode 'both) (cunb cipher-bigrams-list (cunp cipher-bigrams-list '()) (cuns cipher-bigrams-list '()) (list))]))
(define (cunp l1 l2)
  (if (null? l1) l2
      (cunp (cdr l1) (insertchar l2 (car (string->list (car l1)))))))
(define (cuns l1 l2)
  (if (null? l1) l2
      (cuns (cdr l1) (insertchar l2 (cadr (string->list (car l1)))))))
(define (cunb l l1 l2 l3)
  (cond [(and (null? l1) (null? l2))  (sort (map (lambda (z) (aatypbi z l)) l3) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
        [(null? l1)  (cunb l l2 '() l3)]
        [else (cunb l (cdr l1) (removepair l2 (car l1) '()) (cons (cons (caar l1) (+ (cdar l1) (cdrof l2 (car l1)))) l3))]))
(define (removepair l2 p l3)
  (cond [(null? l2) l3]
        [(equal? (caar l2) (car p)) (append l3 (cdr l2))]
        [else (removepair (cdr l2) p (append l3 (list (car l2))))]))
(define (cdrof l2 p)
  (cond [(null? l2) 0]
        [(equal? (caar l2) (car p)) (cdar l2)]
        [else (cdrof (cdr l2) p)]))
(define (aatypbi z l)
  (cond [(null? l) z]
        [(equal? (car l) (string (car z) (car z))) (aatypbi (cons (car z) (- (cdr z) 1)) (cdr l))]
        [else (aatypbi z (cdr l))]))
  

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cond [(equal? mode 'predecessor) (cnp cipher-word-list '())]
        [(equal? mode 'successor) (cns cipher-word-list '())]
        [(equal? mode 'both) (cnb cipher-word-list (cnp cipher-word-list '()) (cns cipher-word-list '()) '())]))
(define (cnp l1 l2)
  (if (null? l1) l2
      (cnp (cdr l1) (cnp1 (car l1) l2))))
(define (cnp1 s l2)
  (if (= (string-length s) 1) l2
      (cnp1 (substring s 1 (string-length s)) (insertchar l2 (car (string->list (substring s 0 1)))))))
(define (cns l1 l2)
  (if (null? l1) l2
      (cns (cdr l1) (cns1 (car l1) l2))))
(define (cns1 s l2)
  (if (= (string-length s) 1) l2
      (cns1 (substring s 1 (string-length s)) (insertchar l2 (car (string->list (substring s 1 2)))))))
(define (cnb l l1 l2 l3)
  (cond [(and (null? l1) (null? l2)) (sort (map (lambda (z) (aatypbili z l)) l3) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
        [(null? l1) (cnb l l2 '() l3)]
        [else (cnb l (cdr l1) (removepair l2 (car l1) '()) (cons (cons (caar l1) (+ (cdar l1) (cdrof l2 (car l1)))) l3))]))
(define (aatypbili z l)
  (if (null? l) z
      (aatypbili (aatypbili1 z (car l)) (cdr l))))
(define (aatypbili1 z s)
  (if (= (string-length s) 1) z
      (aatypbili1 (if (equal? (substring s 0 2) (string (car z) (car z))) (cons (car z) (- (cdr z) 1)) z) (substring s 1 (string-length s )))))
  

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (map (lambda (z) (car z)) (ciptri cipher-word-list '())))
(define (ciptri l l1)
  (if (null? l) l1 (ciptri (cdr l) (insertalltri (car l) l1))))
(define (insertalltri s l1)
  (if (<= (string-length s) 2) l1 (insertalltri (substring s 1 (string-length s)) (insertchar l1 (substring s 0 3)))))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (cipsingle cipher-word-list '()))
(define (cipsingle l1 l2)
  (cond [(null? l1) (map (lambda (z) (car z)) l2)]
        [(= (string-length (car l1)) 1) (cipsingle (cdr l1) (insertchar l2 (car l1)))]
        [else (cipsingle (cdr l1) l2)]))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
