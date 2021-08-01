#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (etai key)
 (etai1 (map (lambda (z) (car (string->list z))) (stats:cipher-common-words-single utils:cipher-word-list))
        (arrange 0 (stats:cipher-monograms utils:ciphertext) (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both) '()) key))
(define (arrange c l1 l2 l3)
  (cond [(= c 5) (map (lambda (z) (car z)) (sort l3 (lambda (x y) (if (> (cdr x) (cdr y)) #t #f))))]
        [else (arrange (+ c 1) (cdr l1) l2 (cons (search (car l1) l2) l3))]))
(define (search c l2)
  (if (char=? c (caar l2)) (car l2) (search c (cdr l2))))
(define (contain? c l)
  (cond [(null? l) #f]
        [(char=? c (car l)) #t]
        [else (contain? c (cdr l))]))
(define (remove ca l l1)
  (cond [(char=? ca (car l)) (append l1 (cdr l))]
        [else (remove ca (cdr l) (append l1 (list (car l))))]))

(define (etai1 l1 l k)
   (let ([lr1 (list-ref l 0)]
         [lr2 (list-ref l 1)]
         [lr3 (list-ref l 2)]
         [lr4 (list-ref l 3)]
         [lr5 (list-ref l 4)])
    (cond [(= 0 (length l1))
                              (list (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr3) (cons #\I lr4))
                                  (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr2) (cons #\I lr4)) (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr4) (cons #\I lr3))
                                  (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr3) (cons #\I lr2)) (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr4) (cons #\I lr2))
                                  
                                  (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr2) (cons #\I lr5)) 
                                  (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr3) (cons #\I lr5)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr3) (cons #\I lr2))
                                  (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr5) (cons #\I lr2)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr5) (cons #\I lr3))

                                  (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr2) (cons #\I lr4)) (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr2) (cons #\I lr5))
                                  (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr4) (cons #\I lr2))
                                  (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr5) (cons #\I lr2)) (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr5) (cons #\I lr4))
                               
                                  (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr1) (cons #\I lr3)) (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr1) (cons #\I lr4))
                                  (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr3) (cons #\I lr4)) (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr3) (cons #\I lr1))
                                  (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr4) (cons #\I lr1)) (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr4) (cons #\I lr3)) 
                                 
                                  (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr1) (cons #\I lr3)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr1) (cons #\I lr5)) 
                                  (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr3) (cons #\I lr5)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr3) (cons #\I lr1))
                                  (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr5) (cons #\I lr3))
                                  
                                  (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr1) (cons #\I lr4)) 
                                  (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr2) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr4) (cons #\I lr1)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr4) (cons #\I lr2))
                                  
                                  (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr1) (cons #\I lr5))
                                  (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr2) (cons #\I lr5)) (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr5) (cons #\I lr2))
                                  
                                  (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr3) (cons #\I lr4)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr3) (cons #\I lr5))
                                  (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr4) (cons #\I lr3))
                                  (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr5) (cons #\I lr3)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr5) (cons #\I lr4))
                                  
                                  (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr1) (cons #\I lr4)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr1) (cons #\I lr5))
                                  (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr4) (cons #\I lr1))
                                  (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr5) (cons #\I lr4))
                                  
                                  (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr1) (cons #\I lr3))
                                  (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr3) (cons #\I lr1)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr3) (cons #\I lr2)) 
                                  
                                  (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr1) (cons #\I lr3))
                                  (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr3) (cons #\I lr1)) (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr3) (cons #\I lr2))
                                  
                                  (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr1) (cons #\I lr5))
                                  (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr2) (cons #\I lr5)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr5) (cons #\I lr2))
                                  
                                  (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr1) (cons #\I lr2)) (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr1) (cons #\I lr4))
                                  (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr2) (cons #\I lr4)) (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr2) (cons #\I lr1))
                                  (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr4) (cons #\I lr1)) (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr4) (cons #\I lr2))
                                  
                                  (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr1) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr1) (cons #\I lr5))
                                  (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr4) (cons #\I lr1))
                                  (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr5) (cons #\I lr4))
                                  
                                  (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr1) (cons #\I lr3)) (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr1) (cons #\I lr5))
                                  (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr3) (cons #\I lr5)) (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr3) (cons #\I lr1))
                                  (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr5) (cons #\I lr1)) (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr5) (cons #\I lr3))
                                  
                                  (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr1) (cons #\I lr3)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr1) (cons #\I lr4))
                                  (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr3) (cons #\I lr4)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr3) (cons #\I lr1))
                                  (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr4) (cons #\I lr1)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr4) (cons #\I lr3))
                                  
                                  (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr3) (cons #\I lr4)) (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr3) (cons #\I lr5))
                                  (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr4) (cons #\I lr3))
                                  (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr5) (cons #\I lr3)) (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr5) (cons #\I lr4))
                                  
                                  (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr2) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr2) (cons #\I lr5))
                                  (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr4) (cons #\I lr5)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr4) (cons #\I lr2))
                                  (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr5) (cons #\I lr2)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr5) (cons #\I lr4))
                                  
                                  (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr2) (cons #\I lr5))
                                  (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr3) (cons #\I lr5)) (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr3) (cons #\I lr2))
                                  (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr5) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr5) (cons #\I lr3))
                                  
                                  (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr2) (cons #\I lr3)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr2) (cons #\I lr4))
                                  (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr3) (cons #\I lr4)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr3) (cons #\I lr2))
                                  (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr4) (cons #\I lr2)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr4) (cons #\I lr3)) )]
          [(= 1 (length l1)) (if (contain? (car l1) l) (let* ([ca (car l1)]
                                                              [reml1 (remove ca l '())]
                                                              [z1 (list-ref reml1 0)]
                                                              [z2 (list-ref reml1 1)]
                                                              [z3 (list-ref reml1 2)]
                                                              [z4 (list-ref reml1 3)])
                                (list  (list (cons #\E z1) (cons #\T z4) (cons #\A ca) (cons #\I z2)) (list (cons #\E z1) (cons #\T z4) (cons #\A ca) (cons #\I z3))
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A ca) (cons #\I z2)) (list (cons #\E z1) (cons #\T z3) (cons #\A ca) (cons #\I z4))
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A ca) (cons #\I z1)) (list (cons #\E z2) (cons #\T z4) (cons #\A ca) (cons #\I z3))
                                       
                                       (list (cons #\E z2) (cons #\T z3) (cons #\A ca) (cons #\I z1)) (list (cons #\E z2) (cons #\T z3) (cons #\A ca) (cons #\I z4))
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A ca) (cons #\I z1)) (list (cons #\E z3) (cons #\T z4) (cons #\A ca) (cons #\I z2))
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A ca) (cons #\I z3)) (list (cons #\E z1) (cons #\T z2) (cons #\A ca) (cons #\I z4))

                                       (list (cons #\E z4) (cons #\T z3) (cons #\A ca) (cons #\I z1)) (list (cons #\E z4) (cons #\T z3) (cons #\A ca) (cons #\I z2))
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A ca) (cons #\I z1)) (list (cons #\E z3) (cons #\T z2) (cons #\A ca) (cons #\I z4))
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A ca) (cons #\I z3)) (list (cons #\E z2) (cons #\T z1) (cons #\A ca) (cons #\I z4))

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A ca) (cons #\I z2)) (list (cons #\E z3) (cons #\T z1) (cons #\A ca) (cons #\I z4))
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A ca) (cons #\I z1)) (list (cons #\E z4) (cons #\T z2) (cons #\A ca) (cons #\I z3))
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A ca) (cons #\I z2)) (list (cons #\E z4) (cons #\T z1) (cons #\A ca) (cons #\I z3))

                                       (list (cons #\E z1) (cons #\T z4) (cons #\A z2) (cons #\I ca)) (list (cons #\E z1) (cons #\T z4) (cons #\A z3) (cons #\I ca))
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A z2) (cons #\I ca)) (list (cons #\E z1) (cons #\T z3) (cons #\A z4) (cons #\I ca))
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A z1) (cons #\I ca)) (list (cons #\E z2) (cons #\T z4) (cons #\A z3) (cons #\I ca))

                                       (list (cons #\E z2) (cons #\T z3) (cons #\A z1) (cons #\I ca)) (list (cons #\E z2) (cons #\T z3) (cons #\A z4) (cons #\I ca))
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A z1) (cons #\I ca)) (list (cons #\E z3) (cons #\T z4) (cons #\A z2) (cons #\I ca))
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A z3) (cons #\I ca)) (list (cons #\E z1) (cons #\T z2) (cons #\A z4) (cons #\I ca))

                                       (list (cons #\E z4) (cons #\T z3) (cons #\A z1) (cons #\I ca)) (list (cons #\E z4) (cons #\T z3) (cons #\A z2) (cons #\I ca))
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A z1) (cons #\I ca)) (list (cons #\E z3) (cons #\T z2) (cons #\A z4) (cons #\I ca))
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A z3) (cons #\I ca)) (list (cons #\E z2) (cons #\T z1) (cons #\A z4) (cons #\I ca))

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A z2) (cons #\I ca)) (list (cons #\E z3) (cons #\T z1) (cons #\A z4) (cons #\I ca))
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A z1) (cons #\I ca)) (list (cons #\E z4) (cons #\T z2) (cons #\A z3) (cons #\I ca))
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A z2) (cons #\I ca)) (list (cons #\E z4) (cons #\T z1) (cons #\A z3) (cons #\I ca))))
                                 (let ([c (car l1)])
                               (list (list (cons #\E lr1) (cons #\T lr5) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr1) (cons #\T lr5) (cons #\A c) (cons #\I lr3))
                                     (list (cons #\E lr1) (cons #\T lr5) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr1) (cons #\T lr4) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr2) (cons #\T lr5) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr2) (cons #\T lr5) (cons #\A c) (cons #\I lr3))
                                     (list (cons #\E lr2) (cons #\T lr5) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr2) (cons #\T lr4) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr1) (cons #\T lr3) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr1) (cons #\T lr3) (cons #\A c) (cons #\I lr4))
                                     (list (cons #\E lr1) (cons #\T lr3) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A c) (cons #\I lr3))
                                     (list (cons #\E lr1) (cons #\T lr2) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr2) (cons #\T lr3) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A c) (cons #\I lr4))
                                     (list (cons #\E lr2) (cons #\T lr3) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr3) (cons #\T lr5) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A c) (cons #\I lr4))

                                     (list (cons #\E lr3) (cons #\T lr4) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr3) (cons #\T lr4) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr3) (cons #\T lr4) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr4) (cons #\T lr5) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A c) (cons #\I lr3))

                                     (list (cons #\E lr5) (cons #\T lr4) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr5) (cons #\T lr4) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr5) (cons #\T lr4) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr4) (cons #\T lr3) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr5) (cons #\T lr3) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr5) (cons #\T lr3) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr5) (cons #\T lr3) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr3) (cons #\T lr2) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr4) (cons #\T lr2) (cons #\A c) (cons #\I lr1)) (list (cons #\E lr4) (cons #\T lr2) (cons #\A c) (cons #\I lr3))
                                     (list (cons #\E lr4) (cons #\T lr2) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A c) (cons #\I lr1))
                                     (list (cons #\E lr5) (cons #\T lr2) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A c) (cons #\I lr4))

                                     (list (cons #\E lr2) (cons #\T lr1) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr2) (cons #\T lr1) (cons #\A c) (cons #\I lr4))
                                     (list (cons #\E lr2) (cons #\T lr1) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr3) (cons #\T lr1) (cons #\A c) (cons #\I lr4)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A c) (cons #\I lr5))

                                     (list (cons #\E lr4) (cons #\T lr1) (cons #\A c) (cons #\I lr2)) (list (cons #\E lr4) (cons #\T lr1) (cons #\A c) (cons #\I lr3))
                                     (list (cons #\E lr4) (cons #\T lr1) (cons #\A c) (cons #\I lr5)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A c) (cons #\I lr2))
                                     (list (cons #\E lr5) (cons #\T lr1) (cons #\A c) (cons #\I lr3)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A c) (cons #\I lr4)) 

                                     (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr3) (cons #\I c))
                                     (list (cons #\E lr1) (cons #\T lr5) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A lr5) (cons #\I c))
                                     
                                     (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr3) (cons #\I c))
                                     (list (cons #\E lr2) (cons #\T lr5) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A lr5) (cons #\I c))

                                     (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr4) (cons #\I c))
                                     (list (cons #\E lr1) (cons #\T lr3) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr3) (cons #\I c))
                                     (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr1) (cons #\T lr2) (cons #\A lr5) (cons #\I c))

                                     (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr4) (cons #\I c))
                                     (list (cons #\E lr2) (cons #\T lr3) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A lr4) (cons #\I c)) 

                                     (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr3) (cons #\T lr4) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A lr3) (cons #\I c))

                                     (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr5) (cons #\T lr4) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A lr5) (cons #\I c))

                                     (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr5) (cons #\T lr3) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A lr5) (cons #\I c))

                                     (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr1) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr3) (cons #\I c))
                                     (list (cons #\E lr4) (cons #\T lr2) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr1) (cons #\I c))
                                     (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A lr4) (cons #\I c))

                                     (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr4) (cons #\I c))
                                     (list (cons #\E lr2) (cons #\T lr1) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr4) (cons #\I c)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A lr5) (cons #\I c))

                                     (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr2) (cons #\I c)) (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr3) (cons #\I c))
                                     (list (cons #\E lr4) (cons #\T lr1) (cons #\A lr5) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr2) (cons #\I c))
                                     (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr3) (cons #\I c)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A lr4) (cons #\I c)))))]
          [(= (length l1) 2) (let* ([ca (car l1)]
                                   [cv (cadr l1)]
                                   [contca (contain? ca l)]
                                   [contcv (contain? cv l)])
                             (cond [(and contca contcv)
                                    (let* ([rem (remove cv (remove ca l '()) '())]
                                           [s1 (list-ref rem 0)]
                                           [s2 (list-ref rem 1)]
                                           [s3 (list-ref rem 2)])
                             (list (list (cons #\E s1) (cons #\T s3) (cons #\A ca) (cons #\I cv)) (list (cons #\E s1) (cons #\T s2) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E s2) (cons #\T s3) (cons #\A ca) (cons #\I cv)) (list (cons #\E s3) (cons #\T s2) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E s2) (cons #\T s1) (cons #\A ca) (cons #\I cv)) (list (cons #\E s3) (cons #\T s1) (cons #\A ca) (cons #\I cv))

                                   (list (cons #\E s1) (cons #\T s3) (cons #\A cv) (cons #\I ca)) (list (cons #\E s1) (cons #\T s2) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E s2) (cons #\T s3) (cons #\A cv) (cons #\I ca)) (list (cons #\E s3) (cons #\T s2) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E s2) (cons #\T s1) (cons #\A cv) (cons #\I ca)) (list (cons #\E s3) (cons #\T s1) (cons #\A cv) (cons #\I ca))))]
                                   [(and contca (not contcv))
                                    (let* ([rem1 (remove ca l '())]
                                          [z1 (list-ref rem1 0)]
                                          [z2 (list-ref rem1 1)]
                                          [z3 (list-ref rem1 2)]
                                          [z4 (list-ref rem1 3)])
                                (list  (list (cons #\E z1) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       
                                       (list (cons #\E z2) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A ca) (cons #\I cv))
                                       
                                       (list (cons #\E z4) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 

                                       (list (cons #\E z1) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z2) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z4) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A cv) (cons #\I ca)) ))]
                                   [(and (not contca)  contcv)
                                    (let* ([rem1 (remove cv l '())]
                                          [z1 (list-ref rem1 0)]
                                          [z2 (list-ref rem1 1)]
                                          [z3 (list-ref rem1 2)]
                                          [z4 (list-ref rem1 3)])
                                (list  (list (cons #\E z1) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       
                                       (list (cons #\E z2) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A ca) (cons #\I cv))
                                       
                                       (list (cons #\E z4) (cons #\T z3) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A ca) (cons #\I cv)) 
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A ca) (cons #\I cv)) 

                                       (list (cons #\E z1) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z1) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z2) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z2) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z3) (cons #\T z4) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z1) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z4) (cons #\T z3) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z3) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z2) (cons #\T z1) (cons #\A cv) (cons #\I ca)) 

                                       (list (cons #\E z3) (cons #\T z1) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z4) (cons #\T z2) (cons #\A cv) (cons #\I ca)) 
                                       (list (cons #\E z4) (cons #\T z1) (cons #\A cv) (cons #\I ca)) ))]
                                   [(and (not contca) (not contcv))
                             (list (list (cons #\E lr1) (cons #\T lr5) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E lr2) (cons #\T lr5) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A ca) (cons #\I cv))
                                   
                                   (list (cons #\E lr1) (cons #\T lr3) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E lr1) (cons #\T lr2) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A ca) (cons #\I cv))

                                   (list (cons #\E lr3) (cons #\T lr4) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E lr5) (cons #\T lr4) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A ca) (cons #\I cv))

                                   (list (cons #\E lr5) (cons #\T lr3) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E lr4) (cons #\T lr2) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A ca) (cons #\I cv))

                                   (list (cons #\E lr2) (cons #\T lr1) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A ca) (cons #\I cv))
                                   (list (cons #\E lr4) (cons #\T lr1) (cons #\A ca) (cons #\I cv)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A ca) (cons #\I cv))

                                   (list (cons #\E lr1) (cons #\T lr5) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr1) (cons #\T lr4) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E lr2) (cons #\T lr5) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr2) (cons #\T lr4) (cons #\A cv) (cons #\I ca))
                                   
                                   (list (cons #\E lr1) (cons #\T lr3) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr2) (cons #\T lr3) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E lr1) (cons #\T lr2) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr3) (cons #\T lr5) (cons #\A cv) (cons #\I ca))

                                   (list (cons #\E lr3) (cons #\T lr4) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr4) (cons #\T lr5) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E lr5) (cons #\T lr4) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr4) (cons #\T lr3) (cons #\A cv) (cons #\I ca))

                                   (list (cons #\E lr5) (cons #\T lr3) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr3) (cons #\T lr2) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E lr4) (cons #\T lr2) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr5) (cons #\T lr2) (cons #\A cv) (cons #\I ca))

                                   (list (cons #\E lr2) (cons #\T lr1) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr3) (cons #\T lr1) (cons #\A cv) (cons #\I ca))
                                   (list (cons #\E lr4) (cons #\T lr1) (cons #\A cv) (cons #\I ca)) (list (cons #\E lr5) (cons #\T lr1) (cons #\A cv) (cons #\I ca)))]))])))

(define (trithe key)
 (trio (string->list (car (stats:cipher-trigrams utils:cipher-word-list))) (string->list (car (stats:cipher-bigrams utils:cipher-word-list)))))
(define (trio l1 l2)
  (let ([ccar (car l1)]
       [ccadr (cadr l1)]
       [ccaddr (caddr l1)])
    (if (and (char=? ccar (car l2)) (char=? ccaddr (cadr l2))) (list (list (cons #\T ccar) (cons #\H ccadr) (cons #\E ccaddr))) '())))
                                  

                                   



                                     

                                    
                                       
                                       
                                
                                
                                
                                  

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai trithe))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))
