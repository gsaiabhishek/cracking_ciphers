#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
 (dc1 (map (lambda (z) (utils:decrypt key z)) utils:cipher-word-list) key utils:dictionary))
(define (dc1 l1 key l)
  (cond [(null? l1) key]
        [else (let* ([count (count (car l1) l 0 '())]
                    [carcount (car count)])
                (cond [(or (allcapitals? (string->list (car l1))) (= 2 carcount)) (dc1 (cdr l1) key l)]
                      [(= 0 carcount) #f]
                      [else (let ([cdrcount (cdr count)])
                               (if (utils:is-monoalphabetic? cdrcount key)
                                             (dictionary-closure (utils:add-substitution cdrcount key)) #f))]))]))

(define (allcapitals? l)
  (cond [(null? l) #t]
        [(char-lower-case? (car l)) #f]
        [else (allcapitals? (cdr l))]))
(define (count s l c l1)
 (cond [(= c 2) (cons 2 2)]
        [(and (= c 1) (null? l)) (cons 1 l1)]
        [(and (= c 0) (null? l)) (cons 0 0)]
        [else (let* ([stle (string-length s)]
                    [carl (car l)]
                    [cdrl (cdr l)])
                (if  (= stle (string-length carl)) (let* ([sl (string->list s)]
                                                         [carll (string->list carl)]
                                                         [match (match sl carll '())])
                                                    (if (car match) (count s cdrl (+ c 1) (cdr match))
                                                        (count s cdrl c l1)))
                    (count s cdrl c l1)))]))

(define (match l l1 l2)
  (cond [(null? l) (cons #t l2)]
        [else (let ([carl (car l)]
       [carl1 (car l1)]
       [cdrl (cdr l)]
       [cdrl1 (cdr l1)])
                (cond [(char-lower-case? carl) (match cdrl cdrl1 (cons (cons carl1 carl) l2))] 
        [(equal? carl carl1) (match cdrl cdrl1 l2)]
        [else (cons #f '())]))]))