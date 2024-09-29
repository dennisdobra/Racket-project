#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix-helper prefix rest-w1 rest-w2)
  (cond
    [(or (null? rest-w1) (null? rest-w2))
     (list (reverse prefix) rest-w1 rest-w2)]
    [(equal? (car rest-w1) (car rest-w2))
     (longest-common-prefix-helper (cons (car rest-w1) prefix) (cdr rest-w1) (cdr rest-w2))] ; construiesc prefix in ordine inversa, dar il afisez in ordinea corecta mai sus
    [else
     (list (reverse prefix) rest-w1 rest-w2)])) ; inseamna ca am gasit prefixul comun => STOP
   
(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper '() w1 w2))

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

; functia asta imi afiseaza doar prefixul comun dintre
; 2 cuvinte, nu si restul cuvintelor => d-asta nu o pot
; folosi pe cea de mai sus
(define (common-prefix-word w1 w2)
  (if (or (null? w1) (null? w2) (not (char=? (car w1) (car w2))))
      '()
      (cons (car w1) (common-prefix-word (cdr w1) (cdr w2)))))

; calculeaza prefixul comun tuturor cuv din lista words
; prefix -> prefixul comun curent
; common-prefix-word-func: funcția care calculează prefixul comun între două cuvinte
(define (common-prefix words prefix common-prefix-word-func)
  (if (null? words) ; am parcurs toate cuv si returnez prefixul curent
      prefix
      (let ((current-word (car words)))
        (if (null? prefix)                ; Verificăm dacă prefixul este gol, caz în care nu mai avem un prefix comun
            '()
            (common-prefix (cdr words) (common-prefix-word-func prefix current-word) common-prefix-word-func)))))

; functia data
(define (longest-common-prefix-of-list words)
  (common-prefix (cdr words) (car words) common-prefix-word))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

#| (define (match-pattern-with-label st pattern)
     (cond
       [(st-empty? st) '(#f ())]
       [(equal? (car (longest-common-prefix (get-branch-label (first-branch st)) pattern)) pattern)
        #t] ; pattern continut in label
       [(equal? (car (longest-common-prefix (get-branch-label (first-branch st)) pattern)) (get-branch-label (first-branch st)))
        (list (get-branch-label (first-branch st)) (caddr (longest-common-prefix (get-branch-label (first-branch st)) pattern)) (get-branch-subtree (first-branch st)))]
       [(not (null? (car (longest-common-prefix (get-branch-label (first-branch st)) pattern))))
        (list #f (car (longest-common-prefix (get-branch-label (first-branch st)) pattern)))]
       [else
     (match-pattern-with-label (other-branches st) pattern)])) |#


(define (match-pattern-with-label st pattern)
  (let ([label (and (not (st-empty? st)) (get-branch-label (first-branch st)))]
        [subtree (and (not (st-empty? st)) (get-branch-subtree (first-branch st)))])
    (cond
      [(st-empty? st) '(#f ())]
      [(equal? (car (longest-common-prefix label pattern)) pattern)
        #t] ; pattern continut in label
      [(equal? (car (longest-common-prefix label pattern)) label)
       (list label (caddr (longest-common-prefix label pattern)) subtree)]
      [(not (null? (car (longest-common-prefix label pattern))))
       (list #f (car (longest-common-prefix label pattern)))]
      [else
       (match-pattern-with-label (other-branches st) pattern)])))



; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (let ((value (match-pattern-with-label st pattern)))
    (cond
      [(equal? value #t) #t] ; Caz 1
      [(equal? (car value) #f) #f] ; Caz 3
      [else (st-has-pattern? (first-branch (cddr value)) (cadr value))]))) ; Caz 2