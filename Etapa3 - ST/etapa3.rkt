#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let ([st (text->cst text)])
    (st-has-pattern? st pattern)))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (continue-pattern-search st pattern)
  (let ([result (match-pattern-with-label st pattern)])
    (cond
      [(boolean? result) ; inseamna ca e #t => returnez pattern-ul
       pattern]
      [( = (length result) 2) ; inseamna ca returnez prefixul comun
       (cadr result)]
      [else ; inseamna ca inca pot gasi pattern-ul
       (let* ([subtree (caddr result)] ; obtin subarborele
              [new-pattern (cadr result)]) ; obtin noul sablon
         (append (car result) (continue-pattern-search subtree new-pattern)))]))) ; continui cautarea


(define (longest-common-substring text1 text2)
  (let ([st1 (text->cst text1)])
    (let loop ([suffixes (get-suffixes text2)]
               [longest-match null])
      (if (null? suffixes)
          longest-match
          (let* ([current-suffix (car suffixes)] ; iau primul sufix
                 [match-result (continue-pattern-search st1 current-suffix)]) ; aplic pe sufix match-pattern-with-label
            (if (>= (length longest-match) (length match-result))
                (loop (cdr suffixes) longest-match) ; inseamna ca nu s a gasit o potrivire mai buna
                (loop (cdr suffixes) match-result))))))) ; inseamna ca s a gasit o potrivire mai buna
       
; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

; vf daca lungimea lui str > len
(define (check-length str len)
  (>= (length str) len))

(define (repeated-substring-of-given-length text len)
  (let ([st (text->cst text)]) ; convertesc textul intr-un suffix-tree
    (let loop1 ([tree st] [lungime len]) ; parcurg st ul
      (let loop2 ([branches tree]) ; parcurg ramurile
        (cond
          [(null? branches) #f] ; daca am terminat branch-urile => nu mai exista subsiruri care sa se repete
          [else
           (let* ([curr-br (car branches)] ; primul branch din lista de branch-uri
                  [br-label (get-branch-label curr-br)] ; label-ul branch-ului
                  [br-subtree (get-branch-subtree curr-br)] ; subtree-ul asoc branch-ului
                  [other-branches (cdr branches)]) ; celelalte branch-uri
             (cond
               [(st-empty? br-subtree) ; vf daca mai exista subarbore pt branch-ul curent
                (loop2 other-branches)] ; daca nu mai exista => trec la urmatoarea ramura
               [(check-length br-label lungime) ; vf daca lungimea etichetei > len => daca da
                (take br-label lungime)] ; => returnez subsirul de lungime len folosind take
               [else ; daca exista subarbore
                (let ([result (loop1 br-subtree (- lungime (length br-label)))]) ; caut recursiv in subarbore
                  (if result
                      (append br-label result) ; inseamna ca am gasit un subsir care se repeta => il returnez
                      (loop2 other-branches)))]))]))))) ; altfel, caut in celelalte branch-uri
        