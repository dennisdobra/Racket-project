#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

;; LONGEST-COMMON-PREFIX - nemodificata
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


;; LONGEST-COMMON-PREFIX-OF-COLLECTION - modificata

; fct care afiseaza doar prefixul comun intre 2 cuv
(define (common-prefix-word w1 w2)
  (if (or (null? w1) (null? w2) (not (char=? (car w1) (car w2))))
      '()
      (cons (car w1) (common-prefix-word (cdr w1) (cdr w2)))))

; fct care calc prefixul comun tuturor cuv din lista words
(define (common-prefix-of-words words prefix common-prefix-word-func)
  (if (collection-null? words) ; am parcurs toate cuv si returnez prefixul curent
      prefix
      (let ((current-word (collection-car words)))
        (if (null? prefix)                ; Verificăm dacă prefixul este gol, caz în care nu mai avem un prefix comun
            '()
            (common-prefix-of-words (collection-cdr words) (common-prefix-word-func prefix current-word) common-prefix-word-func)))))

; functia data
(define (longest-common-prefix-of-collection words)
  (common-prefix-of-words (collection-cdr words) (collection-car words) common-prefix-word))


;; MATCH-PATTERN-WITH-LABEL - nemodificata
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


;; ST-HAS-PATTERN? - nemodificata
(define (st-has-pattern? st pattern)
  (let ((value (match-pattern-with-label st pattern)))
    (cond
      [(equal? value #t) #t] ; Caz 1
      [(equal? (car value) #f) #f] ; Caz 3
      [else (st-has-pattern? (first-branch (cddr value)) (cadr value))]))) ; Caz 2


;; GET-SUFFIXES - modificata
(define (get-suffixes text) ; text este o lista de caractere => ramane asa
  (if (null? text)
      null
      (collection-cons text (get-suffixes (cdr text))))) ; construiesc un flux de cuvinte


;; GET-CH-WORDS - modificata
(define (get-ch-words words ch)
  (collection-filter (lambda (x) (char=? (car x) ch)) words))


;; AST-FUNC - modificata
(define (ast-func suffixes)
  (cons (list (car (collection-car suffixes))) (collection-map cdr suffixes)))


;; CST-FUNC - modificata
(define (cst-func suffixes)
  (cons (longest-common-prefix-of-collection suffixes)
        (collection-map (lambda (x) (drop x (length (longest-common-prefix-of-collection suffixes)))) suffixes)))


;; SUFFIXES->ST - modificata

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-null? alphabet)
      ;; then:
      null
      ;; else:
      (let ((generate-symbol-branch (lambda (list-of-suffixes alfabet) ; generate-symbol-branch este o variabila locala definita prin functia anonima care construieste pe rand fiecare ramura din alfabet ca pereche intre: 
                                      (if (collection-null? list-of-suffixes)
                                          null
                                          (cons
                                           [car (labeling-func list-of-suffixes)] ; eticheta ramurii: (labeling-func suffixes) -> genereaza o pereche intre eticheta unei ramuri si subarborele de sub eticheta
                                           [collection-filter (lambda (x) (not (null? x)))
                                                              (suffixes->st labeling-func
                                                                            (collection-filter (lambda (x) (not (null? x)))
                                                                                               (cdr (labeling-func list-of-suffixes)))
                                                                            alfabet)]))))) ; si subarborele de sub eticheta
        (collection-filter
         (lambda (x) (not (null? x))) ; predicatul
         (collection-map ; aplica pentru fiecare litera din alfabet functia anonima lambda care primeste un parametru, symbol, pentru care se apeleaza functia de mai sus generate-symbol-branch, cu param (sufixele care incep cu litera symbol) si (alphabet)
          (lambda (symbol) (generate-symbol-branch (get-ch-words suffixes symbol) alphabet))
          alphabet))))) ; fiecare litera din lista alfabet


; nu uitați să convertiți alfabetul într-un flux
(define (list-to-stream alphabet)
  (if (null? alphabet)
      null
      (collection-cons (car alphabet) (list-to-stream (cdr alphabet)))))


;; TEXT->ST - am modificat alfabetul
(define text->st ; functie curry
  (lambda (text)
    (lambda (labeling-func)
      (let ([suffixes (get-suffixes (append text (list #\$)))] ; obținem sufixele textului la care adăugăm marcajul final $
            [alphabet (sort (remove-duplicates (append text (list #\$))) char<?)]) ; obținem alfabetul sortat asociat textului
        (suffixes->st labeling-func suffixes (list-to-stream alphabet))))))


;; TEXT->AST - nemodificata
(define text->ast
  (lambda (text)
    ((text->st text) ast-func)))


;; TEXT->CST - nemodificata
(define text->cst
  (lambda (text)
    ((text->st text) cst-func)))


;; SUBSTRING? - nemodificata

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ([st (text->ast text)])
    (st-has-pattern? st pattern)))



;; REPEATED-SUBSTRING-OF-GIVEN-LENGTH - modificata

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

; vf daca lungimea lui str > len
(define (check-length str len)
  (>= (length str) len))

(define (repeated-substring-of-given-length text len)
  (let* ([st (text->cst text)]) ; convertesc textul intr-un suffix-tree
    (let loop1 ([tree st] [lungime len]) ; parcurg st ul
      (let loop2 ([branches tree]) ; parcurg ramurile
        (cond
          [(collection-null? branches) #f] ; daca am terminat branch-urile => nu mai exista subsiruri care sa se repete
          [else
           (let* ([curr-br (collection-car branches)] ; primul branch din lista de branch-uri
                  [br-label (get-branch-label curr-br)] ; label-ul branch-ului
                  [br-subtree (get-branch-subtree curr-br)] ; subtree-ul asoc branch-ului
                  [other-branches (collection-cdr branches)]) ; celelalte branch-uri
             (cond
               [(st-empty? br-subtree) ; vf daca mai exista subarbore pt branch-ul curent
                (loop2 other-branches)] ; daca nu mai exista => trec la urmatoarea ramura
               [(check-length br-label lungime) ; vf daca lungimea etichetei > len =>
                (take br-label lungime)] ; => returnez subsirul de lungime len folosind take
               [else ; daca exista subarbore
                (let ([result (loop1 br-subtree (- lungime (length br-label)))]) ; caut recursiv in subarbore
                  (if result
                      (append br-label result) ; inseamna ca am gasit un subsir care se repeta => il returnez
                      (loop2 other-branches)))]))]))))) ; altfel, caut in celelalte branch-uri
        