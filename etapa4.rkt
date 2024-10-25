#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")
(require (lib "trace.ss"))
(require racket/stream)

(require racket/stream)


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

(define (longest-common-prefix w1 w2)
  (longest-help w1 w2 '()))
(define (longest-help w1 w2 acc)
  (cond
    ((collection-empty? w1) (append (list (reverse acc)) (list w1) (list w2)))
    ((collection-empty? w2) (append  (list (reverse acc)) (list w1) (list w2)))
    ((equal? (collection-first w1) (collection-first w2)) (longest-help (collection-rest w1) (collection-rest w2) (cons (collection-first w1) acc)))
    (else (append (list (reverse acc)) (list w1) (list w2)))))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (prefix-helper (collection-first words) words)) ; consideram prefixul initial primul cuvant din flux


; acesta functie intoarce doar prefixul comun dintre doua cuvinte
(define (longest-common-prefix-first words w1)
  (cond
    ((collection-empty? words) w1)
    ((car (longest-common-prefix (collection-first words) w1)))))

; parcurgem tot fluxul de cuvinte si calculam prefixul comun
(define (prefix-helper pref words)
  (cond
    ((collection-empty? words) pref)
    ((prefix-helper (longest-common-prefix-first (collection-rest words) pref) (collection-rest words))))) 



(define (match-pattern-with-label st pattern)
  (match-helper st pattern))

(define (match-helper st pattern)
  (define branch (get-ch-branch st (car pattern)))
  (cond
    ((collection-empty? st) #f)
    ; get-ch-branch va intoarce ramura corespunzatoare daca exista, altfel intoarce false
    ; daca ramura nu exista, intoarcem lista (false, lista vidă), deoarece sablonul nu exista in text
    ((not (get-ch-branch st (car pattern))) (list #f '()))
    ; extrag eticheta ramurei gasite si fac verificarea daca eticheta este egala cu sablonul primit
    
    ((equal? pattern (car (longest-common-prefix (get-branch-label branch) pattern)))
     #t)
    ; daca avem eticheta mai mare decat sablonul, sablonul este inclus in eticheta, dar nu se potrivesc pana la final
    ; calculam prefixul comun, daca exista
    ; vom returna (false, prefixul comun dintre cele doua)
    ((and (not (null? (car (longest-common-prefix (get-branch-label (collection-first st)) pattern)))) (> (length (get-branch-label (collection-first st)) ) (length (car (longest-common-prefix (get-branch-label (collection-first st)) pattern)))))
     (list #f (car (longest-common-prefix (get-branch-label (collection-first st)) pattern))))
    ; verific daca sablonul contine eticheta
    ; daca prefixul comun este chiar eticheta, atunci eticheta este conținută integral în șablon (in pattern)
    ; returnez o lista formata din eticheta, care este si prefixul comun, noul sablon: ce ramane de verificat din pattern si subarborele
    ((equal? (car (longest-common-prefix (get-branch-label branch) pattern)) (get-branch-label branch))
     (append (list (car (longest-common-prefix (get-branch-label branch) pattern)))
             (list (car (cdr (cdr (longest-common-prefix (get-branch-label branch) pattern)))))
             (list (get-branch-subtree branch))))
    (else (match-helper (other-branches st) pattern))))



(define (get-suffixes_copy text)
  (if (null? text) collection-empty
      (collection-cons text (get-suffixes (cdr text)))))

(define (get-ch-words_copy words ch)
  (collection-filter (lambda (x)
                       (and (not (null? x))
                            (char=? (car x) ch)))
                     words))
(define (get-ch-words words ch)
  (stream-filter (lambda (word)
                        (and (not (null? word)) 
                              (char=? (car word) ch))) words))


(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes)))
        (collection-map (lambda (x) (collection-rest x))
                        suffixes)))

(define (cst-func suffixes)
  (define prefix (longest-common-prefix-of-collection suffixes))
  (cons prefix
        (collection-foldr (lambda (x acc)
                            (collection-cons (car (cdr (cdr (longest-common-prefix prefix x)))) acc))
                          collection-empty
                          suffixes)))



(define (cst-func suffixes)
 (cons (longest-common-prefix-of-collection suffixes)
  (stream-map (lambda (suffix) 
         (caddr (longest-common-prefix (longest-common-prefix-of-collection suffixes) suffix)))
       suffixes)))
(define (get-suffixes text)
  (if (null? text)
      (stream)  ;; Returnează o listă goală dacă textul este gol.
      (stream-cons text  ;; Adaugă fluxul (sufixul curent) în lista de sufixuri.
            (get-suffixes (cdr text)))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)

; nu uitați să convertiți alfabetul într-un flux

(define (suffixes->st labeling-func suffixes alphabet)
  (let ((arbore
         (collection-map (lambda (ch) ; Pentru fiecare litera din alfabet vom crea o lista cu sufixele care incepe cu litera corespunzatoare
                           (define list-suffixes (get-ch-words suffixes ch))
                           (cond
                             ((collection-empty? list-suffixes) '())
                             ; Pentru fluxul de sufixe vom apela functia labeling-func
                             ; Parcurgem subarborele pe care il obtinem aplicand labeling-func pentru a crea arborele cerut
                             ; Adaugam mereu eticheta si continuam parcurgerea pe ceea ce obtinem dupa aplicarea functiei labeling-func
                             (else (let* ((result-labeling (labeling-func list-suffixes))
                                          (label-branch (car result-labeling))
                                          (rest-branch (cdr result-labeling))
                                          (recursiv (suffixes->st labeling-func rest-branch alphabet)))
                                     (cons label-branch recursiv )))))
                 
                         alphabet)))
    (collection-filter (lambda (lista) (not (null? lista)))
                       arbore)))


(define (text->st labeling-func)
  (lambda (text)
    (define alphabet (alphabet-func text))
    (define suffixes (suffixes-func text))
  (suffixes->st labeling-func suffixes alphabet)))


(define (list->collection L)
  (if (null? L)
      collection-empty
      (collection-cons (car L) (list->collection (cdr L)))))

; Functie care creeaza alfabetul: adaugam #\$ pentru textul initial, eliminam duplicatele si sortam
(define (alphabet-func text)
  (list->collection (sort (remove-duplicates (cons #\$ text)) char<?)))

(define (suffixes-func text)
  (get-suffixes (append text (list #\$))))
          
(define text->ast
  (text->st ast-func))

(define text->cst
  (text->st cst-func))

(define (st-has-pattern? st pattern)
  (let ((match (match-pattern-with-label st pattern)))
    (cond
      ((not (list? match)) #t)
      ((list? match)
       (if (= (length match) 2) #f
           (if (= (length match) 3)
               (st-has-pattern? (caddr match) (cadr match)) 
               #t))))))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((st (text->cst text)))
    (st-has-pattern? st pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    (let search-deep ((st st) (search-label '())) 
      (if (or (st-empty? st) (collection-empty? (other-branches st))) #f ; daca nu avem nod itern (un nod care sa aiba fii) returnam false
          
          (let search-branches ((branches st))
            
            (let ((length-label (length search-label))) ; lungimea acumulata pentru eticheta
              
              (if (>= length-label len) ; daca reusesc sa gasesc subsirul, il returnez
                  (take search-label len) ; luam primele len caractere din sir
            
                  (let* ((current-branch (first-branch branches))
                         (current-label (get-branch-label current-branch))
                         (new-label (append search-label current-label)) ; la fiecare pas, voi tine minte in new-label eticheta curenta
                         (subtree (get-branch-subtree current-branch))
                         (rest-branches (other-branches branches))
                         (result-search (search-deep subtree new-label))) ; caut in subarbore in functie de noul label, in adancime
                  
                    (cond
                      ; daca rezultatul apelului este o lista, inseamna ca am gasit subsirul, deci il returnam
                      ; altfel, trebuie sa caut prin celelalte ramuri ale arborelui initial
                      ; daca nu mai am ramuri prin care sa caut, returnez false
                      ((list? result-search) result-search) ; daca reusesc sa gasesc subsirul de lungime len, il returnez 
                      ((collection-empty? rest-branches) #f) ; daca nu mai am ramuri, ma opresc 
                      (else (search-branches rest-branches))))))))))) ; continui cautarea pentru celelalte ramuri, daca nu gasesc subsirul
