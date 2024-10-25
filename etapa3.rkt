#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")
(require (lib "trace.ss"))

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
  (let ((st (text->ast text)))
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

; aceasta functie returneaza sirul din pattern care se gaseste in arbore
(define (match-pattern st pattern)
  (let search ((st st) (pattern pattern))
    (let ((match-pattern-label (match-pattern-with-label st pattern))) ; obtin potrivirea patternului in arbore
      (cond
        ((not (list? match-pattern-label)) pattern) ; cazul cand eticheta si sablonul se potrivesc integral, returnam direct sablonul
        ; avem doua cazuri pentru lista: (false, cel mai lung prefix comun între etichetă și șablon) si (etichetă, șablon nou, subarbore)
        ; in primul caz, potrivirea va fi doar cel mai lung prefix
        ; in cel de-al doilea caz, trebuie sa continuam cautarea in subarbore pana cand gasim intreaga potrivire
        ((list? match-pattern-label)
         
         (if (= (length match-pattern-label) 2)
             (let ((longest-common-prefix (car (cdr match-pattern-label))))
               longest-common-prefix)
             
             (if (= (length match-pattern-label) 3)
                 (let ((label (car match-pattern-label)) ; eticheta care s-a potrivit va fi prima in lista de trei elemente
                       (pattern-for-search (car (cdr match-pattern-label))) ; continuam cautarea in subarbore in functie de noul sablon care trebuie verificat
                       (subtree (car (cdr (cdr match-pattern-label))))) ; subarborele in care vom continua cautarea, va fi ultimul element din lista
                   (append label (search subtree pattern-for-search)))
                 pattern)))))))
             
       
(define (longest-common-substring text1 text2)
  (let ((st (text->ast text1))
        (suffixes (get-suffixes text2)))
    
    (let find-substring ((suffixes suffixes) (longest-substring '()))
      
      (cond
        ((null? suffixes) longest-substring) ; daca am terminat de verificat toate sufixele, returnez cel mai lung subsir
        
        ((let ((suffix (car suffixes))
               (rest-suffixes (cdr suffixes)))
           (let ((match-suffix (match-pattern st suffix)) ; pentru fiecare sufix in parte, gasesc potrivirea lui in subarbore
                 (length-longest (length longest-substring))) ; lungimea celui mai lung subsir
             ; in cazul in care lungimea potrivirii este mai mare decat lungimea celui mai lung subsir, voi continua cautarea cu noul subsir
             (find-substring rest-suffixes (if (> (length match-suffix) length-longest) match-suffix
                                               longest-substring)))))))))
  
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

              
(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    (let search-deep ((st st) (search-label '())) 
      (if (or (st-empty? st) (null? (other-branches st))) #f ; daca nu avem nod itern (un nod care sa aiba fii) returnam false
          
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
                      ((null? rest-branches) #f) ; daca nu mai am ramuri, ma opresc 
                      (else (search-branches rest-branches))))))))))) ; continui cautarea pentru celelalte ramuri, daca nu gasesc subsirul
    
                          
                    

