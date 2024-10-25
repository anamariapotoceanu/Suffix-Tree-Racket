#lang racket
(require "suffix-tree.rkt")
(require (lib "trace.ss"))

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
(define (longest-common-prefix w1 w2)
  (longest-help w1 w2 '()))
(define (longest-help w1 w2 acc)
    (cond
      ((null? w1) (append (list (reverse acc)) (list w1) (list w2)))
      ((null? w2) (append  (list (reverse acc)) (list w1) (list w2)))
      ((equal? (car w1) (car w2)) (longest-help (cdr w1) (cdr w2) (cons (car w1) acc)))
      (else (append (list (reverse acc)) (list w1) (list w2)))))



; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.


; consideram prefixul initial primul cuvant din lista
(define (longest-common-prefix-of-list words)
  (prefix-helper (car words) words))


; acesta functie intoarce doar prefixul comun dintre doua cuvinte
(define (longest-common-prefix-first words w1)
  (cond
    ((null? words) w1)
    ; vom calcula prefixul comun dintre urmatorul cuvant din lista si prefix
    ((car (longest-common-prefix (car words) w1)))))

; parcurgem toata lista si calculam prefixul comun
(define (prefix-helper pref words)
  (cond
     ((null? words) pref)
     ((prefix-helper (longest-common-prefix-first (cdr words) pref) (cdr words))))) 



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

(define (match-pattern-with-label st pattern)
  (match-helper st pattern))

(define (match-helper st pattern)
  (define branch (get-ch-branch st (car pattern)))

(cond
  ((null? st) #f)
  ; get-ch-branch va intoarce ramura corespunzatoare daca exista, altfel intoarce false
  ; daca ramura nu exista, intoarcem lista (false, lista vidă), deoarece sablonul nu exista in text
  ((not (get-ch-branch st (car pattern))) (list #f '()))
   ; extrag eticheta ramurei gasite si fac verificarea daca eticheta este egala cu sablonul primit
  ((equal? pattern (car (longest-common-prefix (get-branch-label branch) pattern)))
    #t)
  ; (caar st)-> extrage eticheta
  ; daca avem eticheta mai mare decat sablonul, sablonul este inclus in eticheta, dar nu se potrivesc pana la final
  ; calculam prefixul comun, daca exista
  ; vom returna (false, prefixul comun dintre cele doua)
  ((and (not (null? (car (longest-common-prefix (caar st) pattern)))) (> (length (caar st)) (length (car (longest-common-prefix (caar st) pattern)))))
      (list #f (car (longest-common-prefix (caar st) pattern))))
  ; cazul cand nu exista ramura 
  ((not branch) (list #f '()))

  ; verific daca sablonul contine eticheta
  ; daca prefixul comun este chiar eticheta, atunci eticheta este conținută integral în șablon (in pattern)
  ; (car branch) va fi eticheta
  ; returnez o lista formata din eticheta, care este si prefixul comun, noul sablon: ce ramane de verificat din pattern si subarborele
  ((equal? (car (longest-common-prefix (get-branch-label branch) pattern)) (get-branch-label branch))
     (append (list (car (longest-common-prefix (get-branch-label branch) pattern)))
           (list (car (cdr (cdr (longest-common-prefix (get-branch-label branch) pattern)))))
           (list (get-branch-subtree branch))))
           
  (else (match-helper (other-branches st) pattern))))




; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.


(define (st-has-pattern? st pattern)
  (cond
    ; daca rezultatul intors de functia anterioara nu este o lista, suntem in primul caz: true
    ((not (list? (match-pattern-with-label st pattern))) #t)
    ((list? (match-pattern-with-label st pattern))
     ; daca avem lista si aceasta are doua elemente, suntem in cazul (false, cel mai lung prefix comun între etichetă și șablon)
     ; se returneaza false
     ; daca lista are trei elemente, suntem in cazul (etichetă, șablon nou, subarbore)
     ; se continua cautarea in subarbore in functie de noul sablon, cautarea o vom face recursiv in functie de rezultatele returnate
     ; primul argument va fi subarborele, iar al doilea argument va fi noul sablon
     (if (= (length (match-pattern-with-label st pattern)) 2) #f
         (if (= (length (match-pattern-with-label st pattern)) 3)
              (st-has-pattern? (caddr (match-pattern-with-label st pattern)) (cadr (match-pattern-with-label st pattern))) 
             #t)))))


             
            
     
        
         
        

  
