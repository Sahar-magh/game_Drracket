#lang racket
#lang racket

;;;;;;;;;;;;;;;;;;;;;;; Q1 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct factory (consomation production cout));;list-pair list-pair float

(struct chaine (enter fact-list));;pair list_de_factory

(struct arbre (fact fils))

;;;;;;;;;;;;;;;;;;;;;;; AFFICHAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (aff-fact fact)
  (if (null? (factory-consomation fact))
       (printf "pas de consomation \n")
       (begin (printf "consomation : " )
        (write (string->number (cdar (factory-consomation fact))))
        (printf " de " )
        (printf (caar (factory-consomation fact)))
        (printf "\n")))      
  (printf "production : ")
  (write (string->number(cdar (factory-production fact))))
  (printf " de " )
  (printf (caar (factory-production fact)))
  (printf "\n")
  (printf "cout : ")
  (write (factory-cout fact) )
  (printf "\n"))

(define (aff-enter e)
  (printf "entrer : ")
  (write (cdr e))
  (printf " de ")
  (printf (car e))
  (printf "\n")
  (printf "\n"))

(define (aff-lst-fact lst i n)
  (if (null? lst)
      (printf "end")
      (begin (printf "factory ") (write (add1 i)) (printf "/") (write n) (printf " : \n") (aff-fact (car lst)) (printf "\n")
                           (aff-lst-fact (cdr lst) (add1 i) n))))

(define (aff-chaine c)
  (aff-enter (chaine-enter c))
  (aff-lst-fact (chaine-fact-list c) 0 (length (chaine-fact-list c))))
 

;;;;;;;;;;;;;;;;;;;;;;; Q3 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factory-null? fact)
  (and (null? (factory-consomation fact))
       (null? (factory-production fact)))) 

(define (test-coherance-rec? enter list)
  (cond [(null? list) #t]
        [(not (equal? enter (factory-consomation (car list)))) #f]
        [else (test-coherance-rec? (factory-production (car list)) (cdr list))]))

;;test si la chaine de production est bien def
(define (test-coherance? file)
  (cond [(factory-null? (car (chaine-fact-list file))) #t]
        [(not(equal? (chaine-enter file) (factory-consomation (car (chaine-fact-list file))))) #f]
        [else (test-coherance-rec? (chaine-enter file) (chaine-fact-list file))]))
        
(define f1 (factory (cons "a" 1) (cons "b" 1) 5))
(define f2 (factory (cons "b" 1) (cons "c" 1) 15))
(define f3 (factory (cons "c" 1) (cons "d" 1) 5))


(define fnull (factory null null 0))
;;(factory-null? fnull) -> true
;;(factory-null? f1) -> false

(define c1 (chaine (cons "a" 1) (list f1 f2 f3)))
(define cnull (chaine (cons "a" 1) (list fnull)))
(define c2 (chaine (cons "b" 1) (list f1 f2 f3)))
(define c3 (chaine (cons "a" 1) (list f1 f3 f2)))
;;(chaine-enter c1)
;;(chaine-fact-list c1)

;;(test-coherance? c1) -> true
;;(test-coherance? cnull) ;;-> true
;;(test-coherance? c2) -> false
;;(test-coherance? c3) -> false


(define (cost lst-fact)
  (letrec ([ aux (lambda (lst-fact gold)
                   (if (null? lst-fact)
                       gold
                       (aux (cdr lst-fact) (+ gold (factory-cout (car lst-fact))))))])
    (aux lst-fact 0)))

;;;;;;;;;;;;;;;;;;;;;;; Q2 ;;;;;;;;;;;;;;;;;;;;;;;;;;



;;(define src (file->list "/net/i/cnis/projetS6/Schemeprojet/projets6-fact-4423/src.txt"))        
(define src-lv1 (file->list "/net/i/cnis/projetS6/Schemeprojet/projets6-fact-4423/src_lv1.txt"))

;;(/ (length src-lv1) 6)
(define c-test (chaine (cons "Bread" 1) null))
;;(chaine-enter c-test)
;;(chaine-fact-list c-test)


;;Convertit une list en une chaine de carac !!!!!!! ATTENTION !!!!! ne fonctionne que pour des fatory de niveau 1
(define (trad s)
  (cond [(null? s) null]
        [(and (pair? s) (= (length s) 1) (not(number? (car s)))) (symbol->string (car s))]
        [(and (pair? s) (= (length s) 1) (number? (car s))) (car s)]
        [(number? s) s]
        [else (symbol->string s)]
        ))

(define (convert sl)
  (map trad sl))

(define (string->consomation s);;Marche aussi pour les productions
  (string-split s "="))


(define (trans-fact l)
  (factory (string->consomation (car l)) (string->consomation (cadr l)) (caddr l))) 

(define (traduction l)
  (trans-fact (convert l)))

;;Exemples (penser à decommenter src)
;;(list (car src) (caddr src) (car (cdr (cddddr src))))
;;(convert (list (car src) (caddr src) (car (cdr (cddddr src)))))
;;(trans-fact (convert (list (car src) (caddr src) (car (cdr (cddddr src))))))
;;(define test (trans-fact (convert (list (car src) (caddr src) (car (cdr (cddddr src)))))))
;;(factory-consomation test)
;;(factory-production test)
;;(factory-cout test)
;;(traduction (list (car src) (caddr src) (car (cdr (cddddr src)))))


(define (parcer src n file)
  (if (= 0 n)
      file
      (parcer (cdr (cdr (cddddr src))) (sub1 n)
              (chaine (chaine-enter file) (flatten (list (traduction (list (car src) (caddr src) (car (cdr (cddddr src))))) (chaine-fact-list file) ))))))


;;(define test-parcer (parcer src-lv1 (/ (length src-lv1) 6) c-test))
;;(aff-chaine test-parcer)

;;;;;;;;;;;;;;;;;;;;;;; Q2-v2 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define src2-lv1 (file->lines "/net/i/cnis/projetS6/Schemeprojet/projets6-fact-4423/src2_lv1.txt"))

(define (list-to-tree lst )
  (letrec ([ aux (lambda (lst res)
                   (if (null? lst)                    
                       res
                       (aux (cddr lst) (list (cons (car lst) (cadr lst)) res))))])
    (aux lst null)))
                   

(define (conso-trad s)
  (if (string=? "[]" s)
      null
      (list-to-tree (string-split (string-replace (substring s 1 (sub1 (string-length s))) "=" ",") "," ))))

(define (cout-trad s)
  (string->number s))
  
 

(define (traduction2 s)
  (factory (conso-trad (car (string-split s)))
           (conso-trad (caddr (string-split s)))
           (cout-trad (cadr (cddddr (string-split s))))))


;(factory-consomation (traduction2 (cadr src2-lv1)))
;(factory-production (traduction2 (cadr src2-lv1)))
;(factory-cout (traduction2 (cadr src2-lv1)))

(define (convertion lst-str lst-fact)
  (cond [(null? lst-str) lst-fact]
        [(string=? (substring (car lst-str) 0 1) "#") (convertion (cdr lst-str) lst-fact)]
        [else (flatten (convertion (cdr lst-str) (list  (traduction2 (car lst-str)) lst-fact)))]))

(define lst-fact-src2-lv1 (convertion src2-lv1 null))

;(aff-lst-fact lst-fact-src2-lv1 0 (length lst-fact-src2-lv1))

;;;;;;;;;;;;;;;;;;;;;;; Q5 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extract-factg lst res) ;; renvoie la list des factory qui produisent des gold
  (cond [(null? lst) res]
        [(not (string=? "Gold" (caar (factory-production (car lst))))) (extract-factg (cdr lst) res)]
        [else (flatten (extract-factg (cdr lst) (list res (car lst))))]))

;;(aff-lst-fact (extract-factg lst-fact-src2-lv1 null) 0 (length (extract-factg lst-fact-src2-lv1 null)))
  

(define (pop l1 l2) ;; enlève tous les elt de l2 presant dans l1
  (remove* l2 l1 equal?))

;(aff-lst-fact (pop lst-fact-src2-lv1 (extract-factg lst-fact-src2-lv1 null)) 0 10)

(define (racine-null? tree) ;; indique si la racine de tree ne prend rien en consomation
  (null? (factory-consomation (arbre-fact tree))))

(define (suppress-useless lst-tree) ;; supprime tous les arbres de lst-tree qui n'ont pas une consomation vide à la racine ;; TODO marche pas 'les vide sont des feuille)
  (filter racine-null? lst-tree))

#|

(define (rajout? tree lst-fact)
  (letrec ([ aux (lambda (tree)
                   (if (null? (arbre-fils tree))                       
                       (> (apply + (filter (arbre-fact) 1)
                       (ormap aux (arbre-fils tree))
                                
|#

#|
(define (create-tree factg lst-fact) ;; creer un arbre à partir d'une factg est d'une lst-fact
  (letrec ([ aux (lambda (tree lst-fact)
                   (if (rajout? tree lst-fact)
                       (aux ( (ajout tree lst-fact) (pop lst-fact (ajouter tree lst-fact)) ))
                      tree))])
    (aux (arbre (factg null)) lst-fact)))
  
|#

#|
(define (generation gold-lst lst-factory res) ;; creer une list d'arbre à partir d'une lst-factg et d'une list-fact
            (if (null? gold-lst)
                (flatten res)
               (generation (cdr gold-lst) lst-factory (list res (create-tree (car gold-lst) lst-factory))))) 

(define (ctr-forest lst-fac n g) ;; renvoit la chaine la plus rentable que l'on peut produire à partir de lst-fact en n tour avec g gold
  (let ([gold-list (extract-factg lst-fac null)])
    (best-chaine (suppress-useless (genaration gold-list (pop lst-fac gold-list)) null) n g )))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q6;;;;;;;;;;;;;;;;;;;;;;;;













