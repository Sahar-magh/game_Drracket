#lang racket

;;;;;;;;;;;;;;;;;;;;;;; Q1 ;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct factory (consomation production cout));;list-pair list-pair float

(struct chaine (enter fact-list));;pair list_de_factory

(struct arbre ([fact 	#:mutable] [fils	#:mutable]))

;;;;;;;;;;;;;;;;;;;;;;; AFFICHAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;prend une factory en entrée et l'affiche de la manière suivante
;; exemple : factory (("a" . 1) ("b" . 3) 4)
;; la fonction retourne
;; consommation : 1 de a
;; production : 3 de b
;; cout : 4
(define (aff-fact fact)
  (printf "consomation : " )
  (if (null? (factory-consomation fact))
      (printf "aucune consomation")
      (begin (write (cdr (factory-consomation fact)))
             (printf " de ")
             (printf (car (factory-consomation fact)))))
  (printf "\n")
  (printf "production : ")
  (write (cdr (factory-production fact)))
  (printf " de " )
  (printf (car (factory-production fact)))
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

;;affiche une liste avec i = 0 et n la taille de la liste
(define (aff-lst-fact lst i n)
  (if (null? lst)
      (printf "end")
      (begin (printf "factory ") (write (add1 i)) (printf "/") (write n) (printf " : \n") (aff-fact (car lst)) (printf "\n")
                           (aff-lst-fact (cdr lst) (add1 i) n))))


;;affiche une chaine
(define (aff-chaine c)
  (aff-enter (chaine-enter c))
  (aff-lst-fact (chaine-fact-list c) 0 (length (chaine-fact-list c))))

(define (tiret n)
  (if (= n 0)
      (printf "-")
      (begin (printf "-") (tiret (sub1 n)))))

(define (aff-tree n tree)
  (tiret n)
  (if (null? (arbre-fils tree))
      (begin (print (arbre-fact tree)) (printf "\n"))
      (begin (print (arbre-fact tree)) (printf ":\n") (map (curry aff-tree (add1 n)) (arbre-fils tree)))))

(define (aff-fact2 lst i n)
  (if (null? lst)
      (printf "end")
      (begin (printf "factory ") (write (add1 i)) (printf "/") (write n) (printf " : \n") (aff-fact (car lst)) (printf "\n")
                           (aff-fact2 (cdr lst) (add1 i) n))))


;;;;;;;;;;;;;;;;;;;;;;; Q2 : LE PARCER ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;mettre le chemin vers le fichier txt à convertir
(define src2-lv1 (file->lines "/net/i/cnis/projetS6/Schemeprojet/projets6-fact-4423/src2_lv1.txt"))

;; ces fonctions trient les colonnes est prend en compte seulement les colonnes 1 3 et 4
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


;; prend en parametre une liste de caracteres et une liste de factory initialement vide
;; convertie la liste de caractère en factory
(define (convertion lst-str lst-fact)
  (cond [(null? lst-str) lst-fact]
        [(string=? (substring (car lst-str) 0 1) "#") (convertion (cdr lst-str) lst-fact)]
        [else (flatten (convertion (cdr lst-str) (list  (traduction2 (car lst-str)) lst-fact)))]))

(define lst-fact-src2-lv1 (convertion src2-lv1 null))

;(aff-lst-fact lst-fact-src2-lv1 0 (length lst-fact-src2-lv1))


;;;;;;;;;;;;;;;;;;;;;;; Q3 ;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (factory-null? fact)
  (and (null? (factory-consomation fact))
       (null? (factory-production fact)))) 


;; verifie si la liste de factory est bien cohérente
;; ((("a" . 1) ("b" . 3) 4)(("b" . 3)("c" . 2) 5) renvoie true
;;((("a" . 1) ("b" . 3) 4)(("d" . 3)("c" . 2) 5) renvoie false
(define (test-coherance-rec? enter list)
  (cond [(null? list) #t]
        [(not (equal? enter (factory-consomation (car list)))) #f]
        [else (test-coherance-rec? (factory-production (car list)) (cdr list))]))

;;test si la chaine de production est bien def
(define (test-coherance? file)
  (cond [(factory-null? (car (chaine-fact-list file))) #t]
        [(not(equal? (chaine-enter file) (factory-consomation (car (chaine-fact-list file))))) #f]
        [else (test-coherance-rec? (chaine-enter file) (chaine-fact-list file))]))


#|Plusieur factory est list de factory pour les test
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
|#

;; retourne le cout d'une liste de factory 
(define (cost lst-fact)
  (letrec ([ aux (lambda (lst-fact gold)
                   (if (null? lst-fact)
                       gold
                       (aux (cdr lst-fact) (+ gold (factory-cout (car lst-fact))))))])
    (aux lst-fact 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Q4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;test si on peut ajouter la factory dans une liste de la chaine de production
;; et la rajoute si c'est possible
;; Elle prend en argument une factory, une liste de chaines et le gold
;; Elle retourne un booleen et la liste de chaine modifiée 
(define (add-end-rec factory chaines gold)
  (cond [(null? chaines) (list #f '())]
        [(and (equal? (car(factory-production (last (car chaines)))) (car(factory-consomation factory)))
           (>= (cdr (factory-production (last (car chaines)))) (cdr (factory-consomation factory)))
           (>= gold (factory-cout factory)))
       (list #t (cons (append (car chaines) (list factory))(cdr chaines)))]
        [(equal? (car (factory-consomation factory)) null) (append (list factory)(chaines))]
       [else (list (and #t (car (add-end-rec factory (cdr chaines) gold))) (cons (car chaines) (cdr (add-end-rec factory (cdr chaines) gold))))]))



;;;;;;;;;;;;;;;;;;;;;;REGLE 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cette fonction effectue le tour du jeu
;; elle modifie le gold et la chaine de production 

(define (tour-jeu-r1  gold achat chaines)
  (begin (- gold (factory-cout achat)) (add-end-rec achat chaines gold)))


;;;;;;;;;;;TEST;;;;;;;;;;;;;;
#|
(define gold 50)

(define fact1 (factory (cons "a" 2) (cons "b" 3) 55))
(define fact2 (factory null (cons "a" 2) 40))
(define fact4 (factory (cons "b" 3) (cons "g" 4) 39))


(define addfact (factory (cons "b" 3 ) (cons "c" 1) 5))
(define addfact1 (factory (cons "null" 0) (cons "d" 4) 6))
(define addfact2 (factory (cons "g" 4 ) (cons "c" 1) 5))
(define addfact3 (factory (cons "c" 1) (cons "d" 4) 1))

(define chaine9 (list(list fact2 fact1) (list fact2 fact1 fact4)))



(add-end-rec addfact chaine9 gold)

(define (affiche-rec chaine )
  (if (null? chaine)
      null
      (begin (aff-fact2 (car chaine) 0 3 ) (affiche-rec (cdr chaine) ))))


;; test pour les factory de market tant que ce n'est pas achetable
;;l est la liste retournée après l'appel de end-chaine-prod
;;retourne une factory
;(define (buy-fac-rec list-achetable l)  
 ; (if (add-end-rec (car list-achetable) l)
  ;    (car list-achetable)
   ;   (buy-fac-rec (cdr list-achetable l))))


;on met les factories achetables du market dans liste-achetable en se basant uniquement sur le gold
;; on stocke dans liste-achetable la liste des factory qu'on peut acheter du market
;(define (factories-achetables market gold liste-achetable)  
 ; (if (>= gold (car market))
  ;    (cons liste-achetable (car market))
   ;   (factories-achetables (cdr market) gold liste-achetable)))

(define (aff-chaine2 chaine)
  (cond [(null? chaine) (begin (printf "WTF")(printf "\n"))]
        [(null? (cdr chaine)) (begin (aff-fact (car chaine))(printf "La chaine est morte")(printf"\n")(printf "\n"))]
        [else (begin (aff-fact (car chaine))
                     (aff-chaine2 (cdr chaine)))]))

(define (aff-chaines chaines )
 (if (null? chaines)
     (printf "fin")
     (begin (aff-chaine2 (car chaines)) (aff-chaines (cdr chaines)))))



;(aff-chaines chaine9)

(aff-chaines (car (cdr (add-end-rec addfact chaine9 gold))))

|#

;;;;;;;;;;;;;;;;;;;REGLE 2;;;;;;;;;;;;;;;;;;;;
;;;;Tour de jeu avec la relge 2

(define (tour-jeu-r2b gold market chaines)
  (if (null? market)
      chaines
      (if (car(add-end-rec (car market) chaines gold))
         (tour-jeu-r2b (- gold (factory-cout (car market))) (cdr market) (cadr (add-end-rec (car market) chaines (- gold (factory-cout (car market))) )))
         (tour-jeu-r2b gold (cdr market) chaines))))



;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;;;
#|
(tour-jeu-r1 gold addfact chaine9)
(define market1 (list addfact addfact3))
(tour-jeu-r2b 50 market1 chaine9)
|#



;;;;;;;;;;;;;;;;;;;;;;; Q5 ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; renvoie la list des factory qui produisent des gold
(define (extract-factg lst res) 
  (cond [(null? lst) res]
        [(not (string=? "Gold" (caar (factory-production (car lst))))) (extract-factg (cdr lst) res)]
        [else (flatten (extract-factg (cdr lst) (list res (car lst))))]))

;;(aff-lst-fact (extract-factg lst-fact-src2-lv1 null) 0 (length (extract-factg lst-fact-src2-lv1 null)))
  
;; enlève tous les elt de l2 presant dans l1
(define (pop l1 l2) 
  (remove* l2 l1 equal?))

;(aff-lst-fact (pop lst-fact-src2-lv1 (extract-factg lst-fact-src2-lv1 null)) 0 10)

;; indique si la racine de tree ne prend rien en consomation
(define (racine-null? tree)
  (null? (factory-consomation (arbre-fact tree))))

;;Est ce que fact produit bien "prod" ?
(define (produit? prod fact)
  (equal? prod (factory-production fact)))


;;Est ce qu'on peut rajouter quelque chose ?
(define (rajout? tree lst-fact)
  (letrec ([ aux (lambda (node)
                   (if (null? (arbre-fils node))                       
                       (> (length (filter (curry produit? (factory-consomation (arbre-fact node))) lst-fact)) 0)
                       (ormap aux (arbre-fils node))))])
    (aux tree)))

(define (leaf? node)
  (null? (arbre-fils node)))
  
;;qu'est ce qu'on peut rajouter ?
(define (rajouter tree lst-fact)
  (letrec ([ aux (lambda (res node)
                   (cond [(leaf? node) (list res (filter (curry produit? (factory-consomation (arbre-fact node))) lst-fact))]
                         [ else (map (curry aux res) (arbre-fils node))]))])
    (flatten (aux null tree))))


#|
(define fracine (factory (cons "a" 1)  (cons "gold" 1) 5))
(define fils1 (arbre (factory (cons "b" 1) (cons "a" 1) 5) null))
(define fils2 (arbre (factory (cons "c" 1)  (cons "a" 1) 5) null))
(define fils3 (arbre (factory (cons "d" 1) (cons "a" 1) 5) null))
(define frajout (factory null (cons "b" 1) 5))
(define frajout2 (factory null (cons "f" 1) 5))

(define lst (list fils1 fils2 fils3))
(define lst-rajout (list frajout frajout2))
(define tree-test (arbre fracine lst))

;(rajout? tree-test lst-rajout)
;(rajouter tree-test lst-rajout)

;(set-arbre-fils! tree-test (list (arbre-fils tree-test) fils1))

|#

(define (trans-fact-node fact)
  (arbre fact null))

(define (ajout tree lst-fact)
  (letrec ([ aux (lambda (node)
                   (cond [(leaf? node)
                          (set-arbre-fils!
                           node   
                           (map trans-fact-node (filter (curry produit? (factory-consomation (arbre-fact node))) lst-fact)))]
                         [ else (map aux (arbre-fils node))]))]) 
    (aux tree))
  tree)

#|
(aff-tree 0 tree-test)

(aff-tree 0 (ajout tree-test lst-rajout))

;(aff-tree 0 tree-test)
|#




(define fzero (factory (cons "a" 1) (cons "gold" 10) 2))
(define fzero2 (factory (cons "a" 1) (cons "gold" 10) 2))
(define f1 (factory (cons "b" 1) (cons "a" 1) 1))
(define f2 (factory (cons "c" 1) (cons "a" 1) 288))
(define f3 (factory (cons "d" 1) (cons "b" 1) 25))
(define f4 (factory (cons "e" 1) (cons "b" 1) 25))
(define f5 (factory (cons "f" 1) (cons "c" 1) 28))
(define f6 (factory (cons "g" 1) (cons "c" 1) 2212222222))
(define f7 (factory (cons "z" 1) (cons "y" 1) 0))
(define f8 (factory (cons "x" 1) (cons "t" 1) 12))

(define lst (list f1 f2 f3 f4 f5 f6 f7 f8))

;; creer un arbre à partir d'une factg est d'une lst-fact
(define (create-tree factg lst-fact) 
  (letrec ([ aux (lambda (tree lst-fact)
                   (let ([enlever (rajouter tree lst-fact)])
                   (if (rajout? tree lst-fact)
                       (aux (ajout tree lst-fact) (pop lst-fact enlever))
                       tree)))])
    (aux (arbre factg null) lst-fact)))
  

;;(aff-tree 0 (create-tree fzero lst))


(define listg (list fzero fzero2))

;; creer une list d'arbre à partir d'une lst-factg et d'une list-fact
(define (generation gold-lst lst-factory res) 
            (if (null? gold-lst)
                (flatten res)
                (generation (cdr gold-lst) lst-factory (list res (create-tree (car gold-lst) lst-factory)))))


;;(aff-tree 0 (car (generation listg lst null)))
;;(aff-tree 0 (cadr (generation listg lst null)))

#|
(define (ctr-forest lst-fac n g) ;; renvoit la chaine la plus rentable que l'on peut produire à partir de lst-fact en n tour avec g gold
  (let ([gold-list (extract-factg lst-fac null)])
    (best-chaine (suppress-useless (genaration gold-list (pop lst-fac gold-list)) null) n g )))
|#

;;calcule le cout du chemin
(define (prix road)
  (letrec ([ aux (lambda (road value)
                   (if (null? road)
                       value
                       (aux (cdr road) (+ value (factory-cout (car road))))))])
    (aux road 0)))

;; retourne le chemin qui a le cout le plus faible
(define (compare road1 road2)
  (if (> (prix road1) (prix road2))
      road2
      road1))


;;retourne le meilleur chemin d'un arbre
;;prend en argument un arbre et res une liste initialement vide
(define (best-road-tree tree res)
  (letrec ([current-res res]
           [aux ( lambda (road node)
                   (if (null? (arbre-fils node))
                       (set! res (compare  ((flatten current-res) (flatten (list road (arbre-fact node))))))
                       (map (curry aux (list road (arbre-fact node))) (arbre-fils node))))])
    (aux '() tree)))
                       
(define (first-road node road)
  (if (null? (arbre-fils node))
      (flatten (list road (arbre-fact node)))
      (first-road (car (arbre-fils node)) (list road (arbre-fact (car (arbre-fils node)))))))

;;(aff-tree 0 (create-tree fzero lst))
;;(first-road (create-tree fzero lst) '())
(define road-test (create-tree fzero lst))

;(compare (first-road road-test '()) (flatten (list f5 (first-road road-test '()))))

;;(best-road-tree road-test (first-road road-test '())) ;;TODO le debug

;;retourne le meilleur chemin d'une foret
(define (best-road forest res)
  (if (null? forest)
      res
      (best-road (cdr forest) (best-road-tree (car forest) res))))

