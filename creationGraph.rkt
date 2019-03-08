#lang racket

(struct graphe (node_racine lstnode lst-adj));;
;;struct graph ( la chaine de production ) node_racine l'id de la racine (noeud) et lstnode la liste des ;
;noeuds et lst-adj la liste des noeuds adjacents  : il s'agit d'une liste de liste de noeuds :
;pour chaque noeud on associe une liste des noeud qui lui sont adjacents

(struct node (id factory)) ;; struct noeud node id : l'id du noeud et factory l'usine qui est dans le noeud 

(struct arc (cout id-deb id-end)) ; structure arc qui contient le cout pour acheter la factory d'arrivé id-deb-->id-end

(struct factory (consomation production cout));;list-pair list-pair float

(struct bench (lst-fact));;list de factory : bac de production 

(struct chain (lst-bench));; list de list de factory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;factory null
(define Fact9 (factory (list (cons "" 0)) (cons "b" 1) 10))
(define Fact0 (factory (list (cons "" 0)) (cons "a" 1) 5))
(define Fact1 (factory (list (cons "a" 1)) (cons "b" 1) 5))
(define Fact2 (factory (list (cons "a" 1)) (cons "c" 1) 5))
(define Fact3 (factory (list (cons "b" 1)) (cons "d" 1) 7))
(define Fact4 (factory (list (cons "c" 1) (cons "e" 1)) (cons "d" 1) 1))
(define Fact5 (factory (list (cons "d" 1)) (cons "gold" 10) 3))
(define Fact6 (factory (list (cons "d" 1)) (cons "gold" 15) 4))
(define Fact7 (factory (list (cons "a" 1) (cons "b" 1)) (cons "e" 1) 7))
(define Fact8 (factory (list (cons "e" 1)) (cons "gold" 20) 20))
  
(define factlst (list Fact9 Fact0 Fact1 Fact2 Fact3 Fact4 Fact5 Fact6 Fact7 Fact8 ))

(define FactNull (factory (cons "" 0) (cons "" 0) 0))
;;noeud null
(define NodeNull (node -1 FactNull))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Est ce qu'au moins une des consomations est egal à la production
(define (match-conso? conso produ)
  (ormap (curry equal? produ) conso))

;(match-conso? (factory-consomation Fact3) (factory-production Fact9))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;trouver un consommateur du produit prod
(define (find-consomateur graph produ)
  (letrec ([lst (graphe-lstnode graph)]
           [aux (lambda (lst produ res)
                   (cond [(null? lst) res]
                         [(not (match-conso? (factory-consomation (node-factory (car lst))) produ)) (aux (cdr lst) produ res)]
                         [else (aux (cdr lst) produ (cons (node-id (car lst)) res))]))])
    (aux lst produ null)))

  
;;trouver un producteur du produit cons
(define (find-producteur graph conso) 
  (letrec ([lst (graphe-lstnode graph)]
           [aux (lambda (lst conso res)
                  (cond [(null? lst) res]
                        [(not (match-conso? conso (factory-production (node-factory (car lst))))) (aux (cdr lst) conso res)] 
                        [else (aux (cdr lst) conso (cons (node-id (car lst)) res))]))])
    (aux lst conso null)))

;;ajoute des arc sortant
(define (ajout-sortant sortant adj id)
  (letrec ([aux (lambda (sortant adj id res)
                  (if (= id 0)
                      (append res (cons (append (car adj) sortant) null) (cdr adj)) 
                      (aux sortant (cdr adj) (sub1 id) (append res (cons (car adj) null)) )))])
    (aux sortant adj id null)))
      
;ajoute des arc entrant
(define (ajout-entrant adj entrant id)
  (if (null? entrant)
      adj
      (letrec ([aux (lambda (adj entrant id res i)
                      (cond [(null? entrant) (append res adj)]
                            [(= (car entrant) i) (aux (cdr adj) (cdr entrant) id (append res (cons (append (car adj) (cons id null)) null)) (add1 i))]
                            [else (aux (cdr adj) entrant id (append res (cons (car adj) null)) (add1 i))]))])
        (aux adj entrant id null 0)))
  )

;;connecter les arcs du graphe 
(define (connect-arc graph fact id)
  (let* ([conso (factory-consomation fact)]
        [produ (factory-production fact)]
        [entrant (sort (find-producteur graph conso) <)]
        [sortant (find-consomateur graph produ)]
        [adj (graphe-lst-adj graph)])
    (set! adj (ajout-sortant sortant adj id))
    (set! adj (ajout-entrant adj entrant id))
    adj))


;;ajout dans un graph
(define (ajout-graph graph fact)
  (if (not(null? (graphe-lstnode graph))) 
  (let ([new-node-id (add1 (node-id (car (graphe-lstnode graph))))] ; au debut si  (graphe-lstnode graph) n' est pas vide 
        [new-node (node (add1 (node-id (car (graphe-lstnode graph)))) fact)])
    (graphe (graphe-node_racine graph) (cons new-node (graphe-lstnode graph)) (connect-arc graph fact new-node-id)))
  (let ([new-node-id 0] ; au debut si  (graphe-lstnode graph) est vide 
        [new-node (node 0 fact)])
    (graphe (graphe-node_racine graph) (cons new-node (graphe-lstnode graph)) (connect-arc graph fact new-node-id)))))


(define (create-lst-null len)
  (letrec ([aux (lambda (len res)
                  (if (= len 0)
                      res
                      (aux (sub1 len) (cons null res))))])
    (aux len null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(create-lst-null 10)

;;creer un graphe a partie d'une liste de factory
(define (create-graph lst-fact)
  (if (null? lst-fact)
      (graphe null null null)
      (letrec ([lstAdjNull (create-lst-null (length lst-fact))]
               [g0 (graphe null null lstAdjNull)]
               [aux (lambda (lst-fact graph-res)
                      (if (null? lst-fact)
                          graph-res
                          (aux (cdr lst-fact) (ajout-graph graph-res (car lst-fact)))))])
        (aux lst-fact g0))))



(define graph-test(create-graph factlst))
;(graphe-lstnode graph-test)
;(graphe-lst-adj graph-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;affiche une liste de noeud
(define (aff-lstnode lst-node)
  (letrec ([aux (lambda (lst-node i)
                  (if (null? lst-node)
                      (printf "end\n")
                      (begin (printf "node ") (write i) (printf " : ") (printf "id ") (write (node-id (car lst-node)))
                             (printf "  ||  fact : conso = ") (write (factory-consomation (node-factory (car lst-node))))
                             (printf " produ = ") (write (factory-production (node-factory (car lst-node))))
                             (printf " cout = ") (write (factory-cout (node-factory (car lst-node))))
                             (printf "\n") (aux (cdr lst-node) (add1 i)))))])
    (aux lst-node 0)))

;affiche une liste d'adjacence
(define (aff-lstadj lst-adj)
  (letrec ([aux (lambda (lst-adj i)
                  (if (null? lst-adj)
                      (printf "end\n")
                      (begin (printf "L ") (write i) (printf " : ") (write (car lst-adj)) (printf "\n")
                             (aux (cdr lst-adj) (add1 i)))))])
    (aux lst-adj 0)))

;affiche un graphe
(define (aff-graph graph)
  (let ([len (length (graphe-lstnode graph))])
    (begin (printf "taille : ") (write len) (printf "\n"))
    (printf "lst node : \n" )
    (aff-lstnode (graphe-lstnode graph))
    (printf "lst adj : \n")
    (aff-lstadj (graphe-lst-adj graph))
  ))



;(aff-graph graph-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;calculer le prix d'une route
(define (prix road) ;calculer le prix d'une route 
  (letrec ([ aux (lambda (road value)
                   (if (null? road)
                       value
                       (aux (cdr road) (+ value (factory-cout (car road))))))])
    (aux road 0)))


;;comparer le prix de 2 routes
(define (compare road1 road2) ; comparer le prix de 2 routes road1 et road2
  (if (> (prix road1) (prix road2))
      road2
      road1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct chemin (lst-id cout gain));;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define adj-test (graphe-lst-adj graph-test))
;(aff-graph graph-test)
;adj-test

(define C1 (chemin '(1 3 8 5 9 2) 0 0))
(define C2 (chemin '(4 2 9 3 5 7) 0 0))
(define C3 (chemin '(5 2 6 5 0 5) 0 0))
(define C4 (chemin '(8 8 8 7 7 7) 0 0))
(define C5 (chemin '(0 1 2 3 4 6) 0 0))
(define lst-chemin-test (list C1 C2 C3 C4 C5))

;affiche un chemin
(define (aff-chemin c)
  (printf "lst : ") (write (chemin-lst-id c)) (printf " || cout :") (write (chemin-cout c)) (printf " | gain : ") (write (chemin-gain c))) (printf "\n")

(define (aff-lst-chemin lst-chemin)
  (letrec ([aux (lambda (lst-chemin i)
                  (if (null? lst-chemin)
                      (printf " end\n")
                      (begin (printf "C ") (write i) (printf " : ") (aff-chemin (car lst-chemin)) (printf "\n")
                             (aux (cdr lst-chemin) (add1 i)) )))])
    (aux lst-chemin 0)))

;;degré sortant du noeud id
(define (nb-voisin adj id)
  (letrec ([aux (lambda (adj id i)
                  (if (= id i)
                      (length (car adj))
                      (aux (cdr adj) id (add1 i))))])
    (aux adj id 0)))

;(nb-voisin (graphe-lst-adj graph-test) 1)

;cout d'un noeud
(define (cout-fact-node graph id)
  (letrec ([aux (lambda(lst-node id)
                  (if (= id (node-id (car lst-node)))
                      (factory-cout (node-factory (car lst-node)))
                      (aux (cdr lst-node) id)))])
    (aux (graphe-lstnode graph) id)))


;(cout-fact-node graph-test 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ajoute un noeud au graphe
(define (add-node lst-chemin current graph id)
  (if (= (end-chemin-id current lst-chemin) id)
      lst-chemin
      (letrec ([aux (lambda(lst-chemin current graph id i res)
                      (if (= current i)
                          (append (append res (cons (chemin (append (chemin-lst-id (car lst-chemin)) (cons id null))
                                                            (+ (chemin-cout (car lst-chemin)) (cout-fact-node graph id))
                                                            (chemin-gain (car lst-chemin))) null)) (cdr lst-chemin))
                          (aux (cdr lst-chemin) current graph id (add1 i) (append res (cons (car lst-chemin) null)))))])
        (aux lst-chemin current graph id 0 null))))
  

;(aff-lst-chemin lst-chemin-test)
;(aff-lst-chemin (add-node lst-chemin-test 0 graph-test 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;creer une liste des id des noeudVide contenue dans lst-node 
(define (node-vide-id lst-node)
  (letrec ([aux (lambda (lst-node res)
                  (cond [(null? lst-node) res]
                        [(equal? (cons (cons "" 0) null) (factory-consomation (node-factory (car lst-node)))) (aux (cdr lst-node) (cons (node-id (car lst-node)) res))]
                        [else (aux (cdr lst-node) res)]))])
    (aux lst-node null)))


;(aff-graph graph-test)
;(node-vide-id (graphe-lstnode graph-test))

;initialise la liste des chemin
(define (init-chemin graph)
  (letrec ([lst (node-vide-id (graphe-lstnode graph))]
           [aux (lambda (lst res)
                  (if (null? lst)
                      res
                      (aux (cdr lst) (cons (chemin (cons (car lst) null) (cout-fact-node graph (car lst)) 0) res))))])
    (aux lst null)))

;(aff-lst-chemin(init-chemin graph-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;revoit l'id du noeud qui est au bout du chemin lst-chemin[current]
(define (end-chemin-id current lst-chemin)
      (letrec ([aux (lambda (current lst-chemin i)
                      (cond [(null? lst-chemin) -1]
                            [(= current i) (last (chemin-lst-id (car lst-chemin)))]
                            [else (aux current (cdr lst-chemin) (add1 i))]))])
        (aux current lst-chemin 0)))


;(end-chemin-id 0 (init-chemin graph-test))
;(aff-lst-chemin lst-chemin-test)
;(end-chemin-id 0 lst-chemin-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;renvoit une cpoie du chemin lst-chemin[current]
(define (copie-chemin lst-chemin current)
  (letrec ([aux (lambda( lst-chemin current i)
                 (if (= i current)
                     (car lst-chemin)
                     (aux (cdr lst-chemin) current (add1 i))))])
    (aux lst-chemin current 0)))

;(aff-lst-chemin lst-chemin-test)
;(aff-chemin (copie-chemin lst-chemin-test 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ieme-id adj i)
  (letrec ([aux (lambda (adj i j)
                  (if (= i j)
                      (car adj)
                      (aux (cdr adj) i (add1 j))))])
    (aux adj i 0)))

;renvoit l'id du prochain noeud à explorer
(define (next-id adj id i)
  (letrec ([aux (lambda (adj id i j)
                  (if (= id j)
                      (ieme-id (car adj) i)
                      (aux (cdr adj) id i (add1 j))))])
    (aux adj id i 0)))

;adj-test
;(next-id adj-test 8 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ajoute un chemin à la list des chemin
(define (add-chemin lst-chemin current graph)
  (letrec ([adj (graphe-lst-adj graph)]
           [id (end-chemin-id current lst-chemin)]
           [len (nb-voisin adj id)]
           [c (copie-chemin lst-chemin current)]
           [aux (lambda (lst-chemin c taille i)
                  (if (= taille 1)
                      lst-chemin
                      (aux (append lst-chemin (cons (chemin (append (chemin-lst-id c) (cons (next-id adj id i) null))
                                                      (+ (chemin-cout c) (cout-fact-node graph (next-id adj id i)))
                                                      (chemin-gain c)) null) )
                                   c (sub1 taille) (add1 i))))])
    (aux (add-node lst-chemin current graph (next-id adj id 0)) c len 1)))

;adj-test
;(aff-lst-chemin lst-chemin-test)
;(aff-lst-chemin (add-chemin lst-chemin-test 0 graph-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;creer la liste de tous les chemin
(define (create-lst-chemin graph)
  (letrec ([lst-chemin (init-chemin graph)]
           [adj (graphe-lst-adj graph)]
           [nb (length lst-chemin)]
           [aux (lambda (graph current id i)
                  (cond [(= i 30000) (begin ;(printf "---the end : ") (write i) (printf "\n")
                                    lst-chemin)]
                         [(= nb 0) lst-chemin]
                        
                        [(= (nb-voisin adj id) 0) (begin (set! lst-chemin (add-node lst-chemin current graph id)) 
                                                         (set! nb (sub1 nb))
                                                         (aux graph (add1 current) (end-chemin-id (add1 current) lst-chemin)(add1 i)))]
                        
                        [(= (nb-voisin adj id) 1) (begin (set! lst-chemin (add-node lst-chemin current graph id))
                                                         (set! lst-chemin (add-chemin lst-chemin current graph))
                                                         (aux graph current (end-chemin-id current lst-chemin) (add1 i)))]
                        
                        [(> (nb-voisin adj id) 1) (begin (set! lst-chemin (add-node lst-chemin current graph id))
                                                         (set! lst-chemin (add-chemin lst-chemin current graph))
                                                         (set! nb (+ nb (nb-voisin adj id) -1))
                                                         (aux graph current (end-chemin-id current lst-chemin) (add1 i)))]))])
    
    ;(begin (printf "deb : ") (write nb) (printf " | ") (write (end-chemin-id 0 lst-chemin)) (printf "\n"))
    ;(aff-lst-chemin lst-chemin)
    (aux graph 0 (end-chemin-id 0 lst-chemin) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define graph-test2 (create-graph factlst)) ;;creer un graphe a partir d'une list de factory
;(aff-graph graph-test2) ;;affiche le graphe

;(aff-lst-chemin (create-lst-chemin graph-test)) ;; affiche la list des chemin créé à partir du graphe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

