#lang eopl
;Liseth Natalia Rivera Cordoba - 2223510
;Juan Diego Cardenas Mejia - 2416437
;Grosman Klein García Valencia - 2340247


(define mayor5?
  (lambda (x)
    (> x 5)))

(define crear-lista
  (lambda (x)
    (cons x empty)))

;------------------ 1 --------------------

; <par> ::= (<int> <int>)
; <lista-pares> ::= '()
;               ::= (<par> <lista-pares>)

;invert : lista-pares x Predicado -> lista-pares
;usage: (invert L P) = retorna una lista con los pares invertidos (y x)solo
;                      cuando ambos elementos del par cumplen el predicado P 
(define invert
  (lambda (L P)
    (if (null? L)
        empty
        (if (and (P (car (car L))) (P (cadr (car L))))
            (cons (crear-lista (cadr (car L)) (car (car L)))
                  (invert (cdr L) P))
            (invert (cdr L) P)
            )
        )
    )
  )

;------------------- 2 -----------------------

; <lista> ::= '()
;         ::= (<SchemeVal> <lista>)

;down : lista -> lista
;usage: (down L) = retorna una lista donde cada elemento de L tiene un
;                  nivel más de parentésis
(define down
  (lambda (L)
    (if (null? L)
        empty
        (cons (crear-lista (car L))
              (down (cdr L)))
        )
    )
  )

;----------------- 3 -------------------

;list-set : lista x int x SchemeVal x predicado  -> lista
;usage: (list-set L n x P) = retorna una lista similar a L, pero reemplaza el elemento en la
;                            posición n por x, solo si el elemento original cumple con P.
  
(define list-set
  (lambda (L n x P)
    (if (null? L)
        empty
        (if (zero? n)
            (if (P (car L))
                (cons x (cdr L)) L)
            (cons (car L)
                  (list-set (cdr L) (- n 1) x P))
            )
        )
    )
  )

;------------------ 4 ------------------------

;filter-in : predicado x lista  -> lista
;usage: (list-set L P) = retorna una lista que contiene los elementos de L
;                            que satisfacen el predicado P
(define filter-in
  (lambda (P L)
    (if (null? L)
        empty
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))
            )
        )
    )
)

;----------------- 5 --------------------

; <lista-simbolos> ::= '()
;                  ::= (<simbolo> <lista-simbolos>)

; reversa-aux : lista x lista -> lista
; usage: (reversa-aux L acc) = Invierte la lista L moviendo sus elementos uno por
;                              uno al acumulador recursivamente.
(define reversa-aux
  (lambda (L acc)
    (if (null? L)
        acc
        (reversa-aux (cdr L) (cons (car L) acc))
        )
    )
  )

;palindrome? : lista  -> bool
;usage: (palindrome? palabra) = Retorna #t si la palabra se lee igual de izquierda
;                               a derecha que de derecha a izquierda

(define palindrome?
  (lambda (palabra)
    (equal? palabra (reversa-aux palabra empty))
    )
  )

;------------------ 6 --------------------

; <lista> ::= '()
;         ::= (<SchemeVal> <lista>)
;swappper : SchemeVal x SchemeVal x lista -> lista
;usage: (swapper E1 x E2 x L) = Retorna una lista similar a L pero cada
;                               ocurrencia de E1 se reemplaza con E2, y viceversa.
(define swapper
  (lambda (E1 E2 L)
    (if (null? L)
        empty
        (cons (cond
                [(equal? E1 (car L)) E2]
                [(equal? E2 (car L)) E1]
                [else (car L)]
                )
              (swapper E1 E2 (cdr L))
              )
        )
    )
  )

;----------------- 7 ----------------------------------

;tuple-maker : SchemeValue x lista -> lista
;usage : (tuple-maker s L) = Retorna una lista de tuplas de todos los elementos
;                            de L emparejados con el símbolo s
(define tuple-maker
  (lambda (s L)
    (if (null? L)
        empty
        (cons (cons s (crear-lista(car L)))
              (tuple-maker s (cdr L))
              )
        )
    )
  )

;append-list : lista x lista -> lista
;usage : (append L1 L2) = Concatena las listas retornando una nueva lista con
;                        todos los elementos de L1 seguido de los elementos de L2.
(define append-list
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append-list (cdr L1) L2))
        )
    )
  )
;cartesian-product : lista-simbolos x lista-simbolos -> lista
;usage : (cartesian-product L1 L2) = Retorna la lista de tuplas de simbolos
;                                    resultado del producto cartesiano de ambas listas.
(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        empty
        (append-list (tuple-maker (car L1) L2) (cartesian-product (cdr L1) L2))
        )
    )
  )

;----------------------- 8 ------------------

;mapping : funcion x lista x lista -> lista
;usage : (mapping F L1 L2) = Retorna una lista de tuplas (a_n b_n) dónde a pertenece la posición n en L1
;                            b pertenece a la posición n en L2 y se cumple que F(a) = b
(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        empty
        (if (equal? (F (car L1)) (car L2))
            (cons (cons (car L1) (crear-lista (car L2)))
                  (mapping F (cdr L1) (cdr L2)))
            (mapping F (cdr L1) (cdr L2))
            )
        )
    )
  )


;------------------- 9 -----------------------------

; count-smaller : int x lista -> Int
; usage: (count-smaller x L) = retorna la cantidad de elementos en L que son menores que x.
(define count-smaller
  (lambda (x L)
    (if (null? L)
        0
        (if (> x (car L)) (+ 1 (count-smaller x (cdr L)))
          (count-smaller x (cdr L)))
        )
    )
  )
; inversions : lista -> int
; usage: (inversions L) = retorna el número de inversiones en L
(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (count-smaller (car L) (cdr L))
           (inversions (cdr L)))
        )
    )
  )

;--------------------- 10 --------------------

; <lista-parentesis> ::= '()
;         ::= (<parentesis> <lista-parebtesus>)
;<parentesis> ::= ( | )
;
; balanced-parentheses? : lista -> Bool
; usage: (alanced-parentheses? L) retorna #t si los paréntesis estan balanceados, 
;                                 de lo contrario retorna #f
(define balanced-parentheses?
  (lambda (L)
    (letrec 
        (
         (balance-aux 
          (lambda (lista cont)
            (if (< cont 0)
                #f 
                (if (null? lista)
                    (zero? cont) 
                    (cond
                      [(equal? (car lista) '"(") (balance-aux (cdr lista) (+ cont 1))]
                      [(equal? (car lista) '")") (balance-aux (cdr lista) (- cont 1))]
                      [else (balance-aux (cdr lista) cont)]
                      )
                    )
                )
            )
          )
         )
            (balance-aux L 0)
      )
    )
  )


; para hacer las pruebas deben colocar ""
; con el ejemplo del profe sería algo así:
; Prueba 1: ()() -> Debería ser #t
;(balanced-parentheses? '("(" ")" "(" ")"))
;(balanced-parentheses? '("(" "(" ")" ")" "(" ")"))


;---------------- 11 --------------------

;zip : Function x List x List -> List
;usage : (zip f l1 l2) = Una lista donde la posicion n-esima corresponde al resultado de aplicar
;                        la funcion F sobre los elementos en la posicion n-esima en L1 y L2.

(define zip
  (lambda (f l1 l2)
    (if (null? l1)
        empty
        (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))
        )
    )
  )


;-------------- 12 ----------------------

;filter-acum : Int x Int x Function x Int x Function
;usage : (filter-acum a b f acum filter) = El resultado acum de aplicar la funcion f a los elementos en [a,b] que cumplen filter

(define filter-acum
  (lambda (a b f acum filter)
    (if (> a b)
        acum
        (if (filter a)
            (filter-acum (+ a 1) b f (f acum a) filter)
            (filter-acum (+ a 1) b f acum filter)
            )
        )
    )
  )


(filter-acum 1 10 + 0 odd?)


;------------------ 13 --------------

;operate : List x List -> Int
;usage : (operate lrators lrands) = Un entero que es el resultado de aplicar sucesivamente
;                                     las operaciones de lratos a los valores lrands

(define lrators (list + * + - *))

(define operate
  (lambda (lrators lrands)
    (define helper
       (lambda (lrators lrands acum)
        (if (null? lrators)
            acum
            (helper (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))
         )
        )
       )
    (helper lrators (cdr lrands) (car lrands))
    )
  )

;---------------- 14 ---------------

; <arbol-binario> ::= (arbol-vacio) empty
;                 ::= (nodo) numero <arbol-binario> <arbol-binario>)
; path : n x BST -> lista
; usage: (path n BST) = retorna una lista con la ruta desde la raíz hasta n 

(define path
  (lambda (n BST)
    (if (null? BST)    ; n esta garantizado, lo dejamos o quitamos?
        empty
        (cond
          [(= n (car BST)) empty]
          [(< n (car BST)) (cons 'left (path n (cadr BST)))]
          [(> n (car BST)) (cons 'right (path n (caddr BST)))])
        )
    )
  )

;---------------- 15 ---------------

; <arbol-binario> ::= (arbol-vacio) empty
;                 ::= (nodo) numero <arbol-binario> <arbol-binario>)
; count-odd-and-even : BT -> lista
; usage: (count-odd-and-even arbol) = retorna con dos enteros denotando la cantidad de numeros pares e impares en el árbol.

(define count-odd-and-even
  (lambda (arbol)
    (if (null? arbol)
        (cons 0 (crear-lista 0))
        (let ([left-count (count-odd-and-even (cadr arbol))]
                 [right-count (count-odd-and-even (caddr arbol))])
          (cond
            [(even? (car arbol))
             (cons (+ 1 (car left-count) (car right-count))
                (crear-lista (+ (cadr left-count) (cadr right-count))))]
            [(odd? (car arbol))
             (cons (+ (car left-count) (car right-count))
                (crear-lista (+ 1 (cadr left-count) (cadr right-count))))])
          )
        )
    )
  )


;---------------- 16 ---------------
; <solucion> ::= (solucion-trivial) '(origen destino)
;                 ::= (solucion) <origen-a-auxiliar>(origen destino)<auxiliar-a-destino>
; hanoi : int x symbol x symbol -> lista
; usage: (hanoi n origen auxiliar destino) = retorna los movimientos necesarios para resolver el problema de las torres de hanoi
;                                            como una lista de pares de simbolos representado el paso de discos entre torres.
(define hanoi
  (lambda (n origen auxiliar destino)
    (if (equal? n 1)
        (crear-lista (cons origen (crear-lista destino)))
        (append-list(hanoi (- n 1) origen destino auxiliar)
                           (append-list (hanoi 1 origen auxiliar destino)
                                        (hanoi (- n 1) auxiliar origen destino))
                           )
                    )
        )
    )


;--------------- 17 ---------------

;coin-change : Int x List ->
;usage : (coin-change monto monedas) = el numero de combinaciones posibles para obtener
;                                        exactamente el valor monto con la lista de monedas dadas
(define (coin-change monto monedas)
  (cond
    ((= monto 0) 1)
    ((or (< monto 0) (null? monedas)) 0)
    (else
     (+ (coin-change monto (cdr monedas))              
        (coin-change (- monto (car monedas)) monedas))))) 


;---------------- 18 ---------------
; <fila> ::= (fila inicial) (1)
;        ::= (fila n) (0 <fila n-1>) + (<fila n-1> 0)
; pascal : int -> lista
; usage: (pascal) = retorna la n-ésima fila del triangulo de Pascal como una lista de enteros.

(define pascal
  (lambda (N)
    (if (equal? N 1)
        (crear-lista 1)
        (zip + (append-list (cons 0 empty) (pascal (- N 1)))
             (append-list (pascal (- N 1)) (cons 0 empty))
             )
        )
    )
  )

