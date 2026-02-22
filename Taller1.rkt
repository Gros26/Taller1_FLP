#lang eopl
;Liseth ... xxxx
;Juan Diego ... xxxx
;Grosman Klein García Valencia - 2340247


(define mayor5?
  (lambda (x)
    (> x 5)))

(define crear-lista
  (lambda (x)
    (cons x empty)))

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

;f-tuple-maker : funcion x SchemeValue x lista -> lista
;usage : (f-tuple-maker F a L) = Retorna una lista de tuplas (a l) donde l es un elemento
;                                de l tal que F(a) =l
(define f-tuple-maker
  (lambda (F a L)
    (if (null? L)
        empty
        (if (equal? (F a) (car L))
            (cons (cons a (crear-lista (car L)))
                  (f-tuple-maker F a (cdr L)))
            (f-tuple-maker F a (cdr L))
            )
        )
    )
  )
;mapping : funcion x lista x lista -> lista
;usage : (mapping F L1 L2) = Retorna una lista de tuplas (a b) dónde a pertenece a L1
;                            b pertenece a L2 y se cumple que F(a) = b
(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        empty
        (append-list (f-tuple-maker F (car L1) L2)
                     (mapping F (cdr L1) L2))
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
            (filter-acum (+ a 1) b f (+ acum a) filter)
            (filter-acum (+ a 1) b f acum filter)
            )
        )
    )
  )


(filter-acum 1 10 + 0 odd?)


;------------- 13 ------------

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



; funcion como en scala, que se llama ya con un valor predeterminador, ejemplo
(define x
  (lambda (a)
    ((lambda (b) (x (+ a 2))
       )
     (+ a 2))
  )
 )





