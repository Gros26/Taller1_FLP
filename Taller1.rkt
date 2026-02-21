#lang eopl
#lang eopl

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
;usage: (list-set L n x P) = retorna una lista que contiene los elementos de L
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
;usage: (palindrome? palabra) = retorna #t si la palabra se lee igual de izquierda
;                               a derecha que de derecha a izquierda

(define palindrome?
  (lambda (palabra)
    (equal? palabra (reversa-aux palabra empty))
    )
  )