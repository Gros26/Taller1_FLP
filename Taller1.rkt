#lang eopl
;Liseth Natalia Rivera Cordoba - 2223510
;Juan Diego Cardenas Mejia - 2416437
;Grosman Klein García Valencia - 2340247


(define mayor5?
  (lambda (x)
    (> x 5)))

;------------------ 1 --------------------

;; invert :
;; Proposito:
;; Lista de pares x Predicado -> Lista de pares : Procedimiento que evalua una lista 
;; de pares (x y). Si ambos elementos del par cumplen con el predicado P, invierte 
;; el par a (y x) y lo conserva. Si alguno no cumple, el par es descartado

;; <par> ::= (<int> <int>)
;; <lista-pares> ::= ()
;;               ::= (<par> <lista-pares>)

(define invert
  (lambda (L P)
    (if (null? L)
        empty
        (if (and (P (car (car L))) (P (cadr (car L))))
            (cons (list (cadr (car L)) (car (car L)))
                  (invert (cdr L) P))
            (invert (cdr L) P)
            )
        )
    )
  )

;; Pruebas
; 1. Caso base
(invert empty odd?) 

; 2. Caso mixto: algunos pares cumplen y otros no
(invert '((3 2) (4 2) (1 5) (2 8)) even?) 

; 3. Caso donde todos los pares cumplen con el predicado
(invert '((1 3) (5 7) (9 11)) odd?)

; 4. Caso donde ningún par cumple con el predicado
(invert '((2 4) (6 8)) odd?)


;------------------- 2 -----------------------

;; down :
;; Proposito:
;; Lista -> Lista : Procedimiento que retorna una nueva lista donde cada elemento 
;; de la lista original L tiene un nivel adicional de parentesis
;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)

(define down
  (lambda (L)
    (if (null? L)
        empty
        (cons (list (car L))
              (down (cdr L)))
        )
    )
  )

;; Pruebas
; 1. Caso base
(down empty)

; 2. Caso con una lista plana de números
(down '(1 2 3))
; Resultado esperado: '((1) (2) (3))

; 3. Caso con símbolos y listas anidadas
(down '(a (hi (hello))))

; 4. Caso con elementos que ya son listas
(down '((a) (b)))


;----------------- 3 -------------------

;; list-set :
;; Proposito:
;; Lista x Int x SchemeVal x Predicado -> Lista : Procedimiento que retorna una 
;; lista similar a L, pero reemplaza el elemento en la posicion n (indexada desde 0) 
;; por el valor x, solo si el elemento original en esa posicion satisface el predicado P

;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)
  
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

;; Pruebas
; 1. Caso base
(list-set '() 2 'x odd?)

; 2. Caso en el que elemento en la posicion n cumple predicado
(list-set '(1 2 4 5) 2 'x even?)

; 3. Caso en el que el elemento en la posicion n no cumple el predicado
(list-set '(1 3 4 5) 1 'x even?)

; 4. Prueba indexación 0  
(list-set '(a b c) 0 'z symbol?)

; 5. Cado donde el indice n es mayor a la longitud de la lista
(list-set '(1 2 3) 5 'x odd?)


;------------------ 4 ------------------------

;; filter-in :
;; Proposito:
;; Predicado x Lista -> Lista : Procedimiento que retorna una nueva lista que 
;; contiene unicamente los elementos de L que satisfacen el predicado P

;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)

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

;; Pruebas
; 1. Caso base
(filter-in odd? empty)

; 2. Caso con una lista mixta
(filter-in symbol? '(a 1 b 2 c 3))

; 3. Caso donde ningún elemento cumple el predicado
(filter-in string? '(1 2 3 4))

; 4. Caso donde filtramos solo los números pares de una lista de enteros
(filter-in even? '(1 2 3 4 5 6))


;----------------- 5 --------------------

;; reversa-aux :
;; Proposito:
;; Lista de Simbolos x Lista de Simbolos -> Lista de Simbolos : Procedimiento que 
;; invierte la lista L moviendo sus elementos uno por uno al acumulador 'acc' 
;; de forma recursiva

;; <lista-simbolos> ::= ()
;;                  ::= (<Simbolo> <lista-simbolos>)

(define reversa-aux
  (lambda (L acc)
    (if (null? L)
        acc
        (reversa-aux (cdr L) (cons (car L) acc))
        )
    )
  )

;; palindrome? :
;; Proposito:
;; Lista de Simbolos -> Bool : Procedimiento que determina si una lista de 
;; simbolos es un palindromo, es decir, se lee exactamente igual de izquierda 
;; a derecha que de derecha a izquierda
;; <palindromo> ::= (<Simbolo>)
;;              ::= (<Mismo Simbolo> <palindromo> <Mismo Simbolo>)

(define palindrome?
  (lambda (palabra)
    (cond
      [(null? palabra) #f]
      [(null? (cdr palabra)) #t]
      [else
       (let (
             [palabra-reversed (reversa-aux palabra empty)]
             [palabra-inner (cdr (reversa-aux (cdr palabra) empty))])
         (if (equal? (car palabra) (car palabra-reversed))
             (palindrome? palabra-inner)
             #f)
         )
       ])
    )
  )

;; Pruebas
; 1. Caso base
(palindrome? empty)

; 2. Caso palindromo con longitud impar
(palindrome? '(r a d a r))

; 3. Caso palindromo con longitud par
(palindrome? '(a n n a))

; 4. Caso con palabra que no es palindroma
(palindrome? '(m o t o))

; 5. Palabra de un solo caracter 
(palindrome? '(x))

;------------------ 6 --------------------

;; swapper :
;; Proposito:
;; SchemeVal x SchemeVal x Lista -> Lista : Procedimiento que retorna una lista 
;; similar a L, pero donde cada ocurrencia del elemento E1 es reemplazada por E2, 
;; y cada ocurrencia de E2 es reemplazada por E1

;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)

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

;; Pruebas
; 1. Caso base
(swapper 'x 'y empty)

; 2. Caso donde se intercambian símbolos múltiples veces en una lista
(swapper 'a 'b '(a b c d a b))

; 3. Caso con lista de solo números
(swapper 1 2 '(1 2 3 1 2))

; 4. Caso donde los elementos a intercambiar no están en la lista
(swapper 'x 'y '(a b c))

; 5. Caso con lista mixta
(swapper 7 'a '(c 7 3 a a))


;----------------- 7 ----------------------------------

;; tuple-maker :
;; Proposito:
;; SchemeVal x Lista -> Lista de Tuplas : Procedimiento que retorna una lista 
;; de tuplas de todos los elementos de L emparejados con el símbolo s

;; <lista-tuplas> ::= ()
;;                ::= ((<SchemeVal> <SchemeVal>) <lista-tuplas>)

(define tuple-maker
  (lambda (s L)
    (if (null? L)
        empty
        (cons (cons s (list(car L)))
              (tuple-maker s (cdr L))
              )
        )
    )
  )

;; append-list :
;; Proposito:
;; Lista x Lista -> Lista : Procedimiento que concatena dos listas, retornando 
;; una nueva lista con todos los elementos de L1 seguidos de los elementos de L2

(define append-list
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append-list (cdr L1) L2))
        )
    )
  )

;; cartesian-product :
;; Proposito:
;; Lista de Simbolos x Lista de Simbolos -> Lista de Tuplas : Procedimiento que retorna
;; una lista de tuplas correspondientes al producto cartesiano entre ambas listas

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        empty
        (append-list (tuple-maker (car L1) L2) (cartesian-product (cdr L1) L2))
        )
    )
  )

;; Pruebas
; 1. Caso donde L1 es vacia
(cartesian-product '() '(x y))

; 2. Caso donde L2 es vacia
(cartesian-product '(1 4 7) '())

; 3. Caso de dos listas normales 
(cartesian-product '(a b c) '(x y))

; 4. Producto cartesiano con una lista diferentes (enteros y simbolos)
(cartesian-product '(p q) '(1 2 3))

; 5. Caso donde ambas listas tienen un solo elemento
(cartesian-product '(z) '(w))


;----------------------- 8 ------------------

;; mapping :
;; Proposito:
;; Funcion x Lista x Lista -> Lista de Tuplas : Procedimiento que retorna una 
;; lista de tuplas (a_n b_n), dónde a pertenece la posición n en L1, b
;; pertenece a la posición n en L2 y se cumple que F(a) = b

;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)
;; <lista-tuplas> ::= ()
;;                ::= ((<SchemeVal> <SchemeVal>) <lista-tuplas>)

(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        empty
        (if (equal? (F (car L1)) (car L2))
            (cons (cons (car L1) (list (car L2)))
                  (mapping F (cdr L1) (cdr L2)))
            (mapping F (cdr L1) (cdr L2))
            )
        )
    )
  )

;; Pruebas
; 1. Caso base
(mapping (lambda (x) x) empty empty)

; 2. Caso en donde todos cumplen
(mapping (lambda (d) (* d 2)) '(1 2 3) '(2 4 6))

; 3. Caso donde solo algunos elementos cumplen la condición
(mapping (lambda (d) (* d 2)) '(1 2 3) '(3 4 5))

; 4. Usando predicados de Racket
(mapping even? '(2 3 4) '(#t #f #t))


;------------------- 9 -----------------------------

;; count-smaller : (Funcion Auxiliar)
;; Proposito:
;; Int x Lista de Enteros -> Int : Procedimiento que retorna la cantidad de elementos 
;; en la lista L que son estrictamente menores que el numero x

;; <lista-enteros> ::= ()
;;                 ::= (<Int> <lista-enteros>)

(define count-smaller
  (lambda (x L)
    (if (null? L)
        0
        (if (> x (car L)) (+ 1 (count-smaller x (cdr L)))
          (count-smaller x (cdr L)))
        )
    )
  )

;; inversions :
;; Proposito:
;; Lista de Enteros -> Int : Procedimiento que retorna el numero total de inversiones 
;; en una lista L.

(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (count-smaller (car L) (cdr L))
           (inversions (cdr L)))
        )
    )
  )

;; Pruebas para inversions (y por extension, count-smaller)
; 1. Caso base
(inversions empty)

; 2. Caso donde no hay inversiones posibles (orden descendente)
(inversions '(1 2 3 4 5))

; 3. Caso con el número máximo de inversiones (orden ascendente)
(inversions '(5 4 3 2 1))

; 4. Caso con lista desordenada
(inversions '(2 3 8 6 1))


;--------------------- 10 --------------------
;; balanced-parentheses? :
;; Proposito:
;; List -> Bool : Procedimiento que determina si los paréntesis estan balanceados donde
;;  O representa a ( y C representa a )
;;
;; <lista-parentesis> ::= ()
;;                    ::= (<parentesis> <lista-parentesis>)
;; <parentesis>       ::= O | C

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
                      [(equal? (car lista) 'O) (balance-aux (cdr lista) (+ cont 1))]
                      [(equal? (car lista) 'C) (balance-aux (cdr lista) (- cont 1))]
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

;; Pruebas
; 1. Caso base
(balanced-parentheses? empty)

; 2. Caso con paréntesis balanceados anidados
(balanced-parentheses? '(O O C C))

; 3. Caso con paréntesis balanceados secuenciales
(balanced-parentheses? '(O C O C))

; 4. Caso de desbalance por exceso de cierres 
(balanced-parentheses? '(O O C C C))

; 5. Caso de desbalance por empezar cerrando
(balanced-parentheses? '(C O C O))

;---------------- 11 --------------------

;; zip :
;; Proposito:
;; Funcion x Lista x Lista -> Lista : Procedimiento que retorna una nueva lista 
;; donde la posicion n-esima corresponde al resultado de aplicar la funcion F 
;;  sobre los elementos en la posicion n-esima en L1 y L2

;; <lista> ::= ()
;;         ::= (<SchemeVal> <lista>)

(define zip
  (lambda (f l1 l2)
    (if (null? l1)
        empty
        (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))
        )
    )
  )

;; Pruebas
; 1. Caso base
(zip * empty empty)

; 2. Caso con operador matematico
(zip + '(1 4) '(6 2))

; 3. Caso con operador logico para comparar elementos posicion a posicion
(zip equal? '(a b c) '(a x c))

; 4. Caso usando list para agrupar elementos de ambas listas en sublistas
(zip list '(a b c) '(1 2 3))


;-------------- 12 ----------------------

;; filter-acum :
;; Proposito:
;; Int x Int x Funcion x Int x Funcion -> Int : Procedimiento que aplica la funcion f
;; a un valor acumulado "acum" y retorna el resultado "acum" de aplicar la funcion f 
;; a los elementos en [a,b] que cumplen filter

;; <rango> ::= [<Int> <Int>]   donde a <= b define un rango valido

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

;; Pruebas
; 1. Caso de suma (numeros impares entre 1 y 10)
(filter-acum 1 10 + 0 odd?)

; 2. Caso de multiplicación (numeros pares entre 1 y 5)
(filter-acum 1 5 * 1 even?)

; 3. Caso con rango es invalido
(filter-acum 5 1 + 0 even?)


;------------------ 13 --------------

;; operate :
;; Proposito:
;; Lista de Operadores x Lista de Operandos -> SchemeVal : Procedimiento que aplica
;; sucesivamente cada operador de lrators al valor acumulado con el siguiente elemento
;; de lrands, iniciando con el primer elemento de lrands como acumulador inicial

;; <lista-operadores> ::= ()
;;                    ::= (<Funcion> <lista-operadores>)
;; <lista-operandos>  ::= (<SchemeVal>)
;;                    ::= (<SchemeVal> <lista-operandos>)

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

;; Pruebas
; 1. Caso con un único operando y sin operadores
(operate '() '(5))
; Resultado esperado: 5

; 2. Caso con suma y multiplicación
(operate (list + *) '(1 2 3))
; Resultado esperado: 9  ; ((1 + 2) * 3)

; 3. Caso usando la lista lrators predefinida
(operate lrators '(1 2 3 4 5 6))
; Resultado esperado: 48  ; (((((1+2)*3)+4)-5)*6)

; 4. Caso con resta y suma
(operate (list - +) '(10 3 2))
; Resultado esperado: 9  ; ((10 - 3) + 2)


;---------------- 14 ---------------

;; path :
;; Proposito:
;; Int x BST -> Lista de Simbolos : Procedimiento retorna una lista de simbolos 
;; ('left y 'right) que representa la ruta desde la raiz hasta el nodo que contiene a 'n'.

;; <arbol-binario> ::= empty
;;                 ::= (<Int> <arbol-binario> <arbol-binario>)

(define path
  (lambda (n BST)
    (if (null? BST)    
        empty
        (cond
          [(= n (car BST)) empty]
          [(< n (car BST)) (cons 'left (path n (cadr BST)))]
          [(> n (car BST)) (cons 'right (path n (caddr BST)))])
        )
    )
  )

;; Pruebas
; 1. Buscar la raiz
(path 8 '(8 (3 (1 empty empty) (6 empty empty)) (10 empty (14 empty empty))))

; 2. Buscar un nodo que esta a la izquierda y luego a la derecha
(path 6 '(8 (3 (1 empty empty) (6 empty empty)) (10 empty (14 empty empty))))

; 3. Buscar un nodo profundo a la derecha
(path 14 '(8 (3 (1 empty empty) (6 empty empty)) (10 empty (14 empty empty))))

; 4. Buscar el nodo mas a la izquierda
(path 1 '(8 (3 (1 empty empty) (6 empty empty)) (10 empty (14 empty empty))))


;---------------- 15 ---------------

;; count-odd-and-even :
;; Proposito:
;; arbol-binario -> Lista : Procedimiento que retorna una lista de dos enteros
;; donde el primero indica la cantidad de numeros pares en el arbol y
;; el segundo la cantidad de numeros impares

;; <arbol-binario> ::= ()
;;                 ::= (<Int> <arbol-binario> <arbol-binario>)

(define count-odd-and-even
  (lambda (arbol)
    (if (null? arbol)
        (cons 0 (list 0))
        (let ([left-count (count-odd-and-even (cadr arbol))]
                 [right-count (count-odd-and-even (caddr arbol))])
          (cond
            [(even? (car arbol))
             (cons (+ 1 (car left-count) (car right-count))
                (list (+ (cadr left-count) (cadr right-count))))]
            [(odd? (car arbol))
             (cons (+ (car left-count) (car right-count))
                (list (+ 1 (cadr left-count) (cadr right-count))))])
          )
        )
    )
  )

;; Pruebas
; 1. Caso base: árbol vacío
(count-odd-and-even empty)
; Resultado esperado: (0 0)

; 2. Caso con solo la raiz siendo par
(count-odd-and-even (list 4 empty empty))
; Resultado esperado: (1 0)

; 3. Caso con solo la raiz siendo impar
(count-odd-and-even (list 3 empty empty))
; Resultado esperado: (0 1)

; 4. Árbol completo: pares={8,6,10,14}, impares={3,1}
(count-odd-and-even (list 8 (list 3 (list 1 empty empty) (list 6 empty empty)) (list 10 empty (list 14 empty empty))))
; Resultado esperado: (4 2)

; 5. Árbol con todos los nodos impares
(count-odd-and-even (list 5 (list 3 empty empty) (list 7 empty empty)))
; Resultado esperado: (0 3)


;---------------- 16 ---------------

;; hanoi :
;; Proposito:
;; Int x Symbol x Symbol x Symbol -> Lista de Pares : Procedimiento que retorna
;; la secuencia de movimientos necesarios para resolver las Torres de Hanoi con n discos,
;; representando cada movimiento como un par (torre-origen torre-destino)

;; <solucion> ::= ()
;;             ::= ((<Symbol> <Symbol>) <solucion>)
(define hanoi
  (lambda (n origen auxiliar destino)
    (if (equal? n 1)
        (list (cons origen (list destino)))
        (append-list(hanoi (- n 1) origen destino auxiliar)
                           (append-list (hanoi 1 origen auxiliar destino)
                                        (hanoi (- n 1) auxiliar origen destino))
                           )
                    )
        )
    )

;; Pruebas
; 1. Caso con 1 disco
(hanoi 1 'A 'B 'C)
; Resultado esperado: '((A C))

; 2. Caso con 2 discos
(hanoi 2 'A 'B 'C)
; Resultado esperado: '((A B) (A C) (B C))

; 3. Caso con 3 discos
(hanoi 3 'A 'B 'C)
; Resultado esperado: '((A C) (A B) (C B) (A C) (B A) (B C) (A C))

; 4. Caso con nombres de torres descriptivos
(hanoi 2 'inicio 'aux 'fin)
; Resultado esperado: '((inicio aux) (inicio fin) (aux fin))


;--------------- 17 ---------------

;; coin-change :
;; Proposito:
;; Int x Lista de Enteros -> Int : Procedimiento que retorna el numero de
;; combinaciones posibles para obtener exactamente el valor monto utilizando
;; las monedas disponibles en la lista (con repeticion permitida)

;; <lista-monedas> ::= ()
;;                 ::= (<Int> <lista-monedas>)
(define coin-change
  (lambda (monto monedas)
    (cond
      ((= monto 0) 1)
      ((or (< monto 0) (null? monedas)) 0)
      (else
      (+ (coin-change monto (cdr monedas))              
          (coin-change (- monto (car monedas)) monedas)
        )
      )
    )
  )
) 

;; Pruebas
; 1. Caso base: monto 0 (siempre hay exactamente 1 combinacion)
(coin-change 0 '(1 5 10))
; Resultado esperado: 1

; 2. Caso sin monedas disponibles
(coin-change 5 '())
; Resultado esperado: 0

; 3. Caso donde el monto no se puede obtener con las monedas dadas
(coin-change 3 '(5 10))
; Resultado esperado: 0

; 4. Caso con monedas estandar (cambio de 10 con monedas 1, 5, 10)
(coin-change 10 '(1 5 10))
; Resultado esperado: 4  ; {10}, {5+5}, {5+1*5}, {1*10}

; 5. Caso con una sola denominacion
(coin-change 6 '(2))
; Resultado esperado: 1  ; solo {2+2+2}


;---------------- 18 ---------------

;; pascal :
;; Proposito:
;; Int -> Lista de Enteros : Procedimiento que retorna la N-esima fila del
;; triangulo de Pascal como una lista de enteros


(define pascal
  (lambda (N)
    (if (equal? N 1)
        (list 1)
        (zip + (append-list (cons 0 empty) (pascal (- N 1)))
             (append-list (pascal (- N 1)) (cons 0 empty))
             )
        )
    )
  )

;; Pruebas
; 1. Primera fila
(pascal 1)
; Resultado esperado: '(1)

; 2. Segunda fila
(pascal 2)
; Resultado esperado: '(1 1)

; 3. Tercera fila
(pascal 3)
; Resultado esperado: '(1 2 1)

; 4. Cuarta fila
(pascal 4)
; Resultado esperado: '(1 3 3 1)

; 5. Quinta fila
(pascal 5)
; Resultado esperado: '(1 4 6 4 1)

