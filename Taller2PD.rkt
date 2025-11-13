#lang racket/base
; Ejercicio 1 – Contar elementos positivos en una lista
(define (contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))
  
(contar-positivos '(3 -2 7 0 -5 9))

; Ejercicio 2 – Generar lista de cuadrados pares
(define (cuadrados-pares lista)
  (map (lambda (x) (* x x))
       (filter (lambda (x) (even? x)) lista)))

(cuadrados-pares '(1 2 3 4 5 6 7 8))

;Ejercicio 3 – Calcular el factorial de un número
(define (factorial n)
  (if (= n 0) 
      1
      (* n (factorial (- n 1)))))
      
(factorial 5)

; Ejercicio 4 – Elevar cada número al cubo
(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))
  
(cubos '(2 3 4))

; Ejercicio 5 – Sumar todos los elementos impares
(define (sumar-impares lista)
  (foldl + 0
         (filter (lambda (x) (odd? x)) lista)))
         
(sumar-impares '(1 2 3 4 5 6 7))

; Ejercicio 6 – Determinar si una lista contiene números negativos
(define (contiene-negativos? lista)
  (ormap (lambda (x) (< x 0)) lista))
  
(contiene-negativos? '(5 9 -3 2))

; Ejercicio 7 – Calcular la suma acumulada de una lista
(define (suma-acumulada lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))
          
(suma-acumulada '(1 2 3 4))

; Ejercicio 8 – Concatenar cadenas de texto 
(define (concatenar-cadenas lista)
  (foldl string-append "" lista))
  
(concatenar-cadenas '("Hola" " " "Mundo"))

; Ejercicio 9 – Generar lista con el doble de los números mayores que 5
(define (dobles-mayores lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

(dobles-mayores '(3 6 8 2 10))

; Ejercicio 10 – Invertir una lista
(define (invertir lista)
  (if (null? lista)
      '()
      (append (invertir (cdr lista)) (list (car lista)))))

(invertir '(1 2 3 4))

; Ejercicio 11 – Función que recibe otra función como parámetro
(define (aplicar-lista f lista)
  (map f lista))
  
(aplicar-lista (lambda (x) (* x x)) '(1 2 3 4))

; Ejercicio 12 – Reto integrador: combinar múltiples funciones (duda /)
(define (promedio-mayores lista)
  (let* ([mayores (filter (lambda (x) (> x 5)) lista)]
         [suma    (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (/ (* 1.0 suma) cantidad)))

(promedio-mayores '(3 8 10 4 9 2 7))








