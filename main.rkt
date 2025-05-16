#lang racket

(define localidades
  '("Córdoba Capital" "Carlos Paz" "Bialet Massé" "Valle Hermoso"
    "La Falda" "Huerta Grande" "La Cumbre" "Capilla Del Monte"))

(define costos
  '(1500 1500 1000 1200 1000 1200 1600))

(define horarios
  '(((07 00) (10 00) (12 00))
    ((07 30) (10 30) (12 30))
    ((07 45) (10 45) (12 45))
    ((08 15) (11 15) (13 15))
    ((08 30) (11 30) (13 30))
    ((08 45) (11 45) (13 45))
    ((09 30) (12 30) (14 30))
    ((10 00) (13 00) (15 00))))

(define (compararHora h1 h2)
  (or (> (first h1) (first h2))
      (and (= (first h1) (first h2))
           (>= (second h1) (second h2)))))

(define (horarios-disponibles lista-horas hora)
  (filter (lambda (h) (compararHora h hora)) lista-horas))

(define (buscar-indice loc lista)
  (let loop ((iter lista) (i 0))
    (cond
      [(empty? iter) #f]
      [(equal? (car iter) loc) i]
      [else (loop (cdr iter) (+ i 1))])))

(define (sumar-costos costos i-origen i-destino)
  (if (>= i-origen i-destino)
      0
      (+ (list-ref costos i-origen)
         (sumar-costos costos (+ i-origen 1) i-destino))))

(define (obtener-horarios horarios i  )
  (if (= i 0)
      (car horarios)
      (obtener-horarios (cdr horarios) (- i 1))))

(define (ArgentinaTur origen destino hora)
  (let* ([i-origen (buscar-indice origen localidades)]
         [i-destino (buscar-indice destino localidades)])
    (cond
      [(or (not i-origen) (not i-destino) (<= i-destino i-origen))
       '("ERROR")]
      [else
       (let* ([total-costo (sumar-costos costos i-origen i-destino)]
              [horarios-origen (obtener-horarios horarios i-origen)]
              [salidas (horarios-disponibles horarios-origen hora)])
         (if (empty? salidas)
             '("NO HAY HORARIOS DE SALIDA DISPONIBLES")
             (list (list origen destino) total-costo salidas)))])))

(ArgentinaTur "Córdoba Capital" "La Falda" '(10 30))
                      