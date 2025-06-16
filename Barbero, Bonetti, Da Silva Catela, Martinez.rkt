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

#! Esta funcion compara dos horas, si la primera es mayor que la segunda devuelve [true], sino, si las dos son iguales, compara los minutos. Si los minutos de la primera son mayores o iguales devuelve #t,  sino devuelve [falso] #!
(define (compararHora h1 h2)
  (let ([hora1 (car h1)]
        [min1 (car(cdr h1))]
        [hora2 (car h2)]
        [min2 (car(cdr h2))])
    (cond
      [(> hora1 hora2) #t]
      [(< hora1 hora2) #f]
      [(>= min1 min2)   #t]
      [(< min1 min2)   #f]
      )))

#! Esta funcion devuelve una lista con los horarios disponibles a partir de la hora ingresada. Aplica la funcion comparaHora para ir filtrando. #!
(define (horarios-disponibles lista-horas hora)
  (filter (lambda (h) (compararHora h hora)) lista-horas))


#! Esta funcion seria filtrar los horarios disponibles pero si usar el Predicado predefinido de Filter #!
(define (horarios-disponibles-sin-filter lista-horas hora)
  (cond
    [(empty? lista-horas) '()] ;Si la lista esta vacia, devuelve una lista vacia
    [(compararHora (car lista-horas) hora) ; Si compararHora da [True], entonces se ira alistando el primer elemento de la lista-horas y se sigue recorriendo el resto de la lista
      (cons (car lista-horas) (horarios-disponibles-sin-filter (cdr lista-horas) hora))]
    [else 
      (horarios-disponibles-sin-filter (cdr lista-horas) hora)])) ; Sino, se sigue iterando la lista de horas.  

#! Esta funcion busca el indice de la localidad en la lista de localidades. Si no lo encuentra devuelve [falso]. Si la lista esta vacia, devuelve [falso]. Si el primer elemento de la lista es igual al que buscas, devuelve [true]. Sino, llama recursivamente con el resto de la lista. #!
(define (buscar-indice localidad lista_localidades indice)
    (cond
      [(empty? lista_localidades) #f]
      [(equal? (car lista_localidades) localidad) indice]
      [else (buscar-indice localidad (cdr lista_localidades) (+ indice 1))]))

; Devuelve el elemento buscado por el Indice
(define (obtener-elemento-indice lista indice)
  (if (= indice 0)
      (car lista) ; Si el índice es 0, devuelve el primer elemento
      (obtener-elemento-indice (cdr lista) (- indice 1)))) ; Si no, sigue buscando en el resto de la lista


#! Esta funcion suma los costos de las localidades entre el indice de origen y el de destino. Si el indice de origen es mayor o igual al de destino, devuelve 0. Sino, suma el costo del indice de origen con la suma recursiva del resto de la lista. #!
(define (sumar-costos costos i-origen i-destino)
  (if (>= i-origen i-destino)
      0
      (+ (obtener-elemento-indice costos i-origen)
         (sumar-costos costos (+ i-origen 1) i-destino))))



(define (ArgentinaTur origen destino hora)
  (let* ([i-origen (buscar-indice origen localidades 0)]
         [i-destino (buscar-indice destino localidades 0)])
    (cond
      [(or (not i-origen) (not i-destino) (<= i-destino i-origen));Si no se encuentra el origen o destino, o si el destino es menor igual que el origen, lanza error
       '("Error")]
      [else
       (let* ([total-costo (sumar-costos costos i-origen i-destino)];Calcula el costo total desde el origen al destino
              [horarios-origen (obtener-elemento-indice horarios i-origen)] ;Obtiene la lista de horarios de la localidad de origen
              [salidas (horarios-disponibles horarios-origen hora)]) ;Filtra los horarios disponibles de la localidad de origen a partir de la hora ingresada
         (if (empty? salidas)
             (list (list origen destino) 0 '"No Hay Horarios De Salida Disponibles")
             (list (list origen destino) total-costo salidas)))])))

                    
(ArgentinaTur "Córdoba Capital" "La Falda" '(10 30))
(ArgentinaTur "Valle Hermoso" "La Cumbre" '(09 00))
(ArgentinaTur "Córdoba Capital" "La Falda" '(16 00))
(ArgentinaTur "La Falda" "Córdoba Capital" '(10 00))