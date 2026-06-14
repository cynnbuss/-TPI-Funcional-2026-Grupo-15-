;; ================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICIÓN
;; ================================

;; FUNCIÓN: es-transicion-permitida
;; NATURALEZA: Pura (solo devuelve verdadero o falso)
;; ESTRATEGIA: Función predicado
;; IMPACTO: No destructiva
;; ================================

(defun es-transicion-permitida (color-actual cambiar-a)
  (or (and (equal color-actual 'en-rojo) (equal cambiar-a 'verde))
      (and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo))
      (and (equal color-actual 'en-amarillo) (equal cambiar-a 'rojo))))

;; ================================
;; FUNCIÓN: accion-color
;; NATURALEZA: Pura (devuelve texto basado en el color)
;; ESTRATEGIA: Evaluación booleana
;; IMPACTO: No destructiva
;; ================================

(defun accion-color (cambiar-a)
  (or (and (equal cambiar-a 'rojo) "cambiar-a-rojo")
      (and (equal cambiar-a 'amarillo) "cambiar-a-amarillo")
      (and (equal cambiar-a 'verde) "cambiar-a-verde")))

;; ================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (solo devuelve una lista)
;; ESTRATEGIA: Condicional simple 
;; IMPACTO: No destructiva
;; ================================

(defun transicion (color-actual cambiar-a)
   (if (es-transicion-permitida color-actual cambiar-a)
       (list color-actual (accion-color cambiar-a))
      (list color-actual 'accion-por-defecto))) 

;; ============================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO
;; ============================================================

;; ============================================================
;; FUNCIÓN: ubicar-fase
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Condicional Múltiple  
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun ubicar-fase (resto)
  (cond 
    ((< resto 90) 'en-rojo)        ;; 0 a 89 segundos
    ((< resto 210) 'en-verde)      ;; 90 a 209 segundos
    (t 'en-amarillo)))             ;; 210 a 215 segundos
;; ============================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Composición Funcional 
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun timer (tiempo-unix)
  (if (integerp tiempo-unix)
      (ubicar-fase (mod tiempo-unix 216))
      "Error: El tiempo Unix debe ser un numero entero"))


;; ============================================================
;; REQUERIMIENTO 3: SISTEMA DE AUDITORÍA
;; ============================================================

;; ============================================================
;; FUNCIÓN: es-color-valido
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Función Predicado
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun es-color-valido (color)
  (or (equal color 'en-rojo)
      (equal color 'en-amarillo)
      (equal color 'en-verde)))

;; ============================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Imprime en pantalla)
;; ESTRATEGIA DE CONTROL: Secuencial con validación 
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun informe (tiempo-epoch color-anterior color-nuevo)
  (if (and (integerp tiempo-epoch) 
           (es-color-valido color-anterior) 
           (es-color-valido color-nuevo))
      (format t "Tiempo ~a: la luz ha cambiado de ~a a ~a~%" 
              tiempo-epoch color-anterior color-nuevo)   
       "Error: Tipos de datos invalidos para auditoria"))


;; ============================================================
;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; ============================================================

;; FUNCIÓN 4-a: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA DE CONTROL: Condicional Múltiple
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun duracion-ciclo (segundos)
  (cond
    ((or (not (numberp segundos)) (<= segundos 0)) "Error: La duracion ingresada debe ser un numero positivo");;  Verificamos si no es número o es <= 0
    ((and (>= segundos 35) (<= segundos 150)) (list segundos 'Rango-Optimo)) ;; Rango Óptimo (entre 35 y 150 segundos) 
     ((< segundos 35) (list segundos 'Duracion-baja))
     (t (list segundos 'Duracion-alta))))

;; ============================================================
;; FUNCIÓN 4-b: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA DE CONTROL: Condicional Múltiple
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun recomendacion-ciclo (duracion)
  (cond
    ((or (not (numberp duracion)) (<= duracion 0)) "Error: La duracion ingresada debe ser un numero positivo")
    ((and (>= duracion 35) (<= duracion 150))"Recomendacion: Ciclo optimo")
    ((< duracion 35) "Recomendacion: Aumentar tiempo")
    (t "Recomendacion: Reducir tiempo")))

;; ============================================================
;; REQUERIMIENTO 5: PLANIFICACIÓN TEMPORAL
;; ============================================================

;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Condicional Simple con TRUNCATE
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================

(defun ciclos-por-tiempo (minutos)
  (if (and (numberp minutos) (>= minutos 0));;Validamos que minutos sea numero y sea mayor o igual que 0
      (truncate (/ (* minutos 60)
                   (car (duracion-ciclo 216)))) ;;utilizamos el car de la funcion del punto 4a 
      "Error: los minutos deben ser un numero positivo"))


;;=================================================
;;REQUERIMIENTO 6: Informe de Distribución Temporal 
;;=================================================

;;FUNCION:tiempo-total
;;NATURALEZA:pura
;;ESTRATEGIA DE CONTROL: Recursividad simple 
;;IMPACTO EN MEMORIA: No destructiva 
;; ============================================================
(defun tiempo-total (historial) 
  (cond 
      ((endp historial) 0)
      (t (+ (cadar historial)
          (tiempo-total (cdr historial))))))




;; ============================================================ 
;; FUNCIÓN: tiempo-total-color 
;; NATURALEZA: Pura (Calcula el tiempo acumulado de un color) 
;; ESTRATEGIA DE CONTROL: Recursividad 
;; IMPACTO EN MEMORIA: No destructiva 
;; ============================================================
(defun tiempo-total-color (historial color-buscado)
  (cond 
    ((endp historial) 0) 
    ((equal (caar historial) color-buscado)
     (+ (cadar historial)
        (tiempo-total-color (cdr historial) color-buscado))) 
        (t (tiempo-total-color (cdr historial) color-buscado))))




;; ============================================================ 
;; FUNCIÓN: porcentaje-color 
;; NATURALEZA: Pura (Calcula el porcentaje correspondiente a un color)
;; ESTRATEGIA DE CONTROL: Composición funcional con Let e If 
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun porcentaje-color (historial color) 
  (let ((total (tiempo-total historial)))
       (if (= total 0) 
           0.0
           (float (/ (* (tiempo-total-color historial color) 100) total)))))




;; ============================================================ 
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Pura (Genera el informe porcentual de los colores) 
;; ESTRATEGIA DE CONTROL: Composición funcional con List 
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun informe-distribucion (historial) 
  (if(listp historial )
   (list 
   (list 'rojo (porcentaje-color historial 'rojo)) 
   (list 'amarillo (porcentaje-color historial 'amarillo)) 
   (list 'verde (porcentaje-color historial 'verde))))




;;======================================
;; REQUERIMIENTO 7: Casos de Prueba
;;======================================

;;------------------------
;;Pruebas Requerimiento 1
;;------------------------

  
;;(transicion 'en-rojo 'verde)
;;(EN-ROJO "cambiar-a-verde")

;; Camino alternativo:
;;(transicion 'en-verde 'amarillo)
;;(EN-VERDE "cambiar-a-amarillo")

;; Ejemplo que genera error:
;;(transicion 'en-rojo 'amarillo)
;;(EN-ROJO ACCION-POR-DEFECTO)


;;-----------------------
;;Prubas Requerimiento 2
;;-----------------------
;;Resultado Normal: 
;;(timer 100)
;;EN-VERDE

;; Camino alternativo:
;;(timer 432)
;;EN-ROJO

;; Ejemplo que genera error:
;;(timer '(100))
;;"Error: El tiempo Unix debe ser un numero entero" 

;;-----------------------
;;Prubas Requerimiento 3 
;;-----------------------
;; Resultado Normal:
;;(informe 1718254800 'en-rojo 'en-verde)
;;Tiempo 1718254800: la luz ha cambiado de EN-ROJO a EN-VERDE

;; Camino alternativo:
;; (informe 1781329260 'en-verde 'en-amarillo)
;; Tiempo 1781329260: la luz ha cambiado de EN-VERDE a EN-AMARILLO

;; Ejemplo que genera error:
;; (informe 1781329200 'en-azul 'en-rojo)
;;"Error: Tipos de datos invalidos para auditoria" 

;;------------------------
;;Pruebas Requerimiento 4
;;------------------------
;; 4.a funcion duracion-ciclo 
  
;; Resultado Normal:
;;(duracion-ciclo 100)
;;(100 RANGO-OPTIMO)

;;Camino alternativo:
;;(duracion-ciclo 20)
;;(20 DURACION-BAJA)

;; Ejemplo que genera error: 
;; (duracion-ciclo -5)
;;"Error: La duracion ingresada debe ser un numero positivo"

;; 4.b funcion recomendacion-ciclo
;; Resultado Normal:
;;(recomendacion-ciclo 90)
;;"Recomendacion: Ciclo optimo"
  
;; Camino alternativo:
;;(recomendacion-ciclo 200) 
;;"Recomendacion: Reducir tiempo"

;; Ejemplo que genera error:
;; (recomendacion-ciclo sesenta)
;; "Error: La duracion ingresada debe ser un numero positivo"

;;------------------------
;;Pruebas Requerimiento 5
;;------------------------
;; Resultado Normal:
;;(ciclos-por-tiempo 15)
;; 4
;;Explicación: 15 minutos equivalen a 900 segundos. 900 / 216 = 4.16. La función truncate devuelve solo la parte entera 4
  
;; Camino alternativo:
;;(ciclos-por-tiempo 30)
;; 8

;; Ejemplo que genera error:
;; (ciclos-por-tiempo -1)
;; "Error: los minutos deben ser un numero positivo"

;;-------------------------
;;Pruebas Requerimiento 6
;;-------------------------
;;Resultado normal:
;;(informe-distribucion '((rojo 90) (amarillo 6) (verde 120))) 
;; Resultado: ;; ((ROJO 41.666668) (AMARILLO 2.777777) (VERDE 55.555557))

;; Camino alternativo:
;;(informe-distribucion '((rojo 90) (amarillo 6) (verde 120) (rojo 90) (amarillo 6) (verde 120))) 
;; Resultado:((ROJO 41.666668)(AMARILLO 2.777777)(VERDE 55.555557)) 

  
;; Camino alternativo:
;; (informe-distribucion '((rojo 100) (amarillo 10) (verde 50)))
;; Resultado:((ROJO 62.5) (AMARILLO 6.25) (VERDE 31.25))
  
;; Camino alternativo:solo rojo
;;(informe-distribucion '((rojo 90) (rojo 90) (rojo 90)))
;; Resultado: ((ROJO 100.0) (AMARILLO 0.0) (VERDE 0.0))
 
  
;; Camino alternativo:Solo verde  
;; Camino alternativo:nforme-distribucion '((verde 120) (verde 120)))
;; Resultado:  ((ROJO 0.0) (AMARILLO 0.0) (VERDE 100.0)) 
  
;; Historial vacío
;;(informe-distribucion '()) 
;; Resultado: ((ROJO 0.0) (AMARILLO 0.0) (VERDE 0.0))

;; Ejemplo que genera error
;;(informe-distribucion 25)
;; Resultado:Error, porque la función espera una lista que represente el historial.



;;FASE 2: Requerimiento 3 con uso de local-time 
  
(ql:quickload "local-time")

(defun color-correcto (color)
  (or (equal color 'en-rojo)
      (equal color 'en-amarillo)
      (equal color 'en-verde)))

(defun registrar-cambio (tiempo color-viejo color-nuevo)
  (if (and (integerp tiempo)
           (color-correcto color-viejo)
           (color-correcto color-nuevo))
      (let ((fecha (local-time:format-timestring nil 
                      (local-time:unix-to-timestamp tiempo)
                      :format '((:year 4) "-" (:month 2) "-" (:day 2) " " 
                                (:hour 2) ":" (:min 2) ":" (:sec 2)))))
        (format t "Fecha [~a]: la luz cambio de ~a a ~a~%" fecha color-viejo color-nuevo))
      "Error: datos invalidos"))
 
