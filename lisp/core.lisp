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
      (format t "Fecha ~a: la luz ha cambiado de ~a a ~a~%" 
              tiempo-epoch color-anterior color-nuevo)   
       "Error: Tipos de datos invalidos para auditoria"))



;; ============================================================
;; REQUERIMIENTO 4: ANÁLISIS DE CICLOS SEMAFÓRICOS
;; ============================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA DE CONTROL: Let
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun duracion-ciclo (densidad-trafico)
 
  (let ((total (+ 30 5 (if (equal densidad-trafico 'ALTA) 45 20))))
    (list total 
          (cond 
            ((and (>= total 35) (<= total 150)) 'RANGO-OPTIMO)
            ((< total 35) 'DURACION-SUBOPTIMA-BAJA)
            (t 'DURACION-SUBOPTIMA-ALTA)))))

;; ============================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Condicional (cond)
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35) "RECOMENDACION: Incrementar tiempos; ciclos cortos fatigan al usuario.")
    ((> duracion 150) "RECOMENDACION: Reducir tiempos; esperas largas fomentan infracciones.")
    (t "RECOMENDACION: Ciclo eficiente segun estandares de ingenieria de trafico.")))

;; ============================================================
;; FUNCIÓN: planificar-coordinacion
;; NATURALEZA: Pura 
;; ESTRATEGIA DE CONTROL: Let anidado + TRUNCATE
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun planificar-coordinacion (tiempo-total densidad)
  (let ((analisis (duracion-ciclo densidad)))
    (let ((duracion (car analisis)))
       (list (list 'TOTAL-CICLOS-ESTIMADOS (truncate (/ tiempo-total duracion)))
             (list 'DURACION-UN-CICLO duracion)
             (list 'ESTADO-PSICOLOGICO (cadr analisis))
             (list 'INFORME-TECNICO (recomendacion-ciclo duracion))))))

;; ============================================================
;; REQUERIMIENTO 5: PLANIFICACIÓN TEMPORAL
;; ============================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Retorna el cálculo matemático sin modificar el entorno)
;; ESTRATEGIA DE CONTROL: Composición Funcional con TRUNCATE
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun ciclos-por-tiempo (minutos densidad)
  
  (if (and (numberp minutos) (or (equal densidad 'ALTA) (equal densidad 'BAJA)))
      (let ((segundos-totales (* minutos 60))
            (duracion-ciclo (+ 30 5 (if (equal densidad 'ALTA) 45 20))))
        
        (truncate (/ segundos-totales duracion-ciclo)))
      "Error: Argumentos invalidos (minutos debe ser numero y densidad ALTA o BAJA)"))



;;=================================================
;;REQUERIMIENTO 6: Informe de Distribución Temporal 
;;=================================================




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
;;Resultado Normal: 
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


;;------------------------
;;Pruebas Requerimiento 5
;;------------------------

;;-------------------------
;;Pruebas Requerimiento 6
;;-------------------------
  

  ;; CASO 1: Funcionamiento normal 
  ;;(informe-distribucion '((rojo 90) (amarillo 6) (verde 120))) 
  
  ;; Resultado esperado:
  ;; ((ROJO 41.666668) 
  ;; (AMARILLO 2.777777) 
  ;; (VERDE 55.555557))



  ;; CASO 2: Dos ciclos completos
  ;;(informe-distribucion 
  ;;'((rojo 90) (amarillo 6) (verde 120) (rojo 90) (amarillo 6) (verde 120))) 
  
  ;; Resultado esperado: 
  ;; ((ROJO 41.666668) 
  ;; (AMARILLO 2.777777) 
  ;; (VERDE 55.555557))


  
  
  ;; CASO 3: Predomina el rojo
 ;; (informe-distribucion '((rojo 100) (amarillo 10) (verde 50)))
  
  ;; Resultado esperado: 
  ;; ((ROJO 62.5) 
  ;; (AMARILLO 6.25) 
  ;; (VERDE 31.25))

  
  
  ;; CASO 4: Solo rojo
  ;;(informe-distribucion '((rojo 90) (rojo 90) (rojo 90)))

  ;; Resultado esperado: 
  ;; ((ROJO 100.0) 
  ;; (AMARILLO 0.0)
  ;; (VERDE 0.0))

  

  ;; CASO 5: Solo verde
  ;;(informe-distribucion '((verde 120) (verde 120)))

  ;; Resultado esperado: 
  ;; ((ROJO 0.0) 
  ;; (AMARILLO 0.0) 
  ;; (VERDE 100.0))
  

  ;; CASO 6: Historial vacío
  ;;(informe-distribucion '()) 
  
  ;; Resultado esperado: 
  ;; ((ROJO 0.0) 
  ;; (AMARILLO 0.0) 
  ;; (VERDE 0.0))

  
  ;; CASO 7: Ejemplo que genera error
  ;;(informe-distribucion 25)
  ;; Resultado esperado:Error, porque la función espera una lista que represente el historial.

  
  ;; CASO 8: Formato incorrecto









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
 
