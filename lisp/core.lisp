
;; Carga de librerias externas (Fase 2)
(ql:quickload "local-time")

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
;; ESTRATEGIA DE CONTROL: Función Predicado 
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(defun ubicar-fase (resto)
  (or (and (< resto 90) 'en-rojo)
      (and (>= resto 90) (< resto 210) 'en-verde)
      'en-amarillo))

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
              (local-time:unix-to-timestamp tiempo-epoch)  ; Uso de libreria local-time  
               color-anterior
               color-nuevo)     
      "Error: Tipos de datos invalidos"))



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
  (list 
   (list 'rojo (porcentaje-color historial 'rojo)) 
   (list 'amarillo (porcentaje-color historial 'amarillo)) 
   (list 'verde (porcentaje-color historial 'verde))))
