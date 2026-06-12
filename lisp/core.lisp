
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



;; REQUERIMIENTO 4: ANÁLISIS DE EFICIENCIA Y PSICOLOGÍA
;; ============================================================

;; ============================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Retorna siempre el mismo análisis para una densidad dada)
;; ESTRATEGIA DE CONTROL: Condicional con Let*
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================

(defun duracion-ciclo (densidad-trafico)

(let* ((t-rojo 30)
    (t-amarillo 5)
    (t-verde (if (equal densidad-trafico 'ALTA) 45 20))
    (total (+ t-rojo t-verde t-amarillo)))
    (list total
        (cond
        ;; Rango óptimo
        ((and (>= total 35) (<= total 150)) 'RANGO-OPTIMO)
        ((< total 35) 'DURACION-SUBOPTIMA-BAJA)
        (t 'DURACION-SUBOPTIMA-ALTA)))))


;;=================================================
;;REQUERIMIENTO 6: Informe de Distribución Temporal 
;;=================================================



;;=============================================================
;;FUNCION:calcular-porcentaje
;;NATURALEZA:Pura(Calculo matematico sin efectos colaterales)
;;ESTRATEGIA:Condicional plana
;;IMPACTO:No destructiva 
;;=============================================================

(defun calcular-porcentaje(tiempo-color total)
(if(> total 0)
   (*(/tiempo-color total)100.0)
   0))
