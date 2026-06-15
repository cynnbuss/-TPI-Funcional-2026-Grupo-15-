
;;  ITERACIÓN 2  EXTENSIONES 1 Y 2

;; ============================================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICIÓN
;; ============================================================

(defun es-transicion-permitida (color-actual cambiar-a)
  (or
   (and (equal color-actual 'en-rojo)
        (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente)
        (equal cambiar-a 'en-verde))
   (and (equal color-actual 'en-verde)
        (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente)
        (equal cambiar-a 'en-amarillo))
   (and (equal color-actual 'en-amarillo)
        (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente)
        (equal cambiar-a 'en-rojo))))


(defun accion-color (cambiar-a)
  (or
    (and(equal cambiar-a 'en-rojo) 'cambiar-a-rojo)
    (and(equal cambiar-a 'en-verde) 'cambiar-a-verde)
    (and(equal cambiar-a 'en-amarillo) 'cambiar-a-amarillo)
    (and(equal cambiar-a 'amarillo-intermitente)'cambiar-a-amarillo-intermitente)))

;; ============================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (solo devuelve una lista)
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;; ============================================================


(defun transicion (color-actual cambiar-a)
  (if (es-transicion-permitida color-actual cambiar-a)
      (list color-actual
            (accion-color cambiar-a))
      (list color-actual
            'accion-por-defecto)))

;; ============================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO
;; ============================================================

 ;; ciclo ajustado a 225 segundos para incluir las fases de intermitencia de 3 segundos cada una.

(defun ubicar-fase (resto)
  (cond
    ((< resto 90) 'en-rojo)
    ((< resto 93) 'amarillo-intermitente)
    ((< resto 213) 'en-verde)
    ((< resto 216) 'amarillo-intermitente)
    ((< resto 222) 'en-amarillo)
    ((< resto 225) 'amarillo-intermitente)
    (t 'fase-invalida)))

;; (mod tiempo-unix 225) asegura que el ciclo se repita cada 225 segundos.

(defun timer (tiempo-unix) 
  (if (integerp tiempo-unix)
      (ubicar-fase 
        (mod tiempo-unix 225))
      'error-tiempo-no-entero))


;; ============================================================
;; REQUERIMIENTO 3: SISTEMA DE AUDITORÍA. (Extension 2 de la Iteracion 2)
;; ============================================================

(defun es-color-valido (color)
  (or
   (equal color 'en-rojo)
   (equal color 'en-verde)
   (equal color 'en-amarillo)
   (equal color 'amarillo-intermitente)))

;; validacion de datos
(defun informe (tiempo-epoch color-anterior color-nuevo)
  (if (and (integerp tiempo-epoch)
           (es-color-valido color-anterior)
           (es-color-valido color-nuevo))

      (with-open-file ;;sobre la aperturta y cierre autoimatico de nuestro archivo 
          (archivo "informe-ejecucion-semaforo.txt" 
                  :direction :output ;; define que vamos a escribir
                  :if-exists :append ;; evita sobrescribir
                  :if-does-not-exist :create)

        (format archivo "Fecha ~a: cambio de ~a a ~a~%" tiempo-epoch color-anterior color-nuevo))
  'error-datos-invalidos))

;; ============================================================
;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; ============================================================

;; ============================================================
;; FUNCIÓN 4-a: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional MUltiple
;; IMPACTO: No destructiva
;; ============================================================

(defun duracion-ciclo (segundos)
  (cond
    ((or (not (numberp segundos))
         (<= segundos 0))
     "Error: La duracion ingresada debe ser un numero positivo")

    ((and (>= segundos 35)
          (<= segundos 150))
     (list segundos 'Rango-Optimo))

    ((< segundos 35)
     (list segundos 'Duracion-baja))

    (t
     (list segundos 'Duracion-alta))))


;; ============================================================
;; FUNCION 4-b: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional Multiple
;; IMPACTO: No destructiva
;; ============================================================

(defun recomendacion-ciclo (duracion)
  (cond
    ((or (not (numberp duracion))
         (<= duracion 0))
     "Error: La duracion ingresada debe ser un numero positivo")

    ((and (>= duracion 35)
          (<= duracion 150))
     "Recomendacion: Ciclo optimo")

    ((< duracion 35)
     "Recomendacion: Aumentar tiempo")

    (t
     "Recomendacion: Reducir tiempo")))


;; ============================================================
;; REQUERIMIENTO 5: PLANIFICACION TEMPORAL
;; ============================================================

;; ============================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional Simple con TRUNCATE
;; IMPACTO: No destructiva
;; ============================================================

(defun ciclos-por-tiempo (minutos)
  (if (and (numberp minutos)
           (>= minutos 0))
      (truncate
       (/ (* minutos 60)
          225))
      "Error: los minutos deben ser un numero positivo"))




;; ============================================================
;; REQUERIMIENTO 6: INFORME DE DISTRIBUCIÓN TEMPORAL
;; ============================================================

;; 1. Funciones base para calcular el ciclo estándar de 225 segundos
(defun ciclos-hora ()
  "Calcula la cantidad de ciclos enteros que entran en 1 hora (3600s)"
  (truncate 3600 225))
;;========================================================
;; FUNCIÓN:ciclos-hora
;; NATURALEZA:  Pura
;; ESTRATEGIA : Composicion funcional mediante truncate
;; IMPACTO: no destructiva 
;;=========================================================


(defun resto-hora ()
  "Calcula los segundos sobrantes tras completar los ciclos enteros"
  (mod 3600 225))
;;========================================================
;; FUNCIÓN: resto-hora
;; NATURALEZA:pura
;; ESTRATEGIA : composicion funcionalmediante MOD
;; IMPACTO: no destructiva
;;=========================================================


;; 2. Cálculo exacto de segundos por color distribuyendo el resto con MIN/MAX
(defun segundos-rojo-hora ()
  "90 segundos base por ciclo + el remanente (hasta 90s máximos)"
  (+ (* (ciclos-hora) 90)
     (min (resto-hora) 90)))
;;========================================================
;; FUNCIÓN: segundos-rojo-hora 
;; NATURALEZA: pura
;; ESTRATEGIA : composicion funcional utilizando operaciones aritmeticas y MIN
;; IMPACTO: no destructiva 
;;=========================================================


(defun segundos-intermitente-hora ()
  "9 segundos base por ciclo (3+3+3) + el remanente"
  (+ (* (ciclos-hora) 9)
     (min (max (- (resto-hora) 90) 0) 9)))
;;========================================================
;; FUNCIÓN: segundos-intermitente-hora
;; NATURALEZA:Pura
;; ESTRATEGIA: Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO: no destructiva 
;;=========================================================


(defun segundos-verde-hora ()
  "120 segundos base por ciclo + remanente tras restar rojo e intermitencia"
  (+ (* (ciclos-hora) 120)
     (min (max (- (resto-hora) 99) 0) 120)))
;;========================================================
;; FUNCIÓN:segundos-verde-hora 
;; NATURALEZA:Pura
;; ESTRATEGIA:Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO:No destructiva
;;=========================================================


(defun segundos-amarillo-hora ()
  "6 segundos base por ciclo + remanente tras restar rojo, intermitencia y verde"
  (+ (* (ciclos-hora) 6)
     (min (max (- (resto-hora) 219) 0) 6)))
;;========================================================
;; FUNCIÓN: segundos-amarillo-hora
;; NATURALEZA:Pura 
;; ESTRATEGIA: Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO:No destructiva 
;;=========================================================


;; 3. Función auxiliar para calcular el porcentaje final

(defun calcular-porcentaje (segundos)
  "Convierte los segundos totales de un color en porcentaje sobre 1 hora"
  (float (* (/ segundos 3600) 100)))
;;========================================================
;; FUNCIÓN:  calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional mediante operaciones aritméticas y conversión con FLOAT. 
;; IMPACTO: No destructiva
;;=========================================================


;; 4. Función principal requerida por el ejercicio
(defun informe-distribucion-hora ()
  "Genera el informe porcentual de los colores en 1 hora"
  (list
   (list 'rojo
         (calcular-porcentaje
          (segundos-rojo-hora)))

   (list 'amarillo-intermitente
         (calcular-porcentaje
          (segundos-intermitente-hora)))

   (list 'verde
         (calcular-porcentaje
          (segundos-verde-hora)))

   (list 'amarillo
         (calcular-porcentaje
          (segundos-amarillo-hora)))))
;;========================================================
;; FUNCIÓN: informe-distribucion-hora 
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional mediante construcción de listas (LIST) y llamadas a funciones auxiliares.
;; IMPACTO: No destructiva.
;;=========================================================




