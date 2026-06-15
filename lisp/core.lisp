
#| FASE 1:  El problema de los 3 focos. 

Introducción: Con el equipo decidimos comentar la fase 1 con los requerimientos del 1 al 7 
(Integramos al nucleo la fase 2, primero esta la función original y después la función modificada).
Luego colocamos la iteración 2 con sus extensiones.


(ql:quickload "local-time") ;; cargamos librería "local-time".

;; ================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICION
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
;; NATURALEZA: Pura
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
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMATICO
;; ============================================================

;; ============================================================
;; FUNCIÓN: ubicar-fase
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional Multiple  
;; IMPACTO: No destructiva
;; ============================================================

(defun ubicar-fase (resto)
  (cond 
    ((< resto 90) 'en-rojo)
    ((< resto 210) 'en-verde) 
    (t 'en-amarillo)))

;; ============================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Composicion Funcional 
;; IMPACTO: No destructiva
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
;; ESTRATEGIA: Funcion Predicado
;; IMPACTO: No destructiva
;; ============================================================

;; (defun es-color-valido (color)
;; (or (equal color 'en-rojo)
;;       (equal color 'en-amarillo)
;;       (equal color 'en-verde)))

;; ============================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Imprime en pantalla)
;; ESTRATEGIA: Secuencial con validacion 
;; IMPACTO: No destructiva
;; ============================================================

;; (defun informe (tiempo-epoch color-anterior color-nuevo)
;;   (if (and (integerp tiempo-epoch) 
;;            (es-color-valido color-anterior) 
;;           (es-color-valido color-nuevo))
;;       (format t "Tiempo ~a: la luz ha cambiado de ~a a ~a~%" 
;;               tiempo-epoch color-anterior color-nuevo)   
;;        "Error: Tipos de datos invalidos para auditoria"))


;; ============================================================
;; FASE 2: Requerimiento 3 con uso de local-time 
;; ============================================================

(defun color-correcto (color)
  (or (equal color 'en-rojo)
      (equal color 'en-amarillo)
      (equal color 'en-verde)))

(defun registrar-cambio (tiempo color-viejo color-nuevo)
  (if (and (integerp tiempo)
           (color-correcto color-viejo)
           (color-correcto color-nuevo))
      
      (let ((fecha (local-time:format-timestring ;;  declaramos una variable llamada Fecha y llamamos a la función format-timestring de local-time
                    nil ;;  significa que no lo mande a un archivo, sino que devuelva texto 
                      (local-time:unix-to-timestamp tiempo) ;;  Convertimos el entero tiempo en un objeto timestamp
                      :format '((:year 4) "-" (:month 2) "-" (:day 2) " " 
                                (:hour 2) ":" (:min 2) ":" (:sec 2)))))
           
        (format t "Tiempo [~a]: la luz cambio de ~a a ~a~%" fecha color-viejo color-nuevo)) ;; Ejecutamos la impresión
      
      "Error: datos invalidos")) ;; Si el tiempo no era un entero, o algún color no existía, la función devuelve Error
 
;; ============================================================
;; REQUERIMIENTO 4: ANALISIS DE CICLOS SEMAFORICOS
;; ============================================================

;; FUNCIÓN 4-a: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional Multiple
;; IMPACTO: No destructiva
;; ============================================================
(defun duracion-ciclo (segundos)
  (cond
    ((or (not (numberp segundos)) (<= segundos 0)) "Error: La duracion ingresada debe ser un numero positivo")
    ((and (>= segundos 35) (<= segundos 150)) (list segundos 'Rango-Optimo))
     ((< segundos 35) (list segundos 'Duracion-baja))
     (t (list segundos 'Duracion-alta))))

;; ============================================================
;; FUNCIÓN 4-b: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional Multiple
;; IMPACTO: No destructiva
;; ============================================================
(defun recomendacion-ciclo (duracion)
  (cond
    ((or (not (numberp duracion)) (<= duracion 0)) "Error: La duracion ingresada debe ser un numero positivo")
    ((and (>= duracion 35) (<= duracion 150))"Recomendacion: Ciclo optimo")
    ((< duracion 35) "Recomendacion: Aumentar tiempo")
    (t "Recomendacion: Reducir tiempo")))

;; ============================================================
;; REQUERIMIENTO 5: PLANIFICACION TEMPORAL
;; ============================================================

;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional Simple con TRUNCATE
;; IMPACTO: No destructiva
;; ============================================================

(defun ciclos-por-tiempo (minutos)
  (if (and (numberp minutos) (>= minutos 0))
      (truncate (/ (* minutos 60) 216))
      "Error: los minutos deben ser un numero positivo"))


;; ============================================================
;; REQUERIMIENTO 6: INFORME DE DISTRIBUCION TEMPORAL
;; ============================================================

;; 1. Funciones base para calcular el ciclo estandar
(defun ciclos-hora ()
  "Calcula la cantidad de ciclos enteros que entran en 1 hora (3600s)"
  (truncate 3600 216))
;; ============================================================
;; FUNCIÓN:ciclos-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con TRUNCATE
;; IMPACTO: No destructiva
;; ============================================================

(defun resto-hora ()
  "Calcula los segundos sobrantes tras completar los ciclos enteros"
  (mod 3600 216))
;; ============================================================
;; FUNCIÓN:resto-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con MOD
;; IMPACTO: No destructiva
;; ============================================================

;; 2. Calculo exacto de segundos por color distribuyendo el resto con MIN/MAX
(defun segundos-rojo-hora ()
  "90 segundos base por ciclo + el remanente (hasta 90s máximos)"
  (+ (* (ciclos-hora) 90)
     (min (resto-hora) 90)))
;; ============================================================
;; FUNCIÓN:segundos-rojo-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con operaciones aritméticas y MIN
;; IMPACTO: No destructiva
;; ============================================================

(defun segundos-verde-hora ()
  "120 segundos base por ciclo + remanente tras restar la fase roja"
  (+ (* (ciclos-hora) 120)
     (min (max (- (resto-hora) 90) 0) 120)))
;; ============================================================
;; FUNCIÓN:segundos-verde-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con operaciones aritméticas, MIN y MAX
;; IMPACTO: No destructiva
;; ============================================================

(defun segundos-amarillo-hora ()
  "6 segundos base por ciclo + remanente tras restar rojo y verde"
  (+ (* (ciclos-hora) 6)
     (min (max (- (resto-hora) 210) 0) 6)))
;; ============================================================
;; FUNCIÓN:segundos-amarillo-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con operaciones aritméticas, MIN y MAX
;; IMPACTO: No destructiva
;; ============================================================

;; 3. calcular el porcentaje final
(defun calcular-porcentaje (segundos)
  "Convierte los segundos totales de un color en porcentaje sobre 1 hora"
  (float (* (/ segundos 3600) 100)))
;; ============================================================
;; FUNCIÓN:calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional con operaciones aritméticas y FLOAT
;; IMPACTO: No destructiva
;; ============================================================

;; 4. Funcion principal
(defun informe-distribucion-hora ()
  "Genera el informe porcentual de los tres colores en 1 hora"
  (list
   (list 'rojo (calcular-porcentaje (segundos-rojo-hora)))
   (list 'verde (calcular-porcentaje (segundos-verde-hora)))
   (list 'amarillo (calcular-porcentaje (segundos-amarillo-hora)))))
;; ============================================================
;; FUNCIÓN:informe-distribucion-hora
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional mediante LIST y llamadas a funciones auxiliares
;; IMPACTO: No destructiva
;; ============================================================


;;======================================
;; REQUERIMIENTO 7: Casos de Prueba
;;======================================

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

;;-------------------------------
;;Prubas Fase 2 Requerimiento 3 
;;-------------------------------
;; Resultado Normal:
;; (registrar-cambio 1781524800 'en-rojo 'en-verde)
;; Tiempo [2026-06-15 12:00:00]: la luz cambio de EN-ROJO a EN-VERDE
;; NIL

;; Camino alternativo:
;; (registrar-cambio 0 'en-verde 'en-amarillo)
;; Tiempo [1970-01-01 00:00:00]: la luz cambio de EN-VERDE a EN-AMARILLO
;; NIL

;; Ejemplo que genera error:
;; (registrar-cambio 1781524800 'en-rojo 'en-azul)
;; "Error: datos invalidos"

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
;; Resultado Normal:
;;(informe-distribucion-hora)
;;devuelve una lista con el porcentaje correspondiente a cada color durante una hora.

;; Camino alternativo:
;;(resto hora)
;;144

;; Ejemplo que genera error:
;;(informe-distribucion-hora 5)
;;Error por cantidad incorrecta de argumentos, ya que la función no recibe parámetros.

|#


;;  ITERACIÓN 2  EXTENSIONES 1 Y 2

;; ============================================================
;; REQUERIMIENTO 1: ESTADOS DE TRANSICIÓN
;; ============================================================
;; ============================================================
;; FUNCIÓN: es-transicion-permitida
;; NATURALEZA: Pura 
;; ESTRATEGIA: Evaluación booleana mediante OR y AND
;; IMPACTO: No destructiva
;; ============================================================

(defun es-transicion-permitida (color-actual cambiar-a)
  (or
   (and (equal color-actual 'en-rojo) (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente) (equal cambiar-a 'en-verde))
   (and (equal color-actual 'en-verde) (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente) (equal cambiar-a 'en-amarillo))
   (and (equal color-actual 'en-amarillo) (equal cambiar-a 'amarillo-intermitente))
   (and (equal color-actual 'amarillo-intermitente)(equal cambiar-a 'en-rojo))))

;; ============================================================
;; FUNCIÓN: accion-color
;; NATURALEZA: Pura 
;; ESTRATEGIA: Evaluación booleana mediante OR y AND
;; IMPACTO: No destructiva
;; ============================================================

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
      (list color-actual (accion-color cambiar-a))
      (list color-actual 'accion-por-defecto)))

;; ============================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO
;; ============================================================
;; ============================================================
;; FUNCIÓN: ubicar-fase
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional múltiple
;; IMPACTO: No destructiva
;; ============================================================

(defun ubicar-fase (resto) ;; ciclo ajustado a 225 segundos para incluir las fases de intermitencia de 3 segundos cada una.
  (cond
    ((< resto 90) 'en-rojo)
    ((< resto 93) 'amarillo-intermitente)
    ((< resto 213) 'en-verde)
    ((< resto 216) 'amarillo-intermitente)
    ((< resto 222) 'en-amarillo)
    ((< resto 225) 'amarillo-intermitente)
    (t 'fase-invalida)))

;; ============================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional simple
;; IMPACTO: No destructiva
;; ============================================================

(defun timer (tiempo-unix) 
  (if (integerp tiempo-unix)
      (ubicar-fase  
        (mod tiempo-unix 225)) ;; El uso de MOD asegura que el ciclo se reinicie cada 225 segundos
      'error-tiempo-no-entero))

;; ===========================================================
;; REQUERIMIENTO 3: SISTEMA DE AUDITORÍA. (Extension 2 de la Iteracion 2)
;; ============================================================
;; ============================================================
;; FUNCIÓN: es-color-valido
;; NATURALEZA: Pura 
;; ESTRATEGIA: Evaluación booleana mediante OR
;; IMPACTO: No destructiva
;; ============================================================

(defun es-color-valido (color)
  (or
   (equal color 'en-rojo)
   (equal color 'en-verde)
   (equal color 'en-amarillo)
   (equal color 'amarillo-intermitente)))

;; ============================================================
;; FUNCIÓN: informe
;; NATURALEZA: Pura 
;; ESTRATEGIA: Condicional simple con validación mediante WITH-OPEN-FILE
;; IMPACTO: No destructiva
;; ============================================================

(defun informe (tiempo-epoch color-anterior color-nuevo)
  (if (and (integerp tiempo-epoch) ;; validacion de datos
           (es-color-valido color-anterior)
           (es-color-valido color-nuevo))

      (with-open-file ;; se utiliza para la aperturta y cierre autoimatico del archivo te texto 
          (archivo "informe-ejecucion-semaforo.txt" 
                  :direction :output ;; modo escritura, define lo que vamos a escribir
                  :if-exists :append ;; agrega datos al final sin sobrescribir
                  :if-does-not-exist :create);; crea el archivo si no existe

        (format archivo "Tiempo ~a: cambio de ~a a ~a~%" tiempo-epoch color-anterior color-nuevo))
  'error-datos-invalidos))





;; ============================================================
;; REQUERIMIENTO 6: INFORME DE DISTRIBUCIÓN TEMPORAL
;; ============================================================

;;========================================================
;; FUNCIÓN:ciclos-hora
;; NATURALEZA:  Pura
;; ESTRATEGIA : Composicion funcional mediante truncate
;; IMPACTO: no destructiva 
;;=========================================================
;; 1. Funciones base para calcular el ciclo estándar de 225 segundos
(defun ciclos-hora ()
  "Calcula la cantidad de ciclos enteros que entran en 1 hora (3600s)"
  (truncate 3600 225))


;;========================================================
;; FUNCIÓN: resto-hora
;; NATURALEZA:pura
;; ESTRATEGIA : composicion funcionalmediante MOD
;; IMPACTO: no destructiva
;;=========================================================
(defun resto-hora ()
  "Calcula los segundos sobrantes tras completar los ciclos enteros"
  (mod 3600 225))


;;========================================================
;; FUNCIÓN: segundos-rojo-hora 
;; NATURALEZA: pura
;; ESTRATEGIA : composicion funcional utilizando operaciones aritmeticas y MIN
;; IMPACTO: no destructiva 
;;=========================================================
;; 2. Cálculo exacto de segundos por color distribuyendo el resto con MIN/MAX
(defun segundos-rojo-hora ()
  "90 segundos base por ciclo + el remanente (hasta 90s máximos)"
  (+ (* (ciclos-hora) 90)
     (min (resto-hora) 90)))


;;========================================================
;; FUNCIÓN: segundos-intermitente-hora
;; NATURALEZA:Pura
;; ESTRATEGIA: Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO: no destructiva 
;;=========================================================
(defun segundos-intermitente-hora ()
  "9 segundos base por ciclo (3+3+3) + el remanente"
  (+ (* (ciclos-hora) 9)
     (min (max (- (resto-hora) 90) 0) 9)))


;;========================================================
;; FUNCIÓN:segundos-verde-hora 
;; NATURALEZA:Pura
;; ESTRATEGIA:Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO:No destructiva
;;=========================================================
(defun segundos-verde-hora ()
  "120 segundos base por ciclo + remanente tras restar rojo e intermitencia"
  (+ (* (ciclos-hora) 120)
     (min (max (- (resto-hora) 99) 0) 120)))


;;========================================================
;; FUNCIÓN: segundos-amarillo-hora
;; NATURALEZA:Pura 
;; ESTRATEGIA: Composición funcional utilizando operaciones aritméticas, MIN y MAX
;; IMPACTO:No destructiva 
;;=========================================================
(defun segundos-amarillo-hora ()
  "6 segundos base por ciclo + remanente tras restar rojo, intermitencia y verde"
  (+ (* (ciclos-hora) 6)
     (min (max (- (resto-hora) 219) 0) 6)))


;;========================================================
;; FUNCIÓN:  calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional mediante operaciones aritméticas y conversión con FLOAT. 
;; IMPACTO: No destructiva
;;=========================================================
;; 3. Función auxiliar para calcular el porcentaje final
(defun calcular-porcentaje (segundos)
  "Convierte los segundos totales de un color en porcentaje sobre 1 hora"
  (float (* (/ segundos 3600) 100)))


;;==========================================================================================================
;; FUNCIÓN: informe-distribucion-hora 
;; NATURALEZA: Pura
;; ESTRATEGIA:Composición funcional mediante construcción de listas (LIST) y llamadas a funciones auxiliares.
;; IMPACTO: No destructiva.
;;==========================================================================================================
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


