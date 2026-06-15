
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

(defun transicion (color-actual cambiar-a)
  (if (es-transicion-permitida color-actual cambiar-a)
      (list color-actual
            (accion-color cambiar-a))
      (list color-actual
            'accion-por-defecto)))

;; ============================================================
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO
;; ============================================================

(defun ubicar-fase (resto)    ;; ciclo ajustado a 225 segundos para incluir las fases de intermitencia de 3 segundos cada una.
  (cond
    ((< resto 90) 'en-rojo)
    ((< resto 93) 'amarillo-intermitente)
    ((< resto 213) 'en-verde)
    ((< resto 216) 'amarillo-intermitente)
    ((< resto 222) 'en-amarillo)
    ((< resto 225) 'amarillo-intermitente)
    (t 'fase-invalida)))

(defun timer (tiempo-unix)  ;; (mod tiempo-unix 225) asegura que el ciclo se repita cada 225 segundos.
  (if (integerp tiempo-unix)
      (ubicar-fase 
        (mod tiempo-unix 225))
      'error-tiempo-no-entero))


;; ============================================================
;; REQUERIMIENTO 3: SISTEMA DE AUDITORÍA. (Extensión 2 de la Iteración 2)
;; ============================================================

(defun es-color-valido (color)
  (or
   (equal color 'en-rojo)
   (equal color 'en-verde)
   (equal color 'en-amarillo)
   (equal color 'amarillo-intermitente)))


(defun informe (tiempo-epoch color-anterior color-nuevo)
  (if (and (integerp tiempo-epoch) ;; validación de datos
           (es-color-valido color-anterior)
           (es-color-valido color-nuevo))

      (with-open-file ;; gestiona la apertura y cierre automático
          (archivo "informe-ejecucion-semaforo.txt" 
                  :direction :output ;; define que vamos a escribir
                  :if-exists :append ;; evita sobrescribir
                  :if-does-not-exist :create);; crea el archivo automáticamente si no existe

        (format archivo "Fecha ~a: cambio de ~a a ~a~%" tiempo-epoch color-anterior color-nuevo))
  'error-datos-invalidos)) ;; si la validación falla, devuelve el error sin intentar acceder al disco


