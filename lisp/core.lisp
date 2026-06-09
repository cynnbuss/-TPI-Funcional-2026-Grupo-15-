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

