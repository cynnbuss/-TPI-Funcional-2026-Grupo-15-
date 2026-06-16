;Scheme es un dialecto minimalista del lenguaje Lisp 
;;diseñado en los años 70 en el MIT por Guy L. Steele y Gerald Jay Sussman. 
;;A diferencia de Common Lisp, que es un lenguaje extenso y multiparadigma 
;;Scheme se centra en la elegancia conceptual y la simplicidad sintáctica. 

;;Industrias y Áreas de Uso:

;;Se utiliza principalmente en el ámbito académico y de investigación 
;;(es el lenguaje base del famoso libro SICP), 
;;en sistemas embebidos, y como lenguaje de extensión o scripting para otras aplicaciones.

;;Empresas conocidas:
;; Google: Fue utilizado históricamente en el motor de App Inventor
;;Cisco (utiliza Scheme en procesos de control de calidad y herramientas internas), ;;Google (históricamente en App Inventor) 
;; y la comunidad de software libre a través de GIMP 
;;(utiliza una variante llamada TinyScheme para sus plugins Script-Fu). 

;; ============================================================
;; FUNCIÓN: es-transicion-permitida?
;; NATURALEZA: Pura (Retorna un booleano sin efectos secundarios)
;; ESTRATEGIA: Función predicado (Usa la convención '?' de Scheme)
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(define (es-transicion-permitida? color-actual cambiar-a)
  (or (and (equal? color-actual 'en-rojo) (equal? cambiar-a 'verde))
      (and (equal? color-actual 'en-verde) (equal? cambiar-a 'amarillo))
      (and (equal? color-actual 'en-amarillo) (equal? cambiar-a 'rojo))))

;; ============================================================
;; FUNCIÓN: accion-color
;; NATURALEZA: Pura
;; ESTRATEGIA: Evaluación booleana (Selección por cortocircuito)
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(define (accion-color cambiar-a)
  (or (and (equal? cambiar-a 'rojo) "cambiar-a-rojo")
      (and (equal? cambiar-a 'amarillo) "cambiar-a-amarillo")
      (and (equal? cambiar-a 'verde) "cambiar-a-verde")))

;; ============================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (Retorna un estado sin efectos secundarios)
;; ESTRATEGIA: Condicional Simple (Adaptada de Common Lisp)
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(define (transicion color-actual cambiar-a)
  ;; En Scheme los predicados terminan convencionalmente en '?'
  ;; y se utiliza 'equal?' para igualdad estructural [Parte 2]
  (if (es-transicion-permitida? color-actual cambiar-a)
      (list color-actual (accion-color cambiar-a))
      (list color-actual 'accion-por-defecto)))

;; ============================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura 
;; ESTRATEGIA: Composición Funcional
;; IMPACTO EN MEMORIA: No destructiva
;; ============================================================
(define (timer tiempo-unix)
  ;; Se utiliza 'integer?' y 'modulo' (equivalente a MOD en Lisp)
  (if (integer? tiempo-unix)
      (ubicar-fase (modulo tiempo-unix 216))
      "Error: El tiempo Unix debe ser un numero entero"))




;;;; ============================================================
;;;; FASE 3: ESTUDIO COMPARATIVO - LISP-1 (Scheme) vs Lisp-2 y TCO
;;;; ============================================================

;; 1. Lisp-1 vs Lisp-2: Espacios de Nombres
;; En Common Lisp (un "Lisp-2"), los símbolos tienen espacios de nombres 
;; distintos para variables y funciones. Por esto, cuando pasamos 
;; una función como argumento, necesitamos el operador #' (abreviatura de 
;; function) para decirle al intérprete: "busca en el espacio de funciones", 
;; y luego usar funcall para ejecutarla.

;; En Scheme (un "Lisp-1"), existe un único espacio de nombres. Si un 
;; símbolo está ligado a un objeto función, se evalúa directamente como tal. 
;; No hace falta #' porque no hay ambigüedad entre una variable y una 
;; función con el mismo nombre; y no hace falta funcall porque Scheme 
;; evalúa la primera posición de una lista y, si es una función, la 
;; ejecuta de inmediato.
;;;;;; ============================================================
;;;;;; ============================================================
;; 2. Estructuración para Optimización de Llamada de Cola (TCO)
;; Para asegurar que el compilador de Scheme no agote la pila de memoria 
;; (conocido como Stack Overflow), se estructuró la recursividad de forma que la llamada 
;; recursiva sea la última acción realizada por la función.
;; 
;; En una función recursiva de cola (Tail Recursive), el resultado de la 
;; llamada recursiva se devuelve directamente sin realizar operaciones 
;; pendientes. Esto permite que el sistema reutilice el marco de pila actual en lugar de 
;; crear uno nuevo, garantizando una eficiencia de memoria equivalente a 
;; un bucle imperativo
