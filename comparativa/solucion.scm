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






;;;; ============================================================
;;;; FASE 3: ESTUDIO COMPARATIVO - LISP-1 vs Lisp-2 y TCO
;;;; ============================================================

;; 1. Lisp-1 vs Lisp-2: Espacios de Nombres
;; En Common Lisp (un "Lisp-2"), los símbolos tienen espacios de nombres 
;; distintos para variables y funciones [2, 3]. Por esto, cuando pasamos 
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
;; (Stack Overflow), se estructuró la recursividad de forma que la llamada 
;; recursiva sea la última acción realizada por la función [4].
;; 
;; En una función recursiva de cola (Tail Recursive), el resultado de la 
;; llamada recursiva se devuelve directamente sin realizar operaciones 
;; pendientes (como sumas o construcciones de listas posteriores). Esto 
;; permite que el sistema reutilice el marco de pila actual en lugar de 
;; crear uno nuevo, garantizando una eficiencia de memoria equivalente a 
;; un bucle imperativo, técnica fundamental dentro de la programación 
;; funcional para el manejo de procesos repetitivos
