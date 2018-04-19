#| 
Proyeccion a punto fijo, suponiendo que repetidas iteraciones nos llevaran 
siempre a un tal punto! Esto involucra en forma primaria la idea de Infinidad.  
|# 

(defvar i #c(0 1))
(defvar -i #c(0 -1))


(defun fixed-point (fn x &key (test #'equal)) 
	(do ((i x (funcall fn i))) 
		((funcall test i (funcall fn i)) i)))

(defun coeficiente (monomio)
    (first monomio)
)

(defun potencia (monomio)
    (second monomio)
)

(defun generadores (monomio)
    (third monomio)
)

;;; La funcion basica para el calculo en el Q-plano. Nuestros objetos son 
;;; listas de numeros 1 y -1, representando monomios de variable compleja 
;;; no-conmutativa (z <=> 1 & z* <=> -1). Lista vacia incluida <=> Uno.  

;;; Presentamos 3 soluciones ilustrando diferentes transformaciones de LISP. 
;;; Ehjemplo de polinomio '((1 -1 -1 1 -1 1 -1 . 2))
(defun q-step-4-1 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(i 1) L0)))
	    (if (null j) L
                (progn 
                    (setf (third L) (replace (copy-list L0) '(1 i) :start1 j))
                    (setf (second L) (+ z 1))
                    L
                )
        )))

(defun q-step-4-2 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(-i -1) L0)))
	    (if (null j) L
                (progn 
                    (setf (third L) (replace (copy-list L0) '(-1 -i) :start1 j))
                    (setf (second L) (+ z 1))
                    L
                )
        )))

(defun q-step-4-3 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(i -1) L0)))
	    (if (null j) L
                (progn 
                    (setf (third L) (replace (copy-list L0) '(-1 i) :start1 j))
                    (setf (second L) (- z 1))
                    L
                )
        )))

(defun q-step-4-4 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(-i 1) L0)))
	    (if (null j) L
                (progn 
                    (setf (third L) (replace (copy-list L0) '(1 -i) :start1 j))
                    (setf (second L) (- z 1))
                    L
                )
        )))                

(defun q-step-4-5 (L) 
      (let ((L0 (generadores L)))
        (setf j (search '(1 1) L0)) 

	    (setf (third L) (remove-if (lambda (x) (if (= (subseq x (+ x 2)  ))
        )
        
        ) '(1 -1) L0))
        (setf (third L) (remove '(-1 1) L0))
        (setf (third L) (remove '(i -i) L0))
        (setf (third L) (remove '(-i i) L0))
        L
    ))  

(defun q-step-4-5-perla (L)
    (if (subseq L ))
)    

(defun q-step-4-5 (L)
    (let* ( (x 0) (L0 (generadores L))  (h (1- (length L0))))
        (loop while (< x h)
            do (if 
                    (or 
                        (equal (list 1 -1)  (subseq L0 x (+ x 2)))
                        (equal (list -1 1)  (subseq L0 x (+ x 2)))
                        (equal (list i -i)  (subseq L0 x (+ x 2)))
                        (equal (list -i i)  (subseq L0 x (+ x 2)))
                    )
                    (if (null  (subseq L0 0 x))
                        (setf L0 (subseq L0 (+ x 2)))
                        (setf L0 (append (subseq L0 0 x) (subseq L0 (+ x 2))))
                    )
                )
            (print (subseq L0 x (+ x 2)))                
            (setf x (1+ x))
            (setf h (1- (length L0)))
        )
        (setf (third L) L0)
        L
    )
)             

(defun q-step (L)
    (q-step-4-5 (q-step-4-4 (q-step-4-3 (q-step-4-2 (q-step-4-1 L)))))
)                  


(defun q-connect (x y) (if (and (null x) (atom y))
                                         (cons nil y)
                                         (append x y)))


;;; La version que actua sobre listas (<=> "combinaciones" de monomios): 

(defun q-step-multi (W) (mapcan #'q-step-3 W))

;;; Polinomios con coeficientes arbitrarios (complejos), los vamos a
;;; interpretar como listas de conses (L . z) donde L es el monomio-lista 
;;; de uno & -1, con z coeficiente complejo.  


;;; Funciones Auxiliarias

#| 
La "longitud" que toma en cuenta el numero de los -unos en la lista. Los 
terminos se van a ordenar primero por la longitud estandard y luego por la 
potencia de la variable compleja no-conmutativo z. Por ejemplo: 
    (sort W (lambda (x y) (>= (q-length x) (q-length y))))
|#

(defun q-length (L) 
    (if (atom L) 0 
	(let* ((B (butlast L 0)) (u (length B)) (v (reduce #'+ B))) 
	     (+ u (/ (- u v) (* 2 (1+ u)))))))

;;;; Compactificacion & Expansion de Monomios No-conmutativos. 

(defun q-simplify (L)
        (cond ((null (cdr L)) L)
              ((= (car L) 0) (q-simplify (cdr L)))
              ((> (car L) 0 (cadr L)) (cons (car L) (q-simplify (cdr L))))
              ((< (car L) 0 (cadr L)) (cons (car L) (q-simplify (cdr L))))
              (T (q-simplify (cons (+ (car L) (cadr L)) (cddr L)))))) ;; aquí va

(defun q-expand (L) 
	(labels ((qe (n) 
		 	(if (>= n 0) (make-list n :initial-element 1) 
                 		(make-list (- n) :initial-element -1))))

	(mapcan #'qe L))) 

;;;; Indice de dado objeto en una lista. Si es cero, el objeto no aparece. 

(defun index (x L &key (test #'equal) (indf (constantly 1))) 
	(reduce #'+ (mapcar (lambda (y) (if (funcall test x y) 
					    (funcall indf y) 0)) L)))

;;;; Usamos funcion :test para determinar "igualdad" de objetos. A tales
;;;; objetos se aplica la funcion :op.   


;;;; Colectar objetos de una lista, con sus respetivos indices en conses. 

(defun compress (L &key (test #'equal) (indf (constantly 1)) (op #'identity)) 
    (if (null L) L 
       (let* ((x (car L)) (K (remove x (cdr L) :test test))) 
 	 (cons (append (funcall op x) 
		       (index x L :test test :indf indf)) 
 	       (compress K :op op :test test :indf indf)))))

(defun q-compress (W) 
  (compress W :test (lambda (x y) (equal (pp x) (pp y))) :op #'pp :indf #'qq))
