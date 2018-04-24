(defun fixed-point (fn x &key (test #'equal)) 
	(do ((i x (funcall fn i))) 
		((funcall test i (funcall fn i)) i)))

(defun coeficiente (monomio)
    (first monomio)
)

(defun generadores (monomio)
    (second monomio)
)

;'( ((2 3) (4 -1)) (1 -1 -2 1 2 -2) )

;;(defun pp (L) (if (atom L) nil (butlast L 0)))
;;(defun qq (L) (if (atom L) L (last L 0)))

(defun cambiazetapositivo (L)
    (setf (second L) (+ (second L) 1))
    L
)

(defun cambiazetanegativo (L)
    (setf (second L) (- (second L) 1))
    L
)


(defun cambiazetas+ (L)
    (mapcar #'cambiazetapositivo L)
)

(defun cambiazetas- (L)
    (mapcar #'cambiazetanegativo L)
)

(defun q-step-4-1 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(2 1) L0)))
	    (if (null j) L
                (progn 
                    (setf (second L) (replace (copy-list L0) '(1 2) :start1 j))
                    (setf (first L) (cambiazetas+ (first L)))
                    L
                )
        )))

(defun q-step-4-2 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(-2 -1) L0)))
	    (if (null j) L
                (progn 
                    (setf (second L) (replace (copy-list L0) '(-1 -2) :start1 j))
                    (setf (first L) (cambiazetas+ (first L)))
                    L
                )
        )))

(defun q-step-4-3 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(2 -1) L0)))
	    (if (null j) L
                (progn 
                    (setf (second L) (replace (copy-list L0) '(-1 2) :start1 j))
                    (setf (first L) (cambiazetas- (first L)))
                    L
                )
        )))

(defun q-step-4-4 (L) 
        (let* ((L0 (generadores L)) (z (potencia L)) (c (coeficiente L)) (j (search '(-2 1) L0)))
	    (if (null j) L
                (progn 
                    (setf (second L) (replace (copy-list L0) '(1 -2) :start1 j))
                    (setf (first L) (cambiazetas- (first L)))
                    L
                )
        )))                



(defun q-step-4-5 (L)
    (let* ( (x 0) (L0 (generadores L))  (h (1- (length L0))))
        (loop while (< x h)
            do (if 
                    (or 
                        (equal (list 1 -1)  (subseq L0 x (+ x 2)))
                        (equal (list -1 1)  (subseq L0 x (+ x 2)))
                        (equal (list 2 -2)  (subseq L0 x (+ x 2)))
                        (equal (list -2 2)  (subseq L0 x (+ x 2)))
                    )
                    (if (null  (subseq L0 0 x))
                        (setf L0 (subseq L0 (+ x 2)))
                        (setf L0 (append (subseq L0 0 x) (subseq L0 (+ x 2))))
                    )
                    (setf x (1+ x))
                )
            
            (setf h (1- (length L0)))
        )
        (setf (second L) L0)
        L
    )
)             

(defun q-step-casi (L)
    (q-step-4-5 (q-step-4-4 (q-step-4-3 (q-step-4-2 (q-step-4-1 L)))))
) 

(defun q-step (L)
    (if (equal (copy-list L) (q-step-casi L))
        L
        (q-step (q-step-casi L))
    )
)                 

(defun q-step-multi (W) (mapcan #'q-step W))

(defun q-suma-casi (m1 m2)
    (let ((coef NIL) (polinomio NIL))
        (if (equal (second m1) (second m2))
            (progn 
                (setf coef (append (coeficiente m1) (coeficiente m2)))
                (setf polinomio (list coef (second m1)))
            )
            (setf polinomio (list m1 m2))
        )
        polinomio
    )
)

;Función que multiplica dos monomios
(defun prodmonz (L1 L2)
    "
    Función que multiplica dos monomios
    "
    (if (and (not (null L1)) (not (null L2)))
        (let ((L0 (list 1 1)))
            (setf (first L0) (* (first L1) (first L2)))
            (setf (second L0) (+ (second L1) (second L2)))
            L0
        )
    )
)

;Función que multiplica dos polinomios
(defun z-prod (L1 L2)
    "
    Función que multiplica dos polinomios
    "
    (let (producto)
        (dolist (m1 L1 producto)
            (dolist (m2 L2)
                (push 
                    (prodmonz m1 m2)
                    producto
                )
            )
        )
    )
)


(defun producto2 (m1 m2)
    (let ((producto (list "perla" "perla")))
        (setf (first producto) (z-prod (first m1) (first m2)))
        (setf (second producto) (append (second m1) (second m2)))
        producto
    )
)


(defun q-connect (x y) (if (and (null x) (atom y))
                                         (cons nil y)
                                         (append x y)))



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
