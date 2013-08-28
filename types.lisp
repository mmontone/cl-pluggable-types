(in-package :gradual)

 (defun equidimensional (a)
   (or (< (array-rank a) 2)
       (apply #'= (array-dimensions a))))

 (deftype square-matrix (&optional type size)
   `(and (array ,type (,size ,size))
         (satisfies equidimensional)))

(defun plistp (list)
  (and (listp list)
       (evenp (length list))
       (loop
	  for result = t then (and result (keywordp k))	    
	  for k in list by #'cddr
	  finally (return result))))

(deftype plist ()
  `(and cons
	(satisfies plistp)))

;(subtypep 'plist 'cons)

(defun alistp (list)
  (and (listp list)
       (reduce (lambda (b elem)
		 (and b (consp elem)))
	       list :initial-value t)))

(deftype alist ()
  `(and cons
	(satisfies alistp)))

(set-fun-type 'cl:concatenate (fun (symbol &rest sequence) sequence))
(set-fun-type 'cl:> (fun (number &rest number) boolean))
(set-fun-type 'cl:length (fun (sequence) number))
(set-fun-type 'cl:null (fun (t) boolean))
(set-fun-type 'cl:prin1-to-string (fun (t) string))
(set-fun-type 'cl:apply (fun (&rest t) t))
(set-fun-type 'cl:+ (fun (number &rest number) number))
