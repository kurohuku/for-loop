(defun enumrate (from to)
  (loop :for i from from to to :collect i))

(defun pattern-binding (form pattern)
  (if (/= (length form) (length pattern))
      nil
      (let ((binding
             (mapcar
              #'(lambda (f p)
                  (cond
                    ((and (keywordp f) (keywordp p))
                     (if (eq f p)
                         f
                         nil))
                    ((keywordp p) nil)
                    ((symbolp p) (list p f))
                    (T (error "Invalid pattern or form"))))
              form
              pattern)))
        (if (member nil binding)
            nil
            (remove-if #'keywordp binding)))))

(defmacro keyword-pattern-case (form &body pattern-clauses)
  (if (null pattern-clauses)
      nil
      (let ((binding (pattern-binding form (caar pattern-clauses))))
        (if binding
            `(let ,binding
               ,@(cdar pattern-clauses))
            `(keyword-pattern-case ,form ,@(cdr pattern-clauses))))))

(defun expand-for-forms (forms body)
  (if (null forms)
      `(progn ,@body)
      (let ((form (car forms))
            (rest (cdr forms))
            (sym1 (gensym))
            (sym2 (gensym)))
        (let ((gather (if rest :nconc :collect)))
	  (unless (symbolp (car form))
	    (error "form's first element is required 'symbol'"))
          `(keyword-pattern-case ,(cdr form)
	     ((:in ,sym1)
	      (loop :for ,(car form) :in ,sym1
		 ,gather ,(expand-for-forms rest body)))
	     ((:across ,sym1)
	      (loop :for ,(car form) :across ,sym1
		 ,gather ,(expand-for-forms rest body)))
	     ((:range ,sym1 ,sym2)
	      (loop :for ,(car form) :in (enumrate ,sym1 (1- ,sym2))
		 ,gather ,(expand-for-forms rest body)))
	     ((:repeat ,sym1 ,sym2)
	      (loop :repeat ,sym1
		 :for ,(car form) = (funcall ,sym2)
		 ,gather ,(expand-for-forms rest body))))))))

(defmacro for ((&rest forms) &body body)
  (unless (every #'listp forms)
    (error "Invalid forms"))
  (if (null forms)
      `(progn
         ,@body)
      (expand-for-forms forms body)))

;; example
;; (for ((a :in (list 1 2))
;;       (b :range 4 6)
;;       (c :across #(10 11)))
;;   (list a b c))
;; => ((1 4 10) (1 4 11) (1 5 10) (1 5 11) (2 4 10) (2 4 11) (2 5 10) (2 5 11))
;; (let ((a 0))
;;   (labels ((fn () (incf a)))
;;     (for ((b :repeat 3 #'fn)
;;  	  (c :in '(1 2)))
;;       (list b c))))
;; => ((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))
