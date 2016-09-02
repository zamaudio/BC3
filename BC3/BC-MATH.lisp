
(in-package :BC)


;;; SYMBOLIC MATH

(defmethod _eql ((b1 t) (b2 t)) nil)


;;; Basic elements

(defclass _form ()
  ((symname :initarg :name :accessor symname)
   (ivar :initarg :var :accessor ivar)
   (value :initarg :value :accessor value)
   (bcform :initarg :bcform :accessor bcform))
  (:default-initargs
    :name nil
    :var nil
    :value nil
    :bcform nil))

;;; elementary (non-composite) item
(defclass _elem () ())

;;; test if elementary item
(defmethod _elem? ((arg _elem)) t)

;;; for factoring to:   form rest factor power
(defmethod _collect-term ((arg _elem) form
                             &optional parametrize)
  (declare (ignore parametrize))
  (setq form (_val form))
  (cond ((_eql arg form) (list arg (_const 1) arg 1))
        (t (list nil arg arg 0))))

;;; collection of coefficient groups
(defparameter *csgroups* nil)

(defmethod _collect-coeffs% ((arg t)) nil)

;;; collect coefficient groups for parametrization
(defmethod _collect-coeffs ((arg t) &aux groups)
  (let* ((*csgroups* nil))
    (_collect-coeffs% arg)
    (setq groups (remove-duplicates
                  *csgroups* :test #'_eql)))
  (mapcar #'(lambda (x) (list x (_param nil x))) groups))

;;; parametrization of coefficient groups 
(defmethod _param-coeffgroups ((arg t) groups) arg)


(defun _param-coeffs (term)
  (let* ((cs (_collect-coeffs term)))
    (_param-coeffgroups term cs)))

;;; simplyfy z-expression
(defun _simpz (form)
  (let* ((rat (_make-ratio form)) ;;; rationalize
         (gs (_param-coeffs rat)) ;;; parametrize coeffs
         (ex (_expand gs))        ;;; expand products
         (tv (_tvar "z"))
         (ct (_collect-term ex tv t)))
    (setf (ivar ct) tv)
    ct))

;;; set (initial) value to variable
(defun _set (var value)
  (setq var (_val var))
  (unless (and (typep var '_var)
               (not (typep var '_tvar)))
    (error "Cannot assign ~a a value" var))
  (unless (or (numberp value)
              (typep value '_form))
    (error "Cannot assing ~a to ~a" value var))
  (setf (value var) value)
  var)


;;; For LaTeX presentation of expressions

;;; constant counter
(defparameter *CN* 0)
(defun next-c () (incf *CN*))

;;; variable counter
(defparameter *VN* 0)
(defun next-v () (incf *VN*))

;;; parameter counter
(defparameter *PN* 0)
(defun next-p () (incf *PN*))

(defparameter *leqns* nil) ;;; list of forms to latex
(defparameter *lvars* nil) ;;; ???

;;; LaTeX formatting of expression and its subexpressions
(defmethod latex-equation ((arg t) &optional (stream t))
  (let* ((*leqns* (list arg)) ;;; forms
         (*lvars* nil)        ;;; vars
         (*CN* 0) (*VN* 0) (*PN* 0))  ;;; counters zeroed
    (loop while *leqns*               ;;; while forms/lists
          for eq = (car *leqns*)      ;;; current form list
          for x = (when (consp eq) (car eq))
          for xp = (and x (member x *lvars*) t)
          do (cond (xp nil)
                   (t (format stream "~%\\begin{equation}~%")
                      (latex eq stream)     ;;; send for to latex
                      (format stream "~%\\end{equation}~%")))
          do (setq *leqns* (cdr *leqns*)))) ;;; drop current form
  nil)

;;; subexpression (list) to LaTeX
(defmethod latex ((arg list) &optional (stream t))
  (let* ((x1 (first arg))
         (x2 (second arg)))
    (format stream "~a = " (symname x2))
    (if (or (typep x1 '_compos)
            (typep x1 '_const))
      (latex x1 stream)
      (format-latex x1 stream))
    (pushnew x1 *lvars*))
  nil)

;;; default none to LaTeX
(defmethod latex ((b t) &optional (stream t))
  (declare (ignore stream))
  nil)

;;; formatting to latex, default = none
(defmethod format-latex ((arg t) &optional (stream t))
  stream nil)

;;; write LaTeX expression to file
;;; pre and post are LaTeX source 
;;; needed before and after the main expression(s)
(defun to-latex (form &key (file *latex*) pre post)
  (when (probe-file file) (delete-file file))
  (with-open-file (str file :direction :output :if-exists :overwrite
                       :if-does-not-exist :create)
    (format str "~%\\documentclass[12pt,a4paper]{article}
\\usepackage{times}
\\usepackage{amsmath}
\\title{\\Large BlockCompiler formula(s):\\vspace{-1.5cm}}
\\author{\\large {\\em }}
\\date{}
\\begin{document}
\\maketitle ~%~%
~%")
    (when pre (format str "~%~a~%" pre))
    (remove-dupl-constants form)
    (latex-equation form str)
    (when pre (format str "~%~a~%" post))
    (format str "~%~%\\end{document}")
    form))


;;; _const (constant)

;;; class of constants
(defclass _const (_form _elem) ())

;;; ordering in product terms
(defmethod _order ((arg _const)) 0)

;;; equality of math objects: constant vs. constant
(defmethod _eql ((b1 _const) (b2 _const))
  (= (value b1) (value b2)))

;;; formatting to latex: integer
(defmethod format-latex ((arg integer) &optional (stream t))
  (format stream " ~a" arg))

;;; formatting to latex: non-integer number (floats)
(defmethod format-latex ((arg number) &optional (stream t))
  (let* ((s (format nil "~g" arg))
         (p (or (position #\E s) (position #\e s)
                (position #\D s) (position #\d s)
                (position #\F s) (position #\f s)))
         (d (position #\. s)))
    (cond (p (format stream " ~a \\cdot 10^{~a}"
                     (subseq s 0 p) (subseq s (1+ p))))
          ((and d (eql (elt s (1+ d)) #\Space))
           (setf (elt s (1+ d)) #\0)
           (format stream "~a" s))
          (t  (format stream "~a" s)))))

;;; provide name before latexing
(defmethod check-name ((arg _const))
  (unless (symname arg)
    (setf (symname arg)
          (format nil "c_{.~a}" (next-v)))))

(defmethod latex ((arg _const) &optional (stream t))
  (check-name arg)
  (cond ((and (> (length (symname arg)) 1)
              (not (char-equal (elt (symname arg) 1) #\_))
              (not (char-equal (elt (symname arg) 1) #\^)))
         (format stream " \\underline{~a}" (symname arg)))
        (t (format stream " ~a" (symname arg))))
  (when (value arg)
    (setq *leqns* (append *leqns* (list (list (value arg) arg)))))
  nil)

(defmethod _lisp ((obj _const) &optional (stream t))
  (format stream "~a" (value obj))
  nil)

(defmethod to-lisp ((obj _const)) (value obj))

(defun _const (val &optional name &aux c)
  (cond ((numberp val)
         (setq c (make-instance '_const :value val)))
        ((typep val '.const)
         (let* ((o (out val)) (v (lvar o))
                (d (datasize o)))
           (unless (and (arrayp v) (equal d '(1)))
             (error "Ivalid 'const ~a for _const" val))
           (setq c (make-instance '_const :value (aref v 0)))))
        ((typep val '_const) (return-from _const val))
        (t (error "Invalid value ~a for _val" val)))
  (when name (unless (or (symbolp name) (stringp name))
               (error "Invalid name ~a for _const" name))
        (setf (symname c) (string name)))
  c)

(defparameter *consts* nil)

(defun find-constants (form)
  (declare (special *consts*))
  (let* ((*consts* nil))
    (%find-constants form)
    *consts*))

(defmethod %find-constants ((arg t)) nil)

(defmethod %find-constants ((arg _const))
  (push arg *consts*))

(defun remove-dupl-constants (form)
  ;;; side effect to (forms form)
  (let* ((cs (find-constants form))
         (protos (remove-duplicates cs :test #'_eql)))
    (%rem-dupl-consts form protos)
    form))

(defmethod %rem-dupl-consts ((arg t) protos) nil)


;;; _var (variable)

(defclass _var (_form _elem) ())

(defmethod _order ((arg _var)) 4)

(defmethod _eql ((b1 _var) (b2 _var))
  (cond ((eq b1 b2) t)
        ((and (string-equal (symname b1) (symname b2))
              (equal (value b1) (value b2))))
        (t nil)))

(defun _varterm? (x)
  (cond ((typep x '_var) t)
        ((and (typep x '_mul)
              (= (length (forms x)) 2)
              (typep (car (forms x)) '_const)
              (typep (second (forms x)) '_var)) t)
        ((and (typep x '_mul)
              (= (length (forms x)) 2)
              (typep (car (forms x)) '_var)
              (typep (second (forms x)) '_const)) t)
        (t nil)))

(defmethod latex ((arg _var) &optional (stream t))
  (check-name arg)
  (cond ((and (> (length (symname arg)) 1)
              (not (char-equal (elt (symname arg) 1) #\_))
              (not (char-equal (elt (symname arg) 1) #\^)))
         (format stream " \\underline{~a}" (symname arg)))
        (t (format stream " ~a" (symname arg))))
  (when (value arg)
    (setq *leqns* (append *leqns* (list (list (value arg) arg)))))
  nil)

(defmethod _lisp ((obj _var) &optional (stream t))
  (format stream "~a" (symname obj)))

(defmethod to-lisp ((obj _var))
  (intern (symname obj)))

(defmethod check-name ((arg _var))
  (unless (symname arg)
    (setf (symname arg)
          (format nil "v_{.~a}" (next-v)))))

(defun _var (x &optional value)
  (cond ((not x) (make-instance '_var
                   :name nil :value value))
        ((or (symbolp x) (stringp x))
         (make-instance '_var
           :name (string x) :value value))
        ((typep x '.var)
         (make-instance '_var
           :value x :name (varname (out x))))
        (t (error "Invalid _var spec ~a" x))))


;;; _tvar (transform variable)

(defclass _tvar (_var) ())

(defmethod _order ((arg _tvar)) 16)

(defmethod _tvarf? ((arg t)) nil)
(defmethod _tvarf? ((arg _tvar)) t)

(defun _tvar (x)
  (cond ((not x) (make-instance '_tvar :name nil))
        ((or (symbolp x) (stringp x))
         (make-instance '_tvar :name (string x)))
        (t (error "Invalid _tvar spec ~a" x))))

(defun _zpoly (&rest vals)
  (apply #'_add
         (loop with z = (_tvar "z")
               for i from 0
               for x in vals
               for p = (_pow z (- i))
               for y = (_mul x p)
               collect y)))


;;; _param (parameter)

(defclass _param (_var) ())

(defmethod _order ((arg _param)) 2)

(defmethod check-name ((arg _param))
  (unless (symname arg)
    (setf (symname arg)
          (format nil "p_{.~a}" (next-p)))))

(defmethod %find-constants ((arg _param))
  (when (value arg)
    (%find-constants (value arg))))

(defmethod %rem-dupl-consts ((arg _param) protos)
  (let* ((v (value arg)) x)
    (if (and v (setq x (find v protos :test #'_eql)))
      (setf (value arg) x)
      (%rem-dupl-consts v protos))))

(defun _param (x &optional value)
  (cond ((not x) (make-instance '_param
                   :name nil :value value))
        ((or (symbolp x) (stringp x))
         (make-instance '_param
           :name (string x) :value value))
        ((typep x '.var)
         (make-instance '_param
           :value x :name (varname (out x))))
        (t (error "Invalid _param spec ~a" x))))


;;; _val (mapping to _object)

(defun _val (x &optional value)
  (cond ((or (numberp x) (typep x '.const)) (_const x))
        ((or (symbolp x) (stringp x))
         (_var x value))
        ((or (typep x '_var) (typep x '_const)) x)
        ((typep x 'basic-block) (_param x))
        ((typep x '_form) x)
        (t (error "Invalid spec ~a for _val" x))))


;;; Composite elements

(defclass _compos (_form)
  ((forms :initarg :forms :accessor forms))
  (:default-initargs
    :forms nil))

(defmethod _eql ((b1 _compos) (b2 _compos))
  (cond ((eq b1 b2) t)
        ((and (eql (type-of b1) (type-of b2))
              (= (length (forms b1))
                 (length (forms b2))))
         (loop for x in (forms b1)
               for y in (forms b2)
               do (unless (_eql x y)
                    (return-from _eql nil))) t)
        (t nil)))

(defmethod _elem? ((b _compos))
  (loop for x in (forms b)
        do (when (typep x '_expr)
             (return-from _elem? nil)))
  t)

(defmethod _order ((arg t)) 10)

(defmethod %find-constants ((arg _compos))
  (loop for x in (forms arg)
        do (%find-constants x)))

(defmethod %rem-dupl-consts ((arg _compos) protos)
  (loop for x in (forms arg)
        do (%rem-dupl-consts x protos))
  (loop for x in protos
        do (nsubstitute
            x t (forms arg)
            :key #'(lambda (y) (_eql y x)))))


;;; _mul (multiplication)

(defclass _mul (_compos) ())

(defmethod _order ((arg _mul)) 8)

(defmethod _simp0 ((form _mul))
  (when (not (forms form))
    (return-from _simp0 form))
  (setq form (_mul-muls form))
  (setq form (_mul-consts form))
  (setq form (_mul-multips form))
  (setq form (_mul-ratsimp form))
  (when (typep form '_mul)
    (setf (forms form) (sort (forms form) #'< :key #'_order)))
  (when (and (typep form '_div) (zerop (length (den form))))
    (setq form (num form)))          
  (when (and (typep form '_mul) (= (length (forms form)) 1))
    (setq form (car (forms form))))
  form)

(defmethod _mul-muls ((arg _mul) &aux fs fx)
  (loop for f in (forms arg)
        do (cond ((typep f '_mul) (setq fx (_mul-muls f))
                  (loop for x in (forms fx) do (push x fs)))
                 (t (push f fs))))
  (make-instance '_mul :forms (reverse fs)))

(defmethod _mul-consts ((arg _mul))
  (let* ((ff (forms arg))
         (fs (remove '_const ff :key #'type-of))
         (cs (remove '_const ff :key #'type-of :test #'neq))
         (c (when cs (reduce #'* (mapcar #'value cs)))))
    (cond ((and cs (zerop c) (_const 0.0)))
          ((not fs) (_const c))
          ((and cs (= c 1.0) (= (length fs) 1)) (car fs))
          ((and cs (= c 1.0))
           (make-instance '_mul :forms fs))
          (cs (make-instance '_mul 
                :forms (cons (_const c) fs)))
          (t arg))))

(defmethod _mul-multips ((arg t)) arg)

(defmethod _mul-multips ((arg _mul))
  (let* ((elems (_collect-elems arg)) res)
    (loop for x in elems
          for n = (_mul-count arg x)
          do (cond ((and (typep n '_const) (= (value n) 1))
                    (push x res))
                   (t (push (_pow x n) res))))
    (make-instance '_mul :forms (reverse res))))

(defmethod _mul-ratsimp ((arg t)) arg)

(defmethod _mul-ratsimp ((arg _mul))
  (let* ((ns (mapcar #'num (forms arg)))
         (ds (remove 'nil (mapcar #'den (forms arg)))))
    (unless (intersection ns ds :test #'_eql)
      (return-from _mul-ratsimp arg))
    (loop for int = (intersection ns ds :test #'_eql)
          while int
          do (setq ns (remove (car int) ns :count 1 :test #'_eql))
          do (setq ds (remove (car int) ds :count 1 :test #'_eql)))
    (when (and (not ns) (not ds))
      (return-from _mul-ratsimp (_const 1)))
    (unless ds
      (return-from _mul-ratsimp (apply #'_mul ns)))
    (cond ((= (length ns) 1) (setq ns (car ns)))
          (t (setq ns (apply #'_mul ns))))
    #|
(cond ((= (length ds) 0)
           (return-from _mul-ratsimp ns))
          ((= (length ds) 1) (setq ds (car ds)))
          (t (setq ds (apply #'_mul ds))))
|#
    (_div ns ds)))

(defmethod _mul-count ((arg _mul) (form t) 
                         &aux (count (_const 0)))
  (loop for x in (forms arg)
        do (cond ((_eql x form) (setq count (_add count 1)))
                 ((and (typep x '_pow) (_eql (car (forms x)) form))
                  (setq count (_add count (second (forms x)))))
                 (t nil)))
  count)

(defmethod _collect-elems ((arg _mul) &aux res)
  (loop for x in (forms arg)
        for y = (if (typep x '_pow) (first (forms x)) x)
        do (when (not (member y res :test #'_eql))
             (push y res)))
  (reverse res))

(defmethod _collect-coeffs% ((arg _mul))
  (let* ((fs (forms arg)) cs)
    (loop for x in fs
          do (cond ((and (typep x '_elem)
                         (not (typep x '_tvar)))
                    (push x cs))
                   (t (_collect-coeffs% x))))
    (when (> (length cs) 1)
      (push (apply #'_mul (reverse cs)) *csgroups*))
    nil))

(defmethod _param-coeffgroups ((arg _mul) groups)
  (let* ((fs (forms arg)) cs rest y)
    (loop for x in fs
          do (cond ((and (typep x '_elem)
                         (not (typep x '_tvar)))
                    (push x cs))
                   (t (setq y (_param-coeffgroups
                               x groups))
                      (push y rest))))
    (when nil ; (<= (length cs) 1)
      (return-from _param-coeffgroups arg))
    (setq cs (apply #'_mul (reverse cs)))
; (inspect cs)
    (let* ((x (find cs groups :key #'car :test #'_eql)))
; (inspect (list x (reverse rest)))
      (if x ;(print 
          (apply #'_mul (cons (second x) (reverse rest))); )
          arg))))

(defmethod _negform? ((arg t)) nil)

(defmethod _negform? ((arg _mul))
  (let* ((fs (forms arg)) (f1 (first fs)))
    (and (typep f1 '_const) (< (value f1) 0) t)))

(defmethod _make-ratio ((arg t)) arg)

(defmethod _make-ratio ((arg _mul) &aux fs n d)
  (setq fs (mapcar #'_make-ratio (forms arg)))
  (loop for x in fs
        do (cond ((typep x '_div)
                  (push (num x) n)
                  (push (den x) d))
                 (t (push (_make-ratio x) n))))
  (cond (d (_div (apply #'_mul (reverse n))
                 (apply #'_mul (reverse d))))
        (t (apply #'_mul (reverse n)))))

(defmethod _expand ((arg t)) arg)

(defmethod _expand ((arg _mul) &aux res)
  (setq res (_expand (car (forms arg))))
  (loop for x in (cdr (forms arg))
        for y = (_expand x)
        do (setq res (_expand2 res y)))
  res)

(defmethod _expand2 ((b1 t) (b2 t))
  (_mul b1 b2))

(defmethod _collect-term ((arg _mul) form
                             &optional parametrize 
                             &aux r (p 0) x w)
  (declare (ignore parametrize))
  ;;; form rest factor power
  (setq form (_val form))
  (loop for y in (forms arg)
        do (cond ((_eql y form) (setq x y p 1))
                 ((and (typep y '_pow)
                       (_eql (first (forms y)) form))
                  (setq w (second (forms y)))
                  (unless (and (typep w '_const)
                               (integerp (value w)))
                    (error "Exponent not integer for term ~a" y))
                  (setq x y) (setq p (value w)))
                 (t (push y r))))
  (if r (setq r (apply #'_mul (reverse r)))
      (setq r (_const 1)))
  (list x r form p))

(defmethod latex ((arg _mul) &optional (stream t) &aux p)
  (loop for x in (forms arg)
        do (cond ((and (typep p '_div)
                       (typep x '_div))
                  (format stream " \\cdot"))
                 ((and p (typep x '_const))
                  (format stream " \\cdot"))
                 (t (format stream " \\,")))
        do (cond ((and (typep x '_add)
                       (not (symname x)))
                  (format stream "(")
                  (latex x stream)
                  (format stream ")"))
                 ((typep x '_const)
              ;    (format stream " +")
                  (latex x stream))
                 (t (latex x stream)))
        do (setq p x))
  nil)

(defmethod _lisp ((arg _mul) &optional (stream t))
  (cond ((and (= (length (forms arg)) 2)
              (_eql (first (forms arg)) (_const -1)))
         (format stream "(- ")
         (_lisp (second (forms arg)) stream))
        (t   (format stream "(*")
             (loop for x in (forms arg)
                   do (format stream " ")
                   do (_lisp x stream))))
  (format stream ")")
  nil)

(defmethod to-lisp ((arg _mul))
  (cond ((and (= (length (forms arg)) 2)
              (_eql (first (forms arg)) (_const -1)))
         (list '- (to-lisp (second (forms arg)))))
        (t (cons '* (mapcar #'to-lisp (forms arg))))))

(defmethod _tvar? ((arg _mul))
  (some #'_tvar? (forms arg)))

(defun _mul* (&rest forms)
  (let* ((fs (mapcar #'_val forms))
         (m (make-instance '_mul :forms fs)))
    m))

(defun _mul (&rest forms)
  (_simp0 (apply #'_mul* forms)))


;;; _add (addition)

(defclass _add (_compos) ())

(defmethod _order ((arg _add)) 11)

(defmethod _simp0 ((form _add))
  (setq form (_add-adds form))
  (setq form (_add-terms form))
  (setq form (_add-consts form))
  form)

(defmethod _add-adds ((arg _add) &aux fs fx)
  (loop for f in (forms arg)
        do (cond ((typep f '_add) (setq fx (_add-adds f))
                  (loop for x in (forms fx) do (push x fs)))
                 (t (push f fs))))
  (make-instance '_add :forms (reverse fs)))

(defmethod _add-consts ((arg t)) arg)

(defmethod _add-consts ((arg _add))
  (let* ((ff (forms arg))
         (fs (remove '_const ff :key #'type-of))
         (cs (remove '_const ff :key #'type-of :test #'neq))
         (c (when cs (reduce #'+ (mapcar #'value cs)))))
    (when (and cs (zerop c)) (setq cs nil c nil))
    (cond ((and (not cs) (not fs)) (_const 0))
          ((not fs) (_const c))
          ((and (not cs) (= (length fs) 1)) (car fs))
          ((not cs) (make-instance '_add :forms fs))
          (cs (make-instance '_add
                :forms (cons (_const c) fs)))
          (t arg))))

(defmethod _eqlt ((b1 _mul) (b2 _mul) &aux f1 f2)
  (let* ((c1 (car (forms b1)))
         (n1 (cond ((typep c1 '_const)
                    (setq f1 (cdr (forms b1)))
                    (value c1))
                   (t (setq f1 (forms b1)) 1)))
         (c2 (car (forms b2)))
         (n2 (cond ((typep c2 '_const)
                    (setq f2 (cdr (forms b2)))
                    (value c2))
                   (t (setq f2 (forms b2)) 1))))
    (when (_eql (apply #'_mul f1) (apply #'_mul f2))
      (apply #'_mul (cons (+ n1 n2) f1)))))

(defmethod _eqlt ((b1 t) (b2 _mul))
  (setq b1 (_val b1))
  (when (and (= (length (forms b2)) 2)
             (typep (first (forms b2)) '_const)
             (_eql (second (forms b2)) b1))
    (_mul (_add 1 (first (forms b2))) b1)))

(defmethod _eqlt ((b2 _mul) (b1 t))
  (setq b1 (_val b1))
  (when (and (= (length (forms b2)) 2)
             (typep (first (forms b2)) '_const)
             (_eql (second (forms b2)) b1))
    (_mul (_add 1 (first (forms b2))) b1)))

(defmethod _eqlt ((b2 t) (b1 t))
  (setq b1 (_val b1) b2 (_val b2))
  (when (_eql b2 b1) (_mul 2 b1)))

(defmethod _add-terms ((arg _add) &aux res)
  (loop for x in (forms arg)
        do (loop for i from 0
                 for y in res
                 for eq = (_eqlt x y)
                 do (when eq (setf (nth i res) eq) 
                          (return))
                 finally (push x res)))
  (if (= (length res) 1) (car res)
      (make-instance '_add :forms (reverse res))))

(defmethod _make-ratio ((arg _add) &aux fs fss ds dss n d di)
  (setq fs (mapcar #'_make-ratio (forms arg)))
  (loop for x in fs   ;;; collect _div's
        do (when (typep x '_div) (push (den x) ds)))
  (when (not ds)      ;;; if no _div's, return _add
    (return-from _make-ratio (apply #'_add fs)))
  (loop for x in (reverse ds) ;;; collect common divisors
        do (if (typep x '_mul)
             (loop for w in (forms x)
                   do (pushnew w dss :test #'_eql))
             (pushnew x dss :test-not #'_eql)))
  (loop for x in fs   ;;; multply by common divisors (smart)
        do (cond ((typep x '_div)
                  (setq d (if (typep (den x) '_mul)
                            (forms (den x)) (list (den x))))
                  (setq di (set-difference dss d :test '_eql))
                  (setq n (if (typep (num x) '_mul)
                            (forms (num x)) (list (num x))))
                  (push (apply #'_mul (append di n)) fss))
                 (t (push (apply #'_mul (cons x dss)) fss))))
  (_div (apply #'_add (reverse fss))
        (apply #'_mul (reverse dss))))

(defmethod _expand ((arg _add))
  (apply #'_add (mapcar #'_expand (forms arg))))

(defmethod _expand2 ((b1 _add) (b2 _add) &aux res)
  (loop for x in (forms b1)
        do (loop for y in (forms b2)
                 do (push (_mul x y) res)))
  (apply #'_add (reverse res)))

(defmethod _expand2 ((b1 t) (b2 _add))
  (apply #'_add (loop for x in (forms b2) collect (_mul b1 x))))

(defmethod _expand2 ((b1 _add) (b2 t))
  (apply #'_add (loop for x in (forms b1) collect (_mul x b2))))

(defmethod _collect-term ((arg _add) form 
                             &optional (parametrize t))
  ;;; list of lists: form rest factor power
  (let* ((fs (loop for y in (forms arg)
                   collect (_collect-term y form)))
         (pws (mapcar #'fourth fs))
         (pows (sort (remove-duplicates pws) #'>))
         res)
    (loop for n in pows
          for ff = (remove n fs :test-not #'= :key #'fourth)
          for fx = (apply #'_add (mapcar #'second ff))
          for fw = (if (and parametrize
                            (typep fx '_compos)
                            (> (length (forms fx)) 1))
                     (_param nil fx) fx)
          for fy = (first (first ff))
          for fz = (if fy fy (_const 1))
          do (push (_mul fw fz) res))
    (apply #'_add (reverse res))))

(defmethod _collect-coeffs% ((arg _add))
  (loop for x in (forms arg)
        do (_collect-coeffs% x)))

(defmethod _param-coeffgroups ((arg _add) groups)
  (let (res)
    (loop for x in (forms arg)
          for y = (_param-coeffgroups x groups)
; do (when (typep y '_param) (inspect y))
          do (push y res))
    (setq res (apply #'_add (reverse res)))
    res))

(defmethod latex ((arg _add) &optional (stream t))
  (latex (car (forms arg)) stream)
  (loop for x in (cdr (forms arg))
        ; do (if (_negform? x) (format stream " -")(format stream " +"))
        ; do (unless (_negform? x) (format stream " + "))
        do (format stream " +")
        do (latex x stream))
  nil)

(defmethod _lisp ((arg _add) &optional (stream t))
  (format stream "(+")
  (loop for x in (forms arg)
        do (format stream " ")
        do (_lisp x stream))
  (format stream ")")
  nil)

(defmethod to-lisp ((arg _add))
  (cons '+ (mapcar #'to-lisp (forms arg))))

(defmethod _tvar? ((arg _add))
  (some #'_tvar? (forms arg)))

(defun _add* (&rest forms)
  (let* ((fs (mapcar #'_val forms))
         (m (make-instance '_add :forms fs)))
    m))

(defun _add (&rest forms)
  (_simp0 (apply #'_add* forms)))


;;; _sub (subtraction, maps to addition)

(defun _sub (f1 f2)
  (setq f1 (_val f1) f2 (_val f2))
  (cond ((and (typep f1 '_const) (typep f2 '_const))
         (_const (- (value f1) (value f2))))
        ((and (typep f2 '_const) (zerop (value f2))) f1)
        (t (_simp0 (_add f1 (_neg f2))))))


;;; _div (division, rational expression)

(defclass _div (_compos) ())

(defmethod _order ((arg _div)) 12)

(defmethod num ((arg t)) arg)
(defmethod num ((arg _div)) (first (forms arg)))
(defmethod den ((arg t)) nil)
(defmethod den ((arg _div)) (second (forms arg)))

(defmethod _divsimp ((b1 t) (b2 t) form)
  (if (_eql b1 b2) (_const 1) form))

(defmethod _divsimp ((b1 _mul) (b2 _mul) form)
  (let* ((fs1 (forms b1))
         (fs2 (forms b2))
         (f1 (mapcar #'_baseform fs1))
         (f2 (mapcar #'_baseform fs2)))
    (unless (intersection f1 f2 :test #'_eql)
      (return-from _divsimp form))
    (loop for x in (intersection f1 f2 :test #'_eql)
          for c1 = (_get-pow x (forms b1))
          for c2 = (_get-pow x (forms b2))
          for c = (min c1 c2)
          do (setq fs1 (remove nil (_rem-pow x fs1 c)))
          do (setq fs2 (remove nil (_rem-pow x fs2 c))))
    (when (and (not fs1) (not fs2))
      (return-from _divsimp (_const 1)))
    (cond ((= (length fs1) 1) (setq fs1 (car fs1)))
          (t (setq fs1 (apply #'_mul fs1))))
    (cond ((= (length fs2) 1) (setq fs2 (car fs2)))
          (t (setq fs2 (apply #'_mul fs2))))
    (_div fs1 fs2)))

(defmethod _get-pow ((arg t) terms &aux (c 0))
  (loop for x in terms
        for y = (_baseform x)
        do (when (_eql arg y)
             (cond ((typep x '_pow)
                    (incf c (value (second (forms x)))))
                   (t (incf c)))))
  c)

(defmethod _rem-pow ((arg t) terms count &aux res)
  (loop for x in terms
        for b = (_baseform x)
        for e = (if (typep x '_pow)
                  (value (second (forms x))) 1)
        for ex = (- e count)
        do (cond ((and (_eql arg b) (= ex 0))
                  (push nil res))
                 ((_eql arg b) (push (_pow b ex) res))
                 (t (push x res))))
  (reverse res))

(defmethod _divsimp ((b1 _mul) (b2 t) form)
  (let* ((f1 (forms b1)))
    (if (member b2 f1 :test #'_eql)
      (apply #'_mul (remove b2 f1 :count 1 :test #'_eql))
      form)))

(defmethod _divsimp ((b1 t) (b2 _mul) form)
  (let* ((f2 (forms b2)))
    (if (member b1 f2 :test #'_eql)
      (_inv (apply #'_mul (remove b1 f2 :count 1 :test #'_eql)))
      form)))

#|
(defmethod _divsimp ((b1 _mul) (b2 _div) form)
  (_mul b1 (_div (den b2) (num b2))))
|#

(defmethod _divsimp ((b1 _mul) (b2 _div) form)
  (_div (_mul b1 (den b2)) (num b2)))

(defmethod _divsimp ((b1 t) (b2 _const) form)
  (_mul b1 (_const (/ (value b2)))))

#|
(defmethod _make-ratio ((arg _div))
  (let* ((n (_make-ratio (num arg)))
         (d (_make-ratio (den arg))))
    (cond ((typep d '_div)
           (_mul n (_div (den d) (num d))))
          (t (_div n d)))))
|#

(defmethod _make-ratio ((arg _div))
  (let* ((n (_make-ratio (num arg)))
         (d (_make-ratio (den arg))))
    (cond ((and (typep n '_div)
                (typep d '_div))
           (_div (_mul (num n) (den d))
                 (_mul (den n) (num d))))
          ((typep n '_div)
           (_div (num n) (_mul (den n) d)))
          ((typep d '_div)
           (_div (_mul n (den d)) (num d)))
          (t (_div n d)))))

#|
(defmethod _make-ratio ((arg _div))
  (let* ((n (_make-ratio (num arg)))
         (d (_make-ratio (den arg))))
    (cond ((and (typep n '_div)
                (typep d '_div))
           (_div (_mul (num n) (den d))
                 (_mul (den n) (num d))))
          ((typep n '_div)
           (_div (num n) (_mul (den n) d)))
          ((typep d '_div)
           (_div (_mul n (den d)) (num d)))
          (t (_div n d)))))
|#

(defmethod _expand ((arg _div))
  (_div (_expand (num arg)) (_expand (den arg))))

(defmethod _collect-term ((arg _div) form
                             &optional parametrize)
  (declare (ignore parametrize))
  (let* ((cnum (_collect-term (num arg) form))
         (cden (_collect-term (den arg) form)))
    (when (and (consp cnum) (not (first cnum)))
      (setq cnum (second cnum)))
    (when (and (consp cden) (not (first cden)))
      (setq cden (second cden)))
    (_div cnum cden)))

(defmethod _collect-coeffs% ((arg _div))
  (_collect-coeffs% (num arg))
  (_collect-coeffs% (den arg)))

(defmethod _param-coeffgroups ((arg _div) groups)
  (_div (_param-coeffgroups (num arg) groups)
        (_param-coeffgroups (den arg) groups)))

(defmethod latex ((arg _div) &optional (stream t))
  (let* ((num (num arg))
         (den (den arg)))
    (format stream "\\frac{")
    (latex num stream)
    (format stream "}{")
    (latex den stream)
    (format stream "}"))
  nil)

(defmethod _lisp ((arg _div) &optional (stream t))
  (format stream "(/ ")
  (_lisp (num arg) stream)
  (format stream " ")
  (_lisp (den arg) stream)
  (format stream ")")
  nil)

(defmethod to-lisp ((arg _div))
  (list '/ (to-lisp (num arg)) (to-lisp (den arg))))

(defmethod _tvarf? ((arg _div))
  (let* ((num (num arg))
         (den (den arg)))
    (if (or (_tvarf? num) (_tvarf? den)) t nil)))

(defmethod _div-numden ((arg _div) factor)
  (_div (_div (num arg) factor)
        (_div (den arg) factor)))

(defun _div (f1 f2)
  (setq f1 (_val f1) f2 (_val f2))
  (let ((d (make-instance '_div :forms (list f1 f2))))
    (_divsimp f1 f2 d)))

(defun _div* (f1 f2)
  (setq f1 (_val f1) f2 (_val f2))
  (let ((d (make-instance '_div :forms (list f1 f2))))
    d))


;;; _neg (negation, maps to multiplication)

(defun _neg (form)
  (setq form (_val form))
  (cond ((typep form '_const)
         (_const (- (value form))))
        (t (_mul (_const -1) form))))


;;; _inv (reciprocal, maps to division)

(defun _inv (form)
  (setq form (_val form))
  (cond ((typep form '_const)
         (_const (/ (value form))))
        (t (_div (_const 1) form))))


;;; _pow (power)

(defclass _pow (_compos) ())

(defmethod _baseform ((arg t)) arg)

(defmethod _baseform ((arg _pow))
  (first (forms arg)))

(defmethod _order ((arg _pow))
  (_order (first (forms arg))))

(defmethod _simp0 ((arg _pow))
  (let* ((b (first (forms arg)))
         (e (second (forms arg))))
    (_powsimp b e arg)))

(defmethod _powsimp ((b t) (e t) arg) arg)

(defmethod _powsimp ((b t) (e _const) arg)
  (let* ((x (value e)))
    (cond ((= x 0) (_const 1))
          ((= x 1) b)
          ((and (= x -1) (not (typep b '_tvar)))
           (_div 1 b))
          (t arg))))

(defmethod _powsimp ((b _pow) (e _const) arg)
  (let* ((x (value e)))
    (cond ((= x 0) (_const 1))
          ((< x 0)
           (_div 1 (_pow (first (forms b))
                         (_mul (second (forms b)) e))))
          (t (_pow (first (forms b))
                   (_mul (second (forms b)) e))))))
  
(defmethod latex ((arg _pow) &optional (stream t))
  (cond (t (let* ((fs (forms arg))
                  (f (first fs))
                  (e (second fs)))
             (cond ((typep f '_compos)
                    (format stream "(")
                    (latex f stream)
                    (format stream ")"))
                   (t (latex f stream)))
             (format stream "^{")
             (cond ((typep e '_const)
                    (format-latex (value e) stream))
                   ((typep e '_compos)
                    (format stream "(")
                    (latex e stream)
                    (format stream ")"))
                   (t (latex e stream)))
             (format stream "}"))))
  nil)

(defmethod _lisp ((arg _pow) &optional (stream t))
  (format stream "(expt ")
  (_lisp (first (forms arg)) stream)
  (format stream " ")
  (_lisp (second (forms arg)) stream)
  (format stream ")")
  nil)

(defmethod to-lisp ((arg _pow))
  (list 'expt (to-lisp (first (forms arg)))
        (to-lisp (second (forms arg)))))

(defmethod _tvarf? ((arg _pow))
  (let* ((b (first (forms arg)))
         (e (second (forms arg))))
    (if (or (_tvarf? b) (_tvarf? e)) t nil)))

(defmethod _expand ((arg _pow) &aux fs)
  (let* ((b (first (forms arg)))
         (e (second (forms arg))))
    (setq b (_expand b))
    (cond ((typep b '_mul)
           (loop for x in (forms b)
                 do (push (_pow x e) fs))
           (apply #'_mul (reverse fs)))
          ((and (typep b '_add)
                (typep e '_const)
                (integerp (value e)))
           (setq fs b)
           (loop for i from 1 below (value e)
                 do (setq fs (_expand (_mul e fs))))
           fs)
          (t arg))))

(defmethod _collect-term ((arg _pow) form &optional parametrize)
  ;;; list of lists: form rest factor power
  (declare (ignore parametrize))
  (list arg (_const 1) form (value (second (forms arg)))))

(defmethod _collect-coeffs% ((arg _pow))
  (_collect-coeffs% (first (forms arg))))

(defmethod _param-coeffgroups ((arg _pow) groups)
  (_pow (_param-coeffgroups (first (forms arg)) groups)
        (second (forms arg))))

(defmethod %find-constants ((arg _pow))
  (let* ((b (first (forms arg)))
         (e (second (forms arg))))
    (cond ((and (typep b '_tvar)
                (typep e '_const)) nil)
          (t (%find-constants b)
             (%find-constants e)))))

(defmethod %rem-dupl-consts ((arg _pow) protos)
  (let* ((b (first (forms arg)))
         (e (second (forms arg))))
    (cond ((and (typep b '_tvar)
                (typep e '_const)) nil)
          (t (%rem-dupl-consts b protos)
             (%rem-dupl-consts e protos)))))

(defun _pow* (form power)
  (setq form (_val form) power (_val power))
  (let* ((p (make-instance '_pow)))
    (setf (forms p) (list form power))
    p))

(defun _pow (form power)
  (setq form (_val form) power (_val power))
  (let* ((p (make-instance '_pow)))
    (setf (forms p) (list form power))
    (_simp0 p)))


;;; BC PARSING

(defmethod .bc ((arg number))
  (let* ((b (_const arg))
         (bc (.bc b)))
    (setf (bcform b) bc)
    bc))

(defmethod .bc ((arg basic-block))
  arg)

(defmethod .bc ((arg _const))
  (when (bcform arg)
    (return-from .bc (bcform arg)))
  (let* ((bc (.const (value arg))))
    (setf (bcform arg) bc)
    bc))

(defmethod .bc ((arg _var)) ;;; not ready
  (when (bcform arg)
    (return-from .bc (bcform arg)))
  (unless (value arg)
    (error "No init-value for ~a" arg))
  (let* ((bc (.var (value arg))))
    (setf (bcform arg) bc)
    bc))

(defmethod .bc ((arg _param)) ;;; not ready
  (when (bcform arg)
    (return-from .bc (bcform arg)))
  (unless (value arg)
    (error "No init-value for ~a" arg))
  (let* ((p (.bc (value arg))))
    (setf (bcform arg) p)
    (return-from .bc p))
  (let* ((bc (.var (value arg))))
    (setf (bcform arg) bc)
    bc))

;;; map _xxx to .xxx
(setf (get '_mul 'bc) '.mul
      (get '_add 'bc) '.add
      (get '_div 'bc) '.div
      (get '_pow 'bc) '.pow)

(defmethod .bc ((arg _compos))
  (when (bcform arg)
    (return-from .bc (bcform arg)))
  (unless (forms arg)
    (error "Inputs not defined for ~a" arg))
  (let* ((bcf (get (type-of arg) 'bc))
         (ins (mapcar #'.bc (forms arg)))
         (bc (funcall bcf :inputs ins)))
    (setf (bcform arg) bc)
    bc))


;;; Functions

(defclass _function (_form) ())

;;; map _xxx to .xxx
(setf (get '_sqr 'bc) '.sqr
      (get '_sqrt* 'bc) '.sqrt*
      (get '_sin 'bc) '.sin
      (get '_cos 'bc) '.cos
      (get '_tan 'bc) '.tan
      (get '_atan 'bc) '.atan
      (get '_tanh 'bc) '.tanh
      (get '_D 'bc) '.D)

(defmacro def_fun (name lname)
  (let ((arg (gensym)) (val (gensym)))
    `(progn
       (defclass ,name (_function) ())
       (defun ,name (,arg)
         (let ((,val (_val ,arg)))
           (if (typep ,val '_const)
             (_const (,lname (value ,val)))
             (make-instance ',name
               :value ,val)))))))

(defun sqr% (x) (* x x))
(defun sqrt% (x) (sqrt (max x 0.0)))
(def_fun _sqr sqr%)
(def_fun _sqrt* sqrt%)
(def_fun _sin sin)
(def_fun _cos cos)
(def_fun _tan tan)
(def_fun _atan atan)
(def_fun _tanh tanh)

(defmethod .bc ((arg _function))
  (let* ((type (type-of arg))
         (bc (get type 'bc)) f)
    (setq f (funcall bc (.bc (value arg))))
    (setf (bcform arg) f) f))

#|
(inspect (_tanh 2))
(inspect (_tanh (_var 'a 2)))

(defparameter x (_D 2))
(defparameter x (_sin 2))
(defparameter x (_sin (_var 'a 2)))
(defparameter x (_tanh 2))
(defparameter x (_tanh (_var 'a 2)))
(inspect x)
(inspect (.bc x))
|#

;;; Delays

(defclass _D (_function) ())

(defun _D (arg)
  (make-instance '_D
    :value (_val arg)))

(defmethod .bc ((arg _D))
  (let ((d (.D)))
    (-> (.bc (value arg)) d)
    (setf (bcform arg) d) d))


;;; Expression evaluation ;;; CHECK THESE !!!

(defun valfun (form &optional (vars (list (_var "z"))))
  (let* ((lf (to-lisp form))
         (lv (mapcar #'to-lisp vars)))
    (eval `(lambda ,lv ,lf))))

(defun zvalfun (form)
  (let* ((lf (to-lisp form))
         (lv (mapcar #'to-lisp (list (_var "z")))))
    (eval `(lambda ,lv ,lf))))

(defun svalfun (form)
  (let* ((lf (to-lisp form))
         (lv (mapcar #'to-lisp (list (_var "s")))))
    (eval `(lambda ,lv ,lf))))


;;; NAMING & ETC

(defun setname% (obj name)
  (when (and (typep obj '_form)
             (not (symname obj)))
    (setf (symname obj) (string name)))
  obj)


; -----

(provide :BC-math)
