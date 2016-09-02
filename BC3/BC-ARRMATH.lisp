

(in-package :BC)

#+:CLISP
(eval-when (eval compile load)
  (setq CUSTOM:*FLOATING-POINT-CONTAGION-ANSI* t))

#+:CLISP
(eval-when (eval compile load)
  (setq CUSTOM:*PHASE-ANSI* t))

#+:CLISP
(eval-when (eval compile load)
  (setq pi (float pi 1.0d0)))

(setq *read-default-float-format* 'double-float)


;;; ARRAY MATH for BlockCompiler

(setf (get '.short 'proto) 0
      (get '.long 'proto) 0
      (get '.float 'proto) 0.0s0
      (get '.double 'proto) 0.0d0)

(defun arr-setval (dst value type)
  (let* ((ddim (array-dimensions dst)))
    (cond ((equal ddim '(1))
           `(setf (aref ,dst 0) (map.type1 ,value ',type)))
          ((= (length ddim) 1)
           `(loop for %i from 0 below ,(first ddim)
                  do (setf (aref ,dst %i) (map.type1 ,value ',type))))
          ((= (length ddim) 2)
           `(loop for %i from 0 below ,(first ddim)
                  do (loop for %j from 0 below ,(second ddim)
                           do (setf (aref ,dst %i %j)
                                    (map.type1 ,value ',type)))))
          (t (error "Invalid array dimensions")))))

;;; (arr-setval #(1) '(+ 1 2) 'single-float)
;;; (eval (arr-setval #(1) '(+ 1 2) 'single-float))

(defun arr-op (src dst op type)
  (let* ((sdim (array-dimensions src))
         (ddim (array-dimensions dst)))
    (unless (and (equal sdim ddim) (<= (length sdim) 2))
      (error "Array dimensions incompatible: ~a ~a" src dst))
    (cond ((equal sdim '(1))
           `(setf (aref ,dst 0) (map.type1 (,op (aref ,src 0)) ',type)))
          ((= (length sdim) 1)
           `(loop for %i from 0 below ,(first sdim)
                  do (setf (aref ,dst %i) (map.type1 (,op (aref ,src %i)) ',type))))
          ((= (length sdim) 2)
           `(loop for %i from 0 below ,(first sdim)
                  do (loop for %j from 0 below ,(second sdim)
                           do (setf (aref ,dst %i %j)
                                    (map.type1 (,op (aref ,src %i %j)) ',type)))))
          (t (error "Invalid array dimensions")))))

(defun arr-op-to (src dst op type)
  (let* ((sdim (array-dimensions src))
         (ddim (array-dimensions dst)))
    (unless (and (equal sdim ddim) (<= (length sdim) 2))
      (error "Array dimensions incompatible: ~a ~a" src dst))
    (cond ((equal sdim '(1))
           `(,op (aref ,dst 0) (aref ,src 0) ',type))
          ((= (length sdim) 1)
           `(loop for %i from 0 below ,(first sdim)
                  do (,op (aref ,dst %i) (aref ,src %i) ',type)))
          ((= (length sdim) 2)
           `(loop for %i from 0 below ,(first sdim)
                  do (loop for %j from 0 below ,(second sdim)
                           do (,op (aref ,dst %i %j)
                                   (aref ,src %i %j) ',type))))
          (t (error "Invalid array dimensions")))))

;;; (eval (arr-op-to #(1.0) #(2.0) '%incf 'short-float))

(defmacro %setf (dst src type)
  `(setf ,dst (map.type1 ,src ,type)))

(defmacro %incf (dst src type)
  `(setf ,dst (map.type1 (+ ,dst ,src) ,type)))

(defmacro %decf (dst src type)
  `(setf ,dst (map.type1 (- ,dst ,src) ,type)))

(defmacro %mulf (dst src type)
  `(setf ,dst (map.type1 (* ,dst ,src) ,type)))

(defmacro %divf (dst src type)
  `(setf ,dst (map.type1 (/ ,dst ,src) ,type)))

(defmacro %maxf (dst src type)
  `(setf ,dst (map.type1 (max ,dst ,src) ,type)))

(defmacro %minf (dst src type)
  `(setf ,dst (map.type1 (min ,dst ,src) ,type)))

(defmacro %pwrf (dst src type)
  `(setf ,dst (map.type1 (expt ,dst ,src) ,type)))

(defmacro %andf (dst src type)
  `(setf ,dst (map.type1 (if (and (/= ,dst 0) (/= ,src 0)) 1 0) ,type)))

(defmacro %orf (dst src type)
  `(setf ,dst (map.type1 (if (or (/= ,dst 0) (/= ,src 0)) 1 0) ,type)))

#|
(arr-op-to #(2) #(3) 'incf)
(eval (arr-op-to #(2) #(3) 'incf))
(defparameter arr1 #(1 2))
(defparameter arr2 #(3 4))
(arr-op-to arr1 arr2 'incf)
(arr-op arr1 arr2 'sqrt)
(eval (arr-op-to arr1 arr2 'incf))
(eval (arr-op-to arr1 arr2 'setf))
(defparameter arr1 (make-array '(2 2) :initial-contents '((1 2) (3 4))))
(defparameter arr2 (make-array '(2 2) :initial-contents '((2 2) (4 4))))
|#

#|
(defun inv-constant (lvalue &aux arr)
  (let* ((dim (array-dimensions lvalue)))
    (cond ((= (length dim) 1)
           (map 'vector #'(lambda (x) (/ 1.0d0 x)) lvalue))
          (t (setq arr (make-array dim))
             (loop for i from 0 below (first dim)
                   do (loop for j from 0 below (second dim)
                            for x = (/ 1.0d0 (aref lvalue i j))
                            do (setf (aref arr i j) x)))
             arr))))
|#


;;; BASIC TYPING

(defun map.type1 (val type)
  (when (and (not (eq type 'standard-char))
             (typep val 'standard-char))
    (setq val (char-int val)))
  (case type
    (double-float (cl:float val 1.0d0))
    (short-float (cl:float val 1.0s0))
    (fixnum (floor val))
    (t (coerce val type))))


(defun map.type (val type &aux dim valx)
  (cond ((or (typep val 'number)
             (typep val 'standard-char))
         (setq dim '(1) valx (map.type1 val type))
         (setq valx (make-array 1 :initial-element valx)))
        ((arrayp val)
         (setq dim (array-dimensions val))
         (setq valx (make-array dim))
         (cond ((= (length dim) 1)
                (loop for i from 0 below (first dim)
                      for x = (aref val i)
                      do (setf (aref valx i) (map.type1 x type))))
               ((= (length dim) 2)
                (loop for i from 0 below (first dim)
                      do (loop for j from 0 below (second dim)
                               for x = (aref val i j)
                               do (setf (aref valx i j) (map.type1 x type)))))
               (t (error "Invalid data ~a for data type ~a" val type))))
        ((consp val)
         (cond ((or (typep (first val) 'number)
                    (typep (first val) 'standard-char))
                (setq valx (map 'vector #'(lambda (x) (map.type1 x type)) val))
                (setq dim (list (length val))))
               ((consp (first val))
                (let* ((d1 (length val)) (d2 (length (first val))))
                  (setq dim (list d1 d2))
                  (loop for x in val
                        do (unless (and (= (length x) d2)
                                        (or (typep (first x) 'number)
                                            (typep (first x) 'standard-char)))
                             (error "Invalid list data ~a for type ~a" val type)))
                  (setq valx (make-array dim))
                  (loop for i from 0 below d1
                        for x1 in val
                        do (loop for j from 0 below d2
                                 for x2 in x1
                                 do (setf (aref valx i j) (map.type1 x2 type))))))
               (t (error "Invalid data ~a for data type ~a" val type)))))
  (values valx dim type))

(defun map.init (val type size)
  (setq type (get type 'ltype))
  (cond ((and (or (numberp val) (standard-char-p val))
              (not (equal size '(1))))
         (map.type (make-array size :initial-element val) type))
        ((and (or (numberp val) (standard-char-p val))
              (equal size '(1)))
         (map.type val type))
        (t (let ((arr (map.type val type)))
             (unless (equal (array-dimensions arr) size)
               (error "Invalid size of out-init"))
             arr))))

#|
(map.init 12.0 'short-float '(2))
(map.init 12.0 'double '(2))
(map.init 12.0 'short-float '(1))
(map.init '(12.0) 'short-float '(1))
(get '.float 'ltype)
(get '.double 'ltype)
(map.type 12.0 'short-float)
(map.type 12.0 'double-float)
(map.type '(12.0 13) 'short-float)
|#


;;; C STRUCTURE SUPPORT

(defclass cstruct ()
  ((host-block :initarg :host-block :accessor host-block)
   (sname :initarg :sname :accessor sname))
  (:default-initargs
    :host-block nil
    :sname (intern (string (gensym "STRUCT_"))))) ;; (gentemp "STRUCT_")))

(defmethod initialize-instance :after ((obj cstruct) &key) nil) ; ????

(defmethod get-slots ((obj cstruct))
  (let* ((slots nil)
         (type (type-of obj)))
    (setq slots (get type 'sslots))
    (loop for next = (get type 'inherit)
          while next
          do (setq slots (append (get next 'sslots) slots)))
    slots))

(defmethod struct-def ((obj cstruct) &optional (stream t))
  (let* ((type (type-of obj))
         (slots (get-slots obj)))
    (format stream "~%~%typedef struct {")
    (loop for s in slots
          for sname = (first s)
          for type = (string-downcase (string (second s)))
          for ptr = (third s)
          do (format stream "~%  ~a " type)
          do (when ptr (format stream "*"))
          do (format stream "~a;" sname))
    (format stream "~%} ~a;" (type-of obj))
    (format stream "~%~a" (get type 'code))))

(defmethod struct-spec ((obj cstruct) &optional (stream t))
  (let* ((type (type-of obj))
         (sname (sname obj))
         (slots (get-slots obj)))
    (format stream "~%~a ~a = {" type sname)
    (loop for s in slots
          for i from 0
          for var = (funcall (first s) obj)
          for vval = (cond ((eq var nil)
                            (funcall (first s) (host-block obj)))
                           ((typep var 'blockvar)
                            (funcall 'varname var))
                           (t var))
          do (if (= i 0)
               (format stream "~a" vval)
               (format stream ",~a" vval)))
    (format stream "};~%")))

(defparameter *keypackage*
  #+MCL ccl::*keyword-package*
  #+CLISP system::*keyword-package*
  #+LISPWORKS system::*keyword-package*
  )

(defmacro def.struct ((blockclass struct) inherit slots)
  (setf (get blockclass 'struct) struct
        (get blockclass 'sslots) slots
        (get struct 'block) blockclass
        (get struct 'sslots) slots
        (get struct 'inherit) inherit)
  (setq inherit (cons 'cstruct inherit))
  (loop with slots* = nil
        for s in slots
        for sn = (first s)
        for iarg = (intern (string sn) *keypackage*)
        do (push (list sn :initarg iarg :accessor sn) slots*)
        finally
        (setq slots (reverse slots*)))
  `(defclass ,struct (cstruct) ,slots))

(defmacro def.struct.c ((blockclass struct function) code)
  (setf (get blockclass 'fname) function
        (get struct 'fname) function)
  (setf (get blockclass 'code) code
        (get struct 'code) code)
  nil)

(provide :BC-arrmath)
