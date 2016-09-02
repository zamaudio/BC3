
(in-package :bc)


;;;CLISP for Windows

(defparameter *BC-lib* nil)
(defparameter *BC-patch-dir* "patches")

(defun %syscall (string)
  (ext:shell string))



(defun write-patch% (p)
  (let* ((dirpath *BC-patch-dir*)
         (name (patchname p))
         (dst (format nil "~a~a/~a.c" dirpath name name)))
    (when (probe-file dst) (delete-file dst))
    (ensure-directories-exist dst)
    (with-open-file
      (str dst :direction :output
           :if-exists :overwrite
           :if-does-not-exist :create)
      (funcall 'c-code p str))))

(defun bc-compile% (p &optional (dir *BC-patch-dir*))
  (let* ((file (patchname p))
         (dirpath dir)
         (src (format nil "~a~a/~a.c" dirpath file file))
         (dst (format nil "~a~a/~a" dirpath file file))
         (str (format nil "~a -w -O3 -shared ~a -o ~a.so" *MinGWgcc* src dst)))
    (unless (%syscall str)
      (print str t)
      (error "Cannot compile patch ~a" p))
    nil))

(defun link-main% (p) p nil)

(defparameter *type-mapper*
  '((.char . ffi:char)
    (.short . ffi:short)
    (.long . ffi:long)
    (.float . ffi:single-float)
    (.double . ffi:double-float)))

(defun type.map (var)
  (cdr (assoc (datatype var) *type-mapper*)))

#|
(defmethod c-address ((v terminal))
  (let* ((b (host-block v))
         (p (find-patch b))
         (r (runtime p)))
    (fli:pointer-address
     (fli:make-pointer :symbol-name (string (varname v))
                       :module (patchname p)))))
|#

(defmethod reader-access ((b t)) nil)

(defmethod writer-access ((b t)) nil)

(defmethod reader-access ((b terminal))
  (when (creader b)
    (let* ((dim (datasize b))
           (type (type.map b))
           (name (varname b))
           (rname (format nil "get_~a" name))
           (funame (gensym))
           (p (find-patch (host-block b)))
	   (lname (bundle (runtime p)))
           (pname (string (patchname p)))
           (args (cond ((equal dim '(1)) nil)
                       ((= (length dim) 1) '((x ffi:long)))
                       ((= (length dim) 2)
                        '((x ffi:long) (y ffi:long))))))
      (when (var-p b)
        (eval
         `(ffi:def-call-out ,funame 
            (:name ,rname)
            (:arguments ,@args)
	    (:language :stdc)
            (:library ,lname)
	    (:return-type ,type)))
        (setf (creader b) funame)))))

(defmethod writer-access ((b terminal))
  (when (cwriter b)
    (let* ((dim (datasize b))
           (type (type.map b))
           (name (varname b))
           (rname (format nil "set_~a" name))
           (funame (gensym))
           (p (find-patch (host-block b)))
	   (lname (bundle (runtime p)))
           (pname (string (patchname p)))
           (args (cond ((equal dim '(1)) nil)
                       ((= (length dim) 1) '((x ffi:long)))
                       ((= (length dim) 2)
                        '((x ffi:long) (y ffi:long))))))
      (push (list 'val type) args)
      (when (var-p b)
        (eval
	  `(ffi:def-call-out ,funame 
            (:name ,rname)
            (:arguments ,@args)
	    (:language :stdc)
            (:library ,lname)
	    (:return-type nil)))
        (setf (cwriter b) funame)))))

(defclass runtime ()
  ((patchfun :initarg :patchfun :accessor patchfun)
   (npatchfun :initarg :npatchfun :accessor npatchfun)
   (patchaddr :initarg :patchaddr :accessor patchaddr)
   (bundle :initarg :bundle :accessor bundle)
   (host-patch :initarg :patch :accessor host-patch))
  (:default-initargs
    :patch nil
    :patchfun nil
    :npatchfun nil
    :patchaddr nil
    :bundle nil))

(defmethod stop-patch ((r runtime))
  nil)

(defun make-runtime (p)
  (let* ((pname (string (patchname p)))
         (rname (format nil "~a~a/~a.dll" *BC-patch-dir* pname pname))
         (bundle rname)
         (funame (gensym))
	 (nfuname (gensym))
         (gpname (gensym)))
    (eval
      `(ffi:def-call-out ,funame 
          (:name "patch")
          (:arguments)
	  (:language :stdc)
          (:library ,rname)
	  (:return-type ffi:int)))
    (eval
      `(ffi:def-call-out ,nfuname 
          (:name "npatch")
          (:arguments (n ffi:long))
	  (:language :stdc)
          (:library ,rname)
	  (:return-type nil)))
    (eval
      `(ffi:def-call-out ,gpname 
          (:name "paddr")
          (:arguments)
	  (:language :stdc)
          (:library ,rname)
	  (:return-type ffi:long)))
    (setf (runtime p)
          (make-instance 'runtime
            :bundle bundle :patchfun funame
	    :npatchfun nfuname
            :patchaddr gpname :patch p))
    (loop for b in (used-variables p)
          do (reader-access b))
    (loop for b in (used-variables p)
          do (writer-access b))
    (runtime p)))

(defmethod dispose-runtime ((r runtime))
  (when (bundle r) (ffi:close-foreign-library (bundle r)))
  (let ((hostp (host-patch r)))
    (setf (bundle r) nil (patchaddr r) nil
          (patchfun r) nil 
	  (npatcfun r) nil
	  (host-patch r) nil)
    (setf (runtime hostp) nil)))


(defun dispose-pointer (ptr)
  (ffi:foreign-free ptr))

(defun pointer-address (ptr)
  (ffi:foreign-address ptr))


(provide :BC-WINCLISP)

