
(unless (find-package :BC) (make-package :BC))
(unless (find-package :QS) (make-package :QS))

(in-package :bc)

; "Set directory path to BC3LW in user directory"
(defvar *BC3* (cd))

(defparameter BC-files
  '("BC-arrmath"
    "BC-basics"
    "BC-math"
    "BC-LW"
    "BC-system"
    "BC-audio"
    "BC-elemfun"
    "BC-DSP"
    "BC-filters"
    "BC-Physical"
    
    "BC-consolid"
    
    "BC-immit"
    "BC-circuit"
    "BC-xducer"

    "BC-controls"
    ))

(eval
`(defsystem BC-files
   (:package :BC)
   :members
   ,BC-files))


(defun compile-BC ()
  (loop for fx in BC-files
        for fname = (format nil "~a.lisp" fx)
        do (compile-file fname :verbose nil)))

(defun load-BC ()
  (loop for fx in BC-files
        for fname = (format nil #+MSWINDOWS "~a.ofasl" fx)
        do (load fname :verbose nil)))	

; "Loading BlockCompiler in LispWorks"

; "Platform LispWorks"
(defvar *platform* 'LW)
