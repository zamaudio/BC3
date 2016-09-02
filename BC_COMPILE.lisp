
;;; Evaluate this file to compile BC3
;;; ---------------------------------

;;; Default path to BC3LW In the the user documents directory

(defparameter *BC3* ;;; default path to own documents
  (format nil #+LISPWORKS5.1 "~aBC3LW/" #+LISPWORKS5.0 "~a/BC3LW/"
          (sys:get-folder-path :documents)))

;;; This may be used instead to give an explicit path to BC3LW, e.g.:
;;; (defparameter *BC3* "/Users/mak/BC3LW/") ;;; explicit path to BC3LW (MAC style)

(load (format nil "~aBC3/BC3_files.lisp" *BC3*))

#+MACOSX (compile-system 'BC::BC-files :force t)
#+MSWINDOWS (progn (cd (format nil "~aBC3/" *BC3*))
              (funcall 'bc::compile-bc))

"BlockCompiler compiled"
