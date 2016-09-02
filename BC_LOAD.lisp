
;;; Evaluate this file to load BC3
;;; ------------------------------

;;; Default path to BC3LW in the the user documents directory

(defparameter *BC3* ;;; default path to own documents
  (format nil #+LISPWORKS5.1 "~aBC3LW/" #+LISPWORKS5.0 "~a/BC3LW/"
          (sys:get-folder-path :documents)))

;;; This may be used instead to give an explicit path to BC3LW, e.g.:
;;; (defparameter *BC3* "/Users/mak/BC3LW/") ;;; explicit path to BC3LW (MAC style)

(load (format nil "~aBC3/BC3_files.lisp" *BC3*))
#+MACOSX (load-system 'BC::BC-files)
#+MSWINDOWS (progn (cd (format nil "~aBC3/" *BC3*))
              (funcall 'bc::load-bc))
#+MSWINDOWS (defvar bc::*MinGWgcc* "C:/MinGW/bin/gcc") ;;; gcc-compiler on Windows

;;; Set path to MATLAB/OCTAVE
(defparameter bc::*matlab* (format nil "~aMATLAB" *BC3*))
(format nil "Path to MATLAB/OCTAVE export: ~a" bc::*matlab*)

;;; Set T if audio server is to run continuously
(defparameter bc::*AUDIO-SERVER-ALWAYS-ON* nil)
(format nil "AUDIO-SERVER-ALWAYS-ON = ~a" bc::*AUDIO-SERVER-ALWAYS-ON*)

;;; Comment this out if no audio-I/O required
(format nil "~a created" (bc::make-audio))

"BlockCompiler loaded"
