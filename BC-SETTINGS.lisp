
(in-package :bc)

;;; These are user-specific settings

;;; path to MinGW directory
(defvar *MinGWgcc* "gcc -fPIC -shared")

;;; path to Pd export to be added

;;; path to MATLAB/OCTAVE export
(defvar *matlab* "~")

;;; set T if audio server should run continuously
(defparameter *AUDIO-SERVER-ALWAYS-ON* nil)

;;; comment this out if no audio-I/O required
;;(make-audio)

;;; END OF FILE
