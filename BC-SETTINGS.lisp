
(in-package :bc)

;;; These are user-specific settings

;;; path to MinGW directory
(defvar *MinGWgcc* "C:/MinGW/bin/gcc")

;;; path to Pd export to be added

;;; path to MATLAB/OCTAVE export
(defvar *matlab* "C:/Program Files/MATLAB")

;;; set T if audio server should run continuously
(defparameter *AUDIO-SERVER-ALWAYS-ON* nil)

;;; comment this out if no audio-I/O required
(make-audio)

;;; END OF FILE
