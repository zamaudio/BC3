
(in-package :BC)


(defparameter *WINCdst* "ccl:BC3WINpackage;BC3WINCL;BC3WINCL;")
(defparameter *WINCdemos* "ccl:BC3WINpackage;BC3WINCL;")

(defparameter *WINCL-files*
  '(BC-arrmath
    BC-basics
    BC-math
    BC-WINCL
  ;  BC-MCL
  ;  BC-LW
    BC-system
    BC-audio
    BC-elemfun
    BC-DSP
    BC-filters
    BC-Physical
    BC-immit
    BC-circuit
    BC-xducer
    WINCL-files
    BC-audio-test
    BC-circuit-tests
    BC-elemfun-tests
    ))

(defparameter *WINCL-directories*
  '("MDEMOS;DSP;"
    "MDEMOS;DSP;FILTERS;"
    "MDEMOS;DSP;INSTRU;"
    "MDEMOS;PHYS;ELE;"
    "RTDEMOS;DSP;"
    "RTDEMOS;INSTRU;"))


(defun compile-BC ()
  (defvar *platform* 'WINCL)
  (loop for fx in *WINCL-files*
        for fname = (format nil "~a.lisp" fx)
        do (compile-file fname)))

(defun load-BC ()
  (defvar *platform* 'WINCL)
  (loop for fx in *WINCL-files*
        for fname = (format nil "~a.fas" fx)
        do (if (probe-file fname)
             (load fname :verbose nil)
             (load (format nil "~a.lisp" fx)
                   :verbose nil))))

(defun mac-win-file (src-file dst-file)
  (with-open-file (srcs src-file)
    (with-open-file (dsts dst-file :if-exists :overwrite
                          :direction :output
                          :if-does-not-exist :create)
      (loop for line = (read-line srcs nil nil)
            while line
            do (format dsts "~a~a~a"
                       line '#\Newline '#\Linefeed)))))

(defun mac-win-dir (src-dir)
  (let* ((s (format nil "ccl:BC3;~a" src-dir))
         (d (format nil "~a~a" *WINCdemos* src-dir))
         (sfiles (directory (format nil "~a*" s) :files t))
         files)
    (loop for f in sfiles
          for fs = (namestring f)
          for pos = (position #\: fs :from-end t)
          for fnam = (subseq fs (1+ pos))
          do (progn (search ".lisp" fnam)
               (push fnam files)))
    (loop for f in files
          for src = (format nil "~a~a" s f)
          for dst = (format nil "~a~a" d f)
          do (when (probe-file dst) (delete-file dst))
          do (mac-win-file src dst))
    nil))

(defun mac-win ()
  (loop for fx in (remove 'BC-WINCL *WINCL-files*)
        for src = (format nil "ccl:BC3;~a.lisp" fx)
        for dst = (format nil "~a~a.lisp" *WINCdst* fx)
        do (when (probe-file dst) (delete-file dst))
        do (mac-win-file src dst))
  (loop for dx in *WINCL-directories*
        do (mac-win-dir dx)))

; (mac-win)

(provide :WINCL-files)
