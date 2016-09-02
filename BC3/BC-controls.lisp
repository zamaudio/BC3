
(in-package :BC)

;;; CONTROL WIDGETS

;;; SLIDER

(defclass .slider (capi:slider .var)
  ((show-mapper :initarg :show-mapper :accessor show-mapper)
   (value-mapper :initarg :value-mapper :accessor value-mapper)
   (value-format :initarg :format :accessor value-format)
   (start :initarg :startx :accessor start)
   (stop :initarg :stop :accessor stop)
   (scaler :initarg :scaler :accessor scaler))
  (:default-initargs
   :show-mapper #'identity
   :value-mapper #'identity
   :format "~6,1f"))

(defmethod initialize-instance :after ((b .slider) &key
                                       init)
  (setf (capi:range-slug-start b) init)
  (set-value b (funcall (value-mapper b) init))
  (show-value b (funcall (show-mapper b) init))
  nil)

(defmethod callback ((b .slider) x e)
  (let* ((bx (start b))
         (y (+ bx (* (scaler b) x))))
    (set-value b (funcall (value-mapper b) y))
    (show-value b (funcall (show-mapper b) y))
    nil))

(defmethod show-value ((b .slider) x)
  (let* ((f (value-format b))
         (s (format nil f x)))
    (when (name b)
      (capi:apply-in-pane-process
       b  #'(setf capi:titled-object-title)
       (format nil "~a ~a" (name b) s)
       b))))

(defmethod set-value ((b .slider) x)
  (let* ((out (out b))
         (outv (float (funcall (value-mapper b) x) 1.0d0)))
    (cond ((not (member (cwriter out) '(nil t)))
           (funcall (cwriter out) outv))
          ((arrayp (lvar out))
           (setf (aref (lvar out) 0) outv))
          ((setf (lvar out) (make-array 1 :initial-element outv)))))
  nil)

(defun .slider (&rest rest &key (name nil)
                      (size 500)
                      (init start)
                      (start 0.0)
                      (stop 1000.0)
                      &allow-other-keys)
  (let* ((inid (- init start))
         (ini (* inid (/ (float size) (- stop start))))
         (s (apply #'make-instance '.slider
                   :title name :name name
                   :startx start :stop stop
                   :start 0 :end (floor size) :init (floor ini)
                   :visible-min-width (floor size)
                   :visible-max-width (floor size)
                   :scaler (/ (- stop start) (float size))
                   :callback #'(lambda (o x e) (callback o x e))
                   :allow-other-keys t
                   rest)))
    (callback s ini nil)
    s))

(defun sliders (&rest items)
  (let* (res)
    (push (first items) res)
    (loop for x in (cdr items)
          do (push (separator) res)
          do (push x res))
    (make-instance 'capi:column-layout
                   :description (reverse res))))

(defun h (&rest rest)
  (make-instance 'capi:row-layout
                 :description rest))

(defun v (&rest rest)
  (make-instance 'capi:column-layout
                 :description rest))

(defclass radio-button-panel (capi:radio-button-panel) ())

(defclass start-stop-buttons (radio-button-panel) ())

(defun button-start (button interface)
  (run-patch (bc-patch interface)))

(defun button-stop (button interface)
  (stop-patch (bc-patch interface)))

(defun make-start-stop-buttons ()
  (make-instance 'start-stop-buttons
                 :items '("Patch stopped" "Patch running")
                 :mnemonics '(1 2)
                 :callbacks '(button-stop button-start)
                 :layout-args '(:x-gap 15)))

(defclass .trig-button (.trig capi:push-button) ()
  (:default-initargs :callback 'trig-me))

(defmethod initialize-instance :after ((b .trig-button) &key)
  (setf (capi:item-data b) b))

(defmethod trig-me ((b .trig-button) &rest rest)
  (let* ((out (out b))
         (wr (cwriter out)))
    (when (not (member wr '(t nil)))
      (trig b))))

(defun .trig-button (&key (text "Trig") (name nil))
  (make-instance '.trig-button :mnemonic-text text :data 1 :name name))

#|
(defpatch trig ((tr (.trig-button))
                (pl (.trig-button :text "Pluck")))
  (-> tr (.probe "out"))
  (setq trx tr)
  (patch-controls (h tr pl)))

(load-patch trig)
(capi:contain trx)
(trig trx)
(setf (capi::item-data trx) 2)

(defpatch trig ((tr (.trig-button))
                (pl (.trig-button :text "Pluck"))
                (sl  (.slider :name "Freq:" :start 100.0 :end 1000.0 :init 300.0)))
  (-> tr (.probe "out"))
  (setq trx tr)
  (patch-controls (v (h sl (v tr pl))
                     (setq sep (separator :thickness 70))
                     (.trig-button))))
|#

(defclass patch-interface (capi:interface)
  ((bc-patch :initarg :patch :accessor bc-patch))
  (:default-initargs
   :destroy-callback #'(lambda (x) (stop-patch (bc-patch x)))
   :title "Patch control panel"
   :patch *current-patch*))

; (defparameter sepr-color (color:make-gray 0.5))

(defun separator (&rest rest &key
                        (color :gray)
                        (thickness 2)
                        (external 10))
  (apply #'make-instance 'capi:output-pane
         :allow-other-keys t
         :background color
         :external-min-height external
         :visible-max-height thickness
         :visible-min-height thickness rest))

; (separator)

(defun patch-controls (items &rest rest &key (screen-x 100) (screen-y 100)
                             &allow-other-keys)
  (let* ((lay (make-instance 'capi:column-layout
                             :description
                             (list items
                                   (separator)
                                   (make-start-stop-buttons)))))
    (capi:display
     (apply #'make-instance 'patch-interface
            :layout lay
            :best-x screen-x :best-y screen-y :allow-other-keys t rest))))

#|
(defparameter sx (.slider :name "F0:" :init 300.0))
(capi:contain sx)

(defparameter sx
 (.slider :name "F0:" :init 300.0
          :show-mapper #'(lambda (x) (* x x))))

(setf (capi:range-slug-start sx) 100.0)

(defpatch koe ((s (.slider :name "FO" :init 300)))
  (-> s (.neg) (.probe "out"))
  (patch-controls s))

(load-patch koe)
(step-patch koe)

(defpatch sine ((f (.slider :name "Freq:" :start 100.0 :stop 1000.0 :init 300.0))
                (a (.slider :name "Ampl:" :start 0.0 :stop 1.0 :init 0.3 :format "~5,3f"))
                (so (.sin-osc :ampl (.smooth a) :freq (.smooth f)))
                (da (.da)))
  (setq sl a)
  (-> so (inputs da))
  (patch-controls (sliders f a) :title (format nil "Sinewave oscillator")))

(run-patch sine)
(stop-patch sine)

(defun xx (n)
  (dotimes (i n) (+ x x)))
(defun xx (n)
  (dotimes (i n)
    (gp:draw-line sep 1.0f0 1.0f0 100.0f0 10.0f0)))
(defun xx (n)
  (dotimes (i n)
    (gp:draw-line sep 1.0 1.0 100.0 10.0)))
(defun xx (n)
  (dotimes (i n)
    (gp:draw-line sep 1 1 100 i)))
(defparameter pts (make-array 2000 
                              :element-type 'double-float 
                              ; :element-type '(signed-byte 32)
                              :initial-element 1.0
                              ))
(defun xx (n)
  (dotimes (i n)
    (gp:draw-points sep pts)))

(time (xx 200))

(defparameter xz 100.0)

|#


(provide :BC-controls)
