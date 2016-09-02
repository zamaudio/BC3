
(in-package :BC)


(defclass .string-loop (one-port-macro) ()
  (:default-initargs
   :domain 'electric))


(defmethod initialize-macro-block ((b .string-loop) &key
                                   (y (/ 1.0 0.67)) ; z-equiv
                                   (g 0.998)
                                   (a 0.4)
                                   (dmax 512)
                                   (d 512))
  (let* ((pb (make-instance '.wa-port-block))
         (p (port pb))
         (inv (.inv))
         (in (.sub))
         (dl (.delay :order 5 :decimate 10
                     :max-length dmax))
         (gain (.p. :name 'gain))
         (gmul (.mul t gain))
         (lp (.lp1u :a a))
         (adm (.p. :name 'y))
         (dd (.p. :name 'd)))
    (add-param b g (param gain))
    (add-param b y (param adm))
    (add-param b d (param dd))
    (-> adm (param pb 'z))
    (-> adm inv (param pb 'y))
    (-> dd (param dl))
    (-> (out pb) gmul lp dl (in in 1) (in pb))
    (setf (inputs b) (list (in in 0)))
    (setf (domain p) (domain b))
    (setf (ports b) (list p))))


(defclass .string-unit (one-port-macro ; macro-block  
                        capi:column-layout)
  () ; ((controls :initform nil :accessor controls))
  (:default-initargs
   :domain 'electric))

(defun .string-unit (&rest rest)
  (apply #'make-instance '.string-unit rest))

(defmethod initialize-macro-block ((b .string-unit) &key
                                   string-name
                                   max-length
                                   min-length
                                   level
                                   (a0 0.4)
                                   admit)
  (let* ((as (.slider :name (format nil "~a string loop param a:" string-name)
                      :start 0.0 :stop 1.0
                      :init a0 :format "~5,3f"
                      :size 215))
         (gs (.slider :name (format nil "~a string loop param g:" string-name)
                      :start 0.98 :stop 1.0
                      :init 0.997 :format "~5,3f"
                      :size 215))
         (len (.slider :name (format nil "~a string loop delay:" string-name)
                       :start min-length :stop max-length
                       :init max-length :size 512))
         (pl (.trig-button :text "Pluck" :name (format nil "P~a" string-name)))
         (sl (make-instance '.string-loop :a as :g gs :dmax max-length :d len :y admit))
         (rt (.rtable (list level 0.0)))
         (iir (.iir :bs '(0.024851082301633   0.074553246904900   0.074553246904900   0.024851082301633)
                    :as '(1.000000000000000  -1.598451000715660   1.029462324383925  -0.232202665255198)))
         (sub (.sub)))
    (-> pl (param rt) iir sub (.lp1u :a 0.9) sl)
    (-> iir (.delay :length 120.0 ; (* 0.3 max-length)
                    ) (.coeff 0.9) (in sub 1))
    (setf (ports b) (list (port sl)))
    (setf (capi:layout-description b)
          (list (v len (h as gs pl))))))


#|

;;; 6 strings, bridge velocity output, order 26
;;; -------------------------------------------

(defpatch strs ((s1 (.string-unit :string-name "E4 (1st)" :admit 6.10 :level 1300.0
                                  :a0 0.134 :max-length 134.0 :min-length 58.2))
                (s2 (.string-unit :string-name "B3 (2nd)" :admit 6.02 :level 1000.0
                                  :a0 0.288 :max-length 179.0 :min-length 77.6))
                (s3 (.string-unit :string-name "G3 (3th)" :admit 4.36 :level 900.0
                                  :max-length 225.0 :min-length 97.8))
                (s4 (.string-unit :string-name "D3 (4th)" :admit 2.68 :level 700.0
                                  :max-length 300.0 :min-length 130.0))
                (s5 (.string-unit :string-name "A2 (5th)" :admit 1.93 :level 500.0
                                  :max-length 401.0 :min-length 174.0))
                (s6 (.string-unit :string-name "E2 (6th)" :admit 1.49 :level 500.0
                                  :max-length 535.0 :min-length 232.0))
                (br (y2*26)))
  (.par s1 s2 s3 s4 s5 s6 br)
  (-> (.voltage br) (inputs (.da)))
  (-> (.voltage br) (.probe "out"))
  (patch-controls (v s1 (separator) s2 (separator) s3 (separator)
                     s4 (separator) s5 (separator) s6)
                  :screen-y 50 :title "Guitar model with 6 strings, bridge order 26"))


;;; 6 strings, bridge velocity output, order 40
;;; -------------------------------------------

(defpatch strs ((s1 (.string-unit :string-name "E4 (1st)" :admit 6.10 :level 1300.0
                                  :a0 0.134 :max-length 134.0 :min-length 58.2))
                (s2 (.string-unit :string-name "B3 (2nd)" :admit 6.02 :level 1000.0
                                  :a0 0.288 :max-length 179.0 :min-length 77.6))
                (s3 (.string-unit :string-name "G3 (3th)" :admit 4.36 :level 900.0
                                  :max-length 225.0 :min-length 97.8))
                (s4 (.string-unit :string-name "D3 (4th)" :admit 2.68 :level 700.0
                                  :max-length 300.0 :min-length 130.0))
                (s5 (.string-unit :string-name "A2 (5th)" :admit 1.93 :level 500.0
                                  :max-length 401.0 :min-length 174.0))
                (s6 (.string-unit :string-name "E2 (6th)" :admit 1.49 :level 500.0
                                  :max-length 535.0 :min-length 232.0))
                (br (y2*40)))
  (.par s1 s2 s3 s4 s5 s6 br)
  (-> (.voltage br) (inputs (.da)))
  (-> (.voltage br) (.probe "out"))
  (patch-controls (v s1 (separator) s2 (separator) s3 (separator)
                     s4 (separator) s5 (separator) s6)
                  :screen-y 50 :title "Guitar model with 6 strings, bridge order 40"))


;;; 6 strings, bridge velocity output, order 112
;;; --------------------------------------------

(defpatch strs ((s1 (.string-unit :string-name "E4 (1st)" :admit 6.10 :level 1300.0
                                  :a0 0.134 :max-length 134.0 :min-length 58.2))
                (s2 (.string-unit :string-name "B3 (2nd)" :admit 6.02 :level 1000.0
                                  :a0 0.288 :max-length 179.0 :min-length 77.6))
                (s3 (.string-unit :string-name "G3 (3th)" :admit 4.36 :level 900.0
                                  :max-length 225.0 :min-length 97.8))
                (s4 (.string-unit :string-name "D3 (4th)" :admit 2.68 :level 700.0
                                  :max-length 300.0 :min-length 130.0))
                (s5 (.string-unit :string-name "A2 (5th)" :admit 1.93 :level 500.0
                                  :max-length 401.0 :min-length 174.0))
                (s6 (.string-unit :string-name "E2 (6th)" :admit 1.49 :level 500.0
                                  :max-length 535.0 :min-length 232.0))
                (br (y2*112)))
  (.par s1 s2 s3 s4 s5 s6 br)
  (-> (.voltage br) (inputs (.da)))
  (-> (.voltage br) (.probe "out"))
  (patch-controls (v s1 (separator) s2 (separator) s3 (separator)
                     s4 (separator) s5 (separator) s6)
                  :screen-y 50 :title "Guitar model with 6 strings, bridge order 112"))


;;; 6 strings, bridge force output, order 40
;;; ----------------------------------------

(defpatch strs ((s1 (.string-unit :string-name "E4 (1st)" :admit 6.10 :level 1300.0
                                  :a0 0.134 :max-length 134.0 :min-length 58.2))
                (s2 (.string-unit :string-name "B3 (2nd)" :admit 6.02 :level 1000.0
                                  :a0 0.288 :max-length 179.0 :min-length 77.6))
                (s3 (.string-unit :string-name "G3 (3th)" :admit 4.36 :level 900.0
                                  :max-length 225.0 :min-length 97.8))
                (s4 (.string-unit :string-name "D3 (4th)" :admit 2.68 :level 700.0
                                  :max-length 300.0 :min-length 130.0))
                (s5 (.string-unit :string-name "A2 (5th)" :admit 1.93 :level 500.0
                                  :max-length 401.0 :min-length 174.0))
                (s6 (.string-unit :string-name "E2 (6th)" :admit 1.49 :level 500.0
                                  :max-length 535.0 :min-length 232.0))
                (br (y2*40)))
  (.par s1 s2 s3 s4 s5 s6 br)
  (-> (.current br) (.coeff 0.01) (inputs (.da)))
  (-> (.current br) (.coeff 0.01) (.probe "out"))
  (patch-controls (v s1 (separator) s2 (separator) s3 (separator)
                     s4 (separator) s5 (separator) s6)
                  :screen-y 50 :title "Guitar model with 6 strings, force output"))


;;; 6 strings, bridge velocity output, resistive bridge
;;; ---------------------------------------------------

(defpatch strs ((s1 (.string-unit :string-name "E4 (1st)" :admit 6.10 :level 1300.0
                                  :a0 0.134 :max-length 134.0 :min-length 58.2))
                (s2 (.string-unit :string-name "B3 (2nd)" :admit 6.02 :level 1000.0
                                  :a0 0.288 :max-length 179.0 :min-length 77.6))
                (s3 (.string-unit :string-name "G3 (3th)" :admit 4.36 :level 900.0
                                  :max-length 225.0 :min-length 97.8))
                (s4 (.string-unit :string-name "D3 (4th)" :admit 2.68 :level 700.0
                                  :max-length 300.0 :min-length 130.0))
                (s5 (.string-unit :string-name "A2 (5th)" :admit 1.93 :level 500.0
                                  :max-length 401.0 :min-length 174.0))
                (s6 (.string-unit :string-name "E2 (6th)" :admit 1.49 :level 500.0
                                  :max-length 535.0 :min-length 232.0))
                (br (.R 0.01)))
  (.par s1 s2 s3 s4 s5 s6 br)
  (-> (.voltage br) (inputs (.da)))
  (patch-controls (v s1 (separator) s2 (separator) s3 (separator)
                     s4 (separator) s5 (separator) s6)
                  :screen-y 50 :title "Guitar model with 6 strings, bridge order 26"))


;;; DAMPINGS (g): 1:0.991, 2:0.987, 3:0.997, 4:0.986, 5:0.993, 6:0.997
|#
