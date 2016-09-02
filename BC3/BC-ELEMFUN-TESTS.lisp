
(in-package :BC)


#|
(inspect (.extfun :mfun "koe"))
(inspect (.extfun2 :mfun "koe"))
|#


#|
(inspect (.mean))
(inspect (.mean :inputs 2))
(inspect (.mean (.var 1.0) (.var 2.0)))

(defpatch p ((m (.mean)))
  (-> (.imp1 1.0) (in m 0))
  (-> (.var 2.0) (in m 1))
  (-> m (.probe "out")))

(defpatch p ((x1 1.0)
             (x2 2.0))
  (-> (.mean x1 x2 :out-type '.float)
      (.probe "out")))

(defpatch p ((x1 (.imp1 '((1.0 3.0) (5.0 -5.0))))
             (x2 (.var '((2.0 1.0) (4.0 10.0)))))
  (-> (.mean x1 x2) (.probe "out")))

(to-matlab p)
|#

#|
(defpatch p ((add (.add t 1.0)))
  (-> (.var 1.0) (in add 0) (.probe "out")))

(defpatch p ((add (.add))) ;;; NOT SUPPPORTED
  (-> (.var 1.0) (in add 0))
  (-> (.var 1.0) (in add 1))
  (-> (.var 1.0) (in add 'x t)))


(defpatch p ((x1 (.var '(1.0 3.0)))
             (x2 (.var '(2.0 1.0)))
             (x3 (.var '(0.0 0.0))))
  (-> (.add x1 x2 x3) (.probe "out")))

(inspect (.max))

(defpatch p ((m (.max)))
  (-> (.var 1.0) (in m 0))
  (-> (.var 2.0) (in m 1))
  (-> m (.probe "out")))

(defpatch p ((x1 (.imp1 '((1.0 3.0) (5.0 -5.0))))
             (x2 (.var '((2.0 1.0) (4.0 10.0)))))
  (-> (.max x1 x2) (.probe "out")))

(defpatch p ((x1 (.imp1 '(1.0 3.0)))
             (x2 (.var '(2.0 1.0)))
             (x3 (.var '(0.0 0.0))))
  (-> (.max x1 x2 x3) (.probe "out")))
  
(inspect p)
(c-code p)
(load-patch p)
(step-patch p)
(to-matlab p)
(value (out (elt (block-items p) 0)))
(cprint.data (lvar (out (elt (block-items p) 0))) t)
|#

#|
(inspect (.source "aa"))

(defparameter px
  (patch ()
    (-> (.source "in1") (.x) (.neg) (.x) (.probe "out1"))))

(defparameter px
  (patch ()
    (-> (.var 1.0) (.neg) (.x) (.probe "out1"))))

(defparameter px
  (patch ()
    (-> (.var 1.0) (.neg) (.d) (.x) (.probe "out1"))))

(defparameter px
  (patch ()
    (-> (.const 1.0) (.neg) (.probe "out1"))))

(defparameter px
  (patch ()
    (-> (.const 1.0) (.d) (.neg) (.probe "out1"))))

(inspect px)
(funcall (lstepfun px))
(to-matlab px)

(report-schedule px)
(report-schedule px t)
|#

#|
(inspect (.and))

(defpatch ap ((or (.or)))
  (-> (.var 1) or (.probe "out"))
  (-> (.var 3) (in or 1)))

(defpatch ap ((and (.and)))
  (-> (.var 1) and (.probe "out"))
  (-> (.var 3) (in and 1)))

(inspect ap)
(c-code ap)
(compile-patch ap)
(step-patch ap)
(to-matlab ap)
|#

#|
(defparameter pneg
  (patch ((v1 (.float 1.2)))
    (-> v1 (.neg :name 'neg)
        (.probe "out"))))

(defparameter pneg
  (patch ((v1 (.float 1.2)))
    (-> v1 (.neg :name 'neg)
        (.probe "out"))))

(defparameter pneg
  (patch ()
    (-> (.neg (.float 1.2))
        (.probe "out"))))

(defparameter pneg
  (patch ()
    (-> (.neg (.float 1.2) :name 'neg)
        (.probe "out"))))

(defparameter pneg
  (patch ((v1 (.float 1.2)))
    (-> (.neg v1 :name 'neg)
        (.probe "out"))))

(defparameter pneg
  (patch ()
    (-> (.neg 1.3 :name 'neg)
        (.probe "out"))))

(defparameter pneg
  (patch ()
    (.neg (.neg 1.3 :name 'neg) :name 'out)))

(inspect pneg)

(lcode (find-block pneg 'neg))
(eval (lcode (find-block pneg 'neg)))
|#

#|
(inspect (.select-c 0))

(defparameter sp
  (patch ((in (.var '(1.0 2.0 3.0))))
    (-> in (.select-c 1 :name 'sel) (.probe "out"))))

(inspect sp)
(to-matlab sp)
(lcode (find-block sp 'sel))
|#

#|
(inspect (.imp1))
(inspect (.imp1 2.0))
(inspect (.imp1 '(1.0 2.0)))

(defparameter px
  (patch ()
    (-> (.imp1 '(1.0 2.0)) (.probe "out"))))

(defparameter px
  (patch ()
    (-> (.imp1 '(2.0 3.0)) (.probe "out"))))
(to-matlab px)
(inspect px)

(c-code* (elt (block-items px) 1))
|#

#|
(defpatch xp ((log (.log*)))
  (-> (.var -2.0) log (.probe "out")))
(inspect xp)
(step-patch xp)
|#

#|
(defparameter ro (.ramp-sawtooth :limit (* 2 pi)))
(defparameter ro (.ramp-sawtooth))
(inspect ro)
(inspect (.ramp-sawtooth :out-init 0.5))

(defparameter px
  (patch ((c (.double 0.1))
          (r (.ramp-sawtooth)))
    (-> c (in r) (.probe "out"))))

(inspect px)
(mcode (first (block-items px)))
(c-code px)
(compile-patch px)
(load-patch px)
(step-patch px)
(reader-access (out (elt (block-items px) 0)))
(step-patch px 1000000 t)
(step-patch-n px 90000000 t)

(defparameter px
  (patch ()
    (-> (.imp1 1.0) (.neg) (.x) (.probe "out"))))

(defparameter px
  (patch ()
    (-> (.imp1 '((1.0 2.0) (3.0 4.0))) (.neg) (.x) (.probe "out1"))))
|#

#|
(inspect (.sawtooth-osc :freq 1000.0 :ampl 1.0 :mrate 1))

(defpatch sp ((so (.sawtooth-osc :freq 1000.0 :ampl 1.0 :mrate 1/2)))
  (-> so (.probe "out")))

(defpatch sp ((so (.sawtooth-osc :freq 1000.0 :ampl 1.0)))
  (-> so (.probe "out")))

(defpatch sp ()
  (-> (.sawtooth-osc :freq 1000.0 :ampl 1.0) (.probe "out")))

(c-code sp)
(load-patch sp)
|#

#|
(inspect (.sin-osc :freq 1000.0 :ampl 1.0))

(defpatch so ((osc (.sin-osc :ampl 1.0 :freq 1000.0)))
  (-> osc (.probe "out")))

(defpatch so ((osc (.sin-osc :freq 1000.0)))
  (-> osc (.probe "out")))

(defpatch so ((osc (.sin-osc :freq '(1000.0 1500.0) :ampl '(0.5 0.6))))
  (-> osc (.probe "out")))

(defpatch so ((a 1.0)
              (f 1000.0)
              (osc (.sin-osc :ampl a :freq f)))
  (-> osc (.probe "out")))

(defpatch so ((a (.var 1.0))
              (f (.var 1000.0))
              (osc (.sin-osc :ampl a :freq f)))
  (-> osc (.probe "out")))

(defpatch so ((a (.var 1.0))
              (f (.var 1000.0))
              (osc (.sin-osc :ampl t)))
  (-> a (param osc 'ampl))
  (-> f (param osc 'freq))
  (-> osc (.probe "out")))

(inspect so)
(c-code so)
(load-patch so)
(step-patch so)
(to-matlab so)
|#

#|
; (inspect (make-instance '.coeff :coeff 1.0 :coeff-type '.double))
; (inspect (.coeff 1.0 :coeff-type '.float))
; (inspect (.coeff 1.0 :coeff-type '.float  :out-type '.double))
(defparameter c (.coeff 1.0))
(inspect c)
(host-patch (host-patch (host-block (in c))))
(top-host-block (in c))
|#

#|
(defparameter dp
  (patch ((d (.d :name 'd)))
    (-> (.imp1) d (.probe "out"))))
(lcode (find-block dp 'd))
|#

#|
(inspect (make-instance '.dx% :order 5))

(defparameter dxp
  (patch ((d (make-instance '.dx% :order 3)))
    (-> (.var 0.01) (param d))
    (-> (.var 1.0) d (.probe "out"))))

(defparameter dxp
  (patch ((d (.dt :delay 0.01 :order 5)))
    (-> (.var 1.0) d (.probe "out"))))

(defparameter dxp
  (patch ((d (.dt 0.01 :order 5)))
    (-> (.var 1.0) d (.probe "out"))))

(defparameter dxp
  (patch ((d (.dt)))
    (-> (.var 0.0001) (param d))
    (-> (.var 1.0) d (.probe "out"))))

(inspect dxp)
(funcall (lstepfun dxp))

(defparameter dxp
  (patch ((d (.dx)))
    (-> (.const 3.5) (param d))
    (-> (.var 1.0) d (.probe "out"))))

(defparameter dxp
  (patch ((d (.dx 3.5)))
    (-> (.var 1.0) d (.probe "out"))))
|#



#|
(matlab-response ip :samples 10
                 :outputs '("out")
                 :post "
out_resp")
                 

(inspect (.dn 100))

(defparameter dnp
  (patch ((x (.imp1))
          (dn (.dn 2)))
    (-> x dn (.probe "out"))))

(defparameter dnp
  (patch ((x (.var '(1.0 2.0)))
          (dn (.dn '(2 3))))
    (-> x dn (.probe "out"))))

(defparameter dnp
  (patch ((x (.imp1 '(1.0 2.0)))
          (dn (.dn '(2 3))))
    (-> x dn (.probe "out"))))

(inspect dnp)
(used-variables dnp)
(to-matlab dnp)

(defparameter bx (make-instance '.neg))

(inspect bx)

(defparameter n1 (make-instance '.neg))
(defparameter n2 (make-instance '.neg))
(connect (out n1) (in n2))
(-> n1 n2)
(inspect n1)

(defparameter px
  (patch ((z1 (.double '(12.0 13)))
          (n1 (make-instance '.neg :out-type '.double))
          (n2 (make-instance '.neg)))
    (-> z1 n1 n2)))

(defparameter px
  (patch ((z1 (.double '(12.0 13)))
          (x1 (.x)) (x2 (.x))
          (n (make-instance '.neg)))
    (-> z1 x1 x2 n)))

(inspect px)

(defparameter px
  (patch ((z1 (.float '12.0))
          (mx (.add :out-init 11.0)))
    (-> z1 mx) (-> z1 (in mx 1))))

(inspect px)

(defparameter px
  (patch ((x (.float '(12.0 13)))
          (d (.d))
          (add (.add)))
    (-> x add d (in add 1))))

(defparameter px
  (patch ((x (.double '(12.0 13) :varname 'IN))
          (d (.d :name 'd)) ;;; :out-type '.float))
          (c (.coeff '(0.5 0.9) :out-type '.float))
          (add (.add :varname 'OUT)))
    (-> x add d c (in add 1))))

(lcode (find-block px 'd))
(dolist (b (lin-schedule px)) (print (lcode b)))
(step-form px)
(eval (lcode (find-block px 'd)))

(step-form px)
(make-stepfun px)
(funcall (lstepfun px))
(time (dotimes (i 100000) (funcall (lstepfun px))))

(inspect (make-instance '.ap1))

(defparameter px
  (patch ((x (.var '(1.0 2.0)))
          (d (.ap1)))
    (-> (.const '(0.5 0.6)) (param d))
    (-> x d (.probe "out"))))

(defparameter px
  (patch ((x (.var '(1.0 2.0)))
          (d (.ap1 (.const '(0.5 0.6)))))
    (-> x d (.probe "out"))))

(defparameter px
  (patch ((x (.var '(1.0 2.0)))
          (d (.ap1 '(0.5 0.6))))
    (-> x d (.probe "out"))))

(defparameter px
  (patch ((x (.source "in" :value '(1.0 2.0)))
          (d (make-instance '.ap1m :alpha '(0.2 0.0))))
    (-> x d (.probe "out"))))

(inspect px)
(to-matlab px)
|#

#|
(defparameter ix (.integ :coeff 0.9))
(defparameter ix (.integ (.var 1.0) :coeff 0.5))
(inspect ix)

(defparameter ip
  (patch ()
    (-> (.integ 1.0) (.probe "out"))))

(defparameter ip
  (patch ()
    (-> (.integ 2.0 :value 1.0) (.probe "out"))))

(inspect ip)

(to-matlab ip)

(defparameter ip
  (patch ()
    (-> (.diff 1.0) (.probe "out"))))
|#

