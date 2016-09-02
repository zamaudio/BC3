
(make-package :BC)
(in-package :bc)
(print "Loading BlockCompiler in CLISP")
(ext:cd "..")
(defvar *BC* "BC3")
(ext:cd *BC*)
(load "WINCL-FILES.lisp")
(load-BC)
(ext:cd "..")
(load "BC-SETTINGS.lisp")
(print "BlockCompiler loaded")

