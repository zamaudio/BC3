
(make-package :BC)
(in-package :bc)
(print "Compiling BlockCompiler in CLISP")
(ext:cd "../BC3")
(load "WINCL-FILES.lisp")
(compile-BC)
(print "BlockCompiler compiled")

