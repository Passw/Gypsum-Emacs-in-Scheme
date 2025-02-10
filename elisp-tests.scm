(import
  (scheme base)
  (only (gypsum elisp-eval)
        elisp-load!
        elisp-eval!
        )
  )


(define elisp-ert-dependencies
  ;; This list of programs defines all of the dependencies of the
  ;; Emacs package "ERT" (Emacs Regression Tests). These are loaded
  ;; explicitly here until we get the "require" macro working
  ;; correctly.
  ;;------------------------------------------------------------------
  '("./elisp/lisp/subr.el"
    "./elisp/lisp/emacs-lisp/cl-lib.el"
    "./elisp/lisp/emacs-lisp/debug.el"
    "./elisp/lisp/emacs-lisp/backtrace.el"
    "./elisp/lisp/emacs-lisp/ewoc.el"
    "./elisp/lisp/emacs-lisp/find-func.el"
    "./elisp/lisp/emacs-lisp/pp.el"
    "./elisp/lisp/emacs-lisp/map.el"
    "./elisp/lisp/emacs-lisp/ert.el"
    ))
(for-each elisp-load! elisp-ert-dependencies)


(elisp-load! "./elisp/test/lisp/emacs-lisp/cl-lib-tests.el")
(elisp-eval!
 '(ert-run-tests-batch-and-exit
   '(not (or (tag :expensive-test)
             (tag :unstable)
             (tag :nativecomp)))))
