(import
  (scheme base)
  (scheme write)
  (scheme load))

(cond-expand
  (guile-3
   (load "./main-guile.scm")
   )
  (else
   (display "Sorry, this scheme implementation does not appear to be supported.")
   (newline)
   ))
