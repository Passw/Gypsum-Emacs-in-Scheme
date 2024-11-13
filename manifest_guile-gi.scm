(specifications->manifest
 '("guile"
   "slib"
   "guile-chickadee"
   "guile-gi"
   "guile-goblins"
   "guile-lib"
   "glib:debug"
   "gtk+"

   ;;"gtk+@2:debug"
   ;;"gdb"
   ;;"guile-g-golf"
   ;;"gtk+"

   ;; *** Note
   ;; guile-g-golf does not install Gtk by default, and the Guix
   ;; guile-g-golf package is limited to an old version of libglib
   ;; which only supports Gtk version 3.
   ))
