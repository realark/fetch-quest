(ql:quickload :fetch-quest)

(in-package :fetch-quest)

(%pre-image-save)

(sb-ext:save-lisp-and-die
 (or
  #+win32"fetch-quest-windows.exe"
  #+linux"fetch-quest-linux"
  #+darwin"fetch-quest-osx"
  (error "Unsupported OS for building. Got: ~A" *features*))
 :purify T
 :toplevel
 #'run-fetch-quest
 :executable t
 ;; :application-type :gui
 :save-runtime-options t)
