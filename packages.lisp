(in-package :cl-user)

(defpackage :fetch-quest
  (:use :cl :recurse.vert))

(defpackage :test-fetch-quest
  (:use :prove :cl :fetch-quest))
