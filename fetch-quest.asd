;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:fetch-quest-asd
  (:use :cl :asdf))

(in-package :fetch-quest-asd)

(defsystem fetch-quest
  :name "fetch-quest"
  :version "0.1"
  :author "Ark"
  :components ((:file "packages")
               (:file "src/fetch-quest"))
  :depends-on (#:vert))

(defsystem fetch-quest/test
  :name "fetch-quest/test"
  :description "Tests for fetch-quest"
  :pathname "t/"
  :serial t
  :depends-on (:prove :fetch-quest)
  :components ((:file "packages")
               (:file "test-fetch-quest"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
