(in-package :cl-user)

(defpackage :test-fetch-quest
  (:use :prove :cl :fetch-quest))

(in-package :test-fetch-quest)

(deftest fetch-quest-test
  (let ((fetch-quest 'fun))
    (is fetch-quest 'fun)))
