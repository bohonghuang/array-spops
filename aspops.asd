(defsystem aspops
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "Fast operations for Common Lisp specialized arrays."
  :homepage "https://github.com/bohonghuang/aspops"
  :bug-tracker "https://github.com/bohonghuang/aspops/issues"
  :source-control (:git "https://github.com/bohonghuang/aspops.git")
  :components ((:file "package")
               (:file "array" :depends-on ("package"))
               (:file "vector" :depends-on ("package")))
  :depends-on (#:alexandria)
  :in-order-to ((test-op (test-op #:aspops/test))))

(defsystem aspops/test
  :depends-on (#:asdf #:parachute #:aspops)
  :components ((:file "test"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:aspops.test))))
