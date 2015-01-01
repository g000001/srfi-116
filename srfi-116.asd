;;;; srfi-116.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :srfi-116
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :rnrs-compat-plus
               :mbe
               :srfi-0
               :srfi-9
               :srfi-23)
  :components ((:file "package")
               (:file "ilists-base")
               (:file "ilists-impl")
               (:file "ilist-test")))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-116))))
  (load-system :srfi-116)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) ($ :srfi-116.internal :srfi-116))))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
