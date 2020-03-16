;;;; srfi-116.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :srfi-116
  :version "20200316"
  :description "SRFI 116 For CL: Immutable List Library"
  :long-description "SRFI 116 For CL: Immutable List Library
https://srfi.schemers.org/srfi-116"
  :author "John Cowan"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :rnrs-compat
               :mbe
               :srfi-0
               :srfi-9
               :srfi-23)
  :components ((:file "package")
               (:file "ilists-base")
               (:file "ilists-impl")
               (:file "ilist-test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-116))))
  (let ((name "https://github.com/g000001/srfi-116")
        (nickname :srfi-116))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-116))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-116#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-116)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))

;;; *EOF*
