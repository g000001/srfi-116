;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user)


(defpackage :srfi-116
  (:use)
  (:export :iq)
  (:export :ipair :ilist :xipair :ipair* :make-ilist :ilist-tabulate :iiota)
  (:export :ipair?)
  (:export :proper-ilist? :ilist? :dotted-ilist? :not-ipair? :null-ilist? :ilist=)
  (:export :icar :icdr :ilist-ref)
  (:export :ifirst :isecond :ithird :ifourth :ififth :isixth :iseventh :ieighth :ininth :itenth)
  (:export :icaar :icadr :icdar :icddr)
  (:export :icaaar :icaadr :icadar :icaddr :icdaar :icdadr :icddar :icdddr)
  (:export :icaaaar :icaaadr :icaadar :icaaddr :icadaar :icadadr :icaddar :icadddr)
  (:export :icdaaar :icdaadr :icdadar :icdaddr :icddaar :icddadr :icdddar :icddddr)
  (:export :icar+icdr :itake :idrop :ilist-tail)
  (:export :itake-right :idrop-right :isplit-at :ilast :last-ipair)
  (:export :ilength :iappend :iconcatenate :ireverse :iappend-reverse)
  (:export :izip :iunzip1 :iunzip2 :iunzip3 :iunzip4 :iunzip5)
  (:export :icount :imap :ifor-each :ifold :iunfold :ipair-fold :ireduce )
  (:export :ifold-right :iunfold-right :ipair-fold-right :ireduce-right )
  (:export :iappend-map :ipair-for-each :ifilter-map :imap-in-order)
  (:export :ifilter :ipartition :iremove :imember :imemq :imemv)
  (:export :ifind :ifind-tail :iany :ievery)
  (:export :ilist-index :itake-while :idrop-while :ispan :ibreak)
  (:export :idelete :idelete-duplicates )
  (:export :iassoc :iassq :iassv :ialist-cons :ialist-delete)
  (:export :replace-icar :replace-icdr)
  (:export :pair->ipair :ipair->pair :list->ilist :ilist->list)
  (:export :tree->itree :itree->tree :gtree->itree :gtree->tree)
  (:export :iapply))


(defpackage :srfi-116.internal
  (:use :srfi-116 :rnrs-compat-plus :named-readtables :mbe
        :srfi-9 :srfi-0 :srfi-23
        :fiveam
   ;; :srfi-64
   ))

