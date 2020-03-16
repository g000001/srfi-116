;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-116"
  (:use)
  (:export iq)
  (:export ipair ilist xipair ipair* make-ilist ilist-tabulate iiota)
  (:export ipair?)
  (:export proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist=)
  (:export icar icdr ilist-ref)
  (:export ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth)
  (:export icaar icadr icdar icddr)
  (:export icaaar icaadr icadar icaddr icdaar icdadr icddar icdddr)
  (:export icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr)
  (:export icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr)
  (:export icar+icdr itake idrop ilist-tail)
  (:export itake-right idrop-right isplit-at ilast last-ipair)
  (:export ilength iappend iconcatenate ireverse iappend-reverse)
  (:export izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5)
  (:export icount imap ifor-each ifold iunfold ipair-fold ireduce )
  (:export ifold-right iunfold-right ipair-fold-right ireduce-right )
  (:export iappend-map ipair-for-each ifilter-map imap-in-order)
  (:export ifilter ipartition iremove imember imemq imemv)
  (:export ifind ifind-tail iany ievery)
  (:export ilist-index itake-while idrop-while ispan ibreak)
  (:export idelete idelete-duplicates )
  (:export iassoc iassq iassv ialist-cons ialist-delete)
  (:export replace-icar replace-icdr)
  (:export pair->ipair ipair->pair list->ilist ilist->list)
  (:export tree->itree itree->tree gtree->itree gtree->tree)
  (:export iapply))


(defpackage "https://github.com/g000001/srfi-116#internals"
  (:use 
   "https://github.com/g000001/srfi-116"
   "https://github.com/g000001/srfi-9"
   "https://github.com/g000001/srfi-0"
   "https://github.com/g000001/srfi-23"
   rnrs
   named-readtables
   mbe
   fiveam))


(cl:in-package "https://github.com/g000001/srfi-116#internals")


(cl:declaim 
 (cl:ftype cl:function 
           ialist-cons
           tree->itree
           icdaddr
           icddadr
           ialist-delete
           icdadar
           icdaaar
           replace-icdr
           ipair?
           replace-icar
           iunzip1
           ireverse
           ireduce-right
           proper-ilist?
           gtree->itree
           idrop-right
           idrop-while
           ifor-each
           idrop
           list->ilist
           iunzip4
           iunfold-right
           dotted-ilist?
           icdaadr
           ipair
           iconcatenate
           ifilter-map
           ilist-ref
           itake-right
           itake-while
           ipair-fold-right
           ipair-fold
           ilist-tail
           icdar
           ifilter
           icdddar
           icaaaar
           ibreak
           ilast
           ifind-tail
           iq
           iapply
           iassoc
           ilist?
           iappend-map
           imember
           iseventh
           icadddr
           icadadr
           ifind
           null-ilist?
           iunzip2
           icaadar
           icddaar
           idelete-duplicates
           ilist=
           ifold-right
           itenth
           iassq
           ithird
           itake
           iunfold
           iunzip5
           isplit-at
           pair->ipair
           iremove
           icddr
           itree->tree
           isecond
           ispan
           izip
           icdadr
           ipair*
           isixth
           iappend-reverse
           make-ilist
           not-ipair?
           icaaddr
           icaaadr
           gtree->tree
           icdddr
           icar
           icddar
           icaadr
           icaaar
           ipair->pair
           ilist-index
           ifold
           icaddr
           icadar
           ilist->list
           imap
           ilist-tabulate
           ififth
           ieighth
           ifirst
           ifourth
           icddddr
           iassv
           icdaar
           ievery
           iunzip3
           icount
           ipartition
           iappend
           icar+icdr
           icaar
           imap-in-order
           imemv
           imemq
           ireduce
           ilist
           icaddar
           icadaar
           ininth
           ilength
           ipair-for-each
           icdr
           iiota
           iany
           last-ipair
           xipair
           icadr
           idelete))


;;; *EOF*
