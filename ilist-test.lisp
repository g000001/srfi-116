(cl:in-package :srfi-116.internal)


(in-readtable :rnrs+)


(def-suite srfi-116)


(in-suite srfi-116)


(define (iequal? x y)
  (or (equal? x y)
      (ilist= x y)))


(define (truep obj)
  (cl:not (cl:eq #f obj)))


(define-syntax ==
  (syntax-rules ()
    ((== x y)
     (is-true (truep (iequal? x y))))))


(define-syntax ass
  (syntax-rules ()
    ((ass x)
     (is-true (truep (cl:eq #t x))))))


(test ilists/constructors
  (let ((abc (ilist 'a 'b 'c)))
  (== 'a (icar abc))
  (== 'b (icadr abc))
  (== 'c (icaddr abc))
  (== (ipair 2 1) (xipair 1 2)))
  (let ((abc-dot-d (ipair* 'a 'b 'c 'd)))
  (== 'd (icdddr abc-dot-d))
  (== (iq c c c c) (make-ilist 4 'c))
  (== (iq 0 1 2 3) (ilist-tabulate 4 #'values))
  (== (iq 0 1 2 3 4) (iiota 5))))


(test ilists/predicates
  (ass (ipair? (ipair 1 2)))
  (ass (proper-ilist? '()))
  (ass (proper-ilist? (iq 1 2 3)))
  (ass (ilist? '()))
  (ass (ilist? (iq 1 2 3)))
  (ass (dotted-ilist? (ipair 1 2)))
  (ass (dotted-ilist? 2))
  (ass (null-ilist? '()))
  (ass (not (null-ilist? (iq 1 2 3)))) 
  (signals cl:error (null-ilist? 'a))
  (ass (not-ipair? 'a))
  (ass (not (not-ipair? (ipair 'a 'b))))
  (ass (ilist= #'= (iq 1 2 3) (iq 1 2 3)))
  (ass (not (ilist= #'= (iq 1 2 3 4) (iq 1 2 3))))
  (ass (not (ilist= #'= (iq 1 2 3) (iq 1 2 3 4))))
  (ass (ilist= #'= (iq 1 2 3) (iq 1 2 3)))
  (ass (not (ilist= #'= (iq 1 2 3) (iq 1 2 3 4) (iq 1 2 3 4))))
  (ass (not (ilist= #'= (iq 1 2 3) (iq 1 2 3) (iq 1 2 3 4)))))


(test ilist/cxrs
  (let* ((.ab (ipair 'a 'b))
         (.cd (ipair 'c 'd))
         (.ef (ipair 'e 'f))
         (.gh (ipair 'g 'h))
         (.abcd (ipair .ab .cd))
         (.efgh (ipair .ef .gh))
         (.abcdefgh (ipair .abcd .efgh))
         (.ij (ipair 'i 'j))
         (.kl (ipair 'k 'l))
         (.mn (ipair 'm 'n))
         (.op (ipair 'o 'p))
         (.ijkl (ipair .ij .kl))
         (.mnop (ipair .mn .op))
         (.ijklmnop (ipair .ijkl .mnop))
         (.abcdefghijklmnop (ipair .abcdefgh .ijklmnop)))
  (== 'a (icaar .abcd))
  (== 'b (icdar .abcd))
  (== 'c (icadr .abcd))
  (== 'd (icddr .abcd))
  (== 'a (icaaar .abcdefgh))
  (== 'b (icdaar .abcdefgh))
  (== 'c (icadar .abcdefgh))
  (== 'd (icddar .abcdefgh))
  (== 'e (icaadr .abcdefgh))
  (== 'f (icdadr .abcdefgh))
  (== 'g (icaddr .abcdefgh))
  (== 'h (icdddr .abcdefgh))
  (== 'a (icaaaar .abcdefghijklmnop))
  (== 'b (icdaaar .abcdefghijklmnop))
  (== 'c (icadaar .abcdefghijklmnop))
  (== 'd (icddaar .abcdefghijklmnop))
  (== 'e (icaadar .abcdefghijklmnop))
  (== 'f (icdadar .abcdefghijklmnop))
  (== 'g (icaddar .abcdefghijklmnop))
  (== 'h (icdddar .abcdefghijklmnop))
  (== 'i (icaaadr .abcdefghijklmnop))
  (== 'j (icdaadr .abcdefghijklmnop))
  (== 'k (icadadr .abcdefghijklmnop))
  (== 'l (icddadr .abcdefghijklmnop))
  (== 'm (icaaddr .abcdefghijklmnop))
  (== 'n (icdaddr .abcdefghijklmnop))
  (== 'o (icadddr .abcdefghijklmnop))
  (== 'p (icddddr .abcdefghijklmnop))))


(test ilists/selectors
  (== 'c (ilist-ref (iq a b c d) 2))
  (let ((.ten (ilist 1 2 3 4 5 6 7 8 9 10)))
  (== 1 (ifirst .ten))
  (== 2 (isecond .ten))
  (== 3 (ithird .ten))
  (== 4 (ifourth .ten))
  (== 5 (ififth .ten))
  (== 6 (isixth .ten))
  (== 7 (iseventh .ten))
  (== 8 (ieighth .ten))
  (== 9 (ininth .ten))
  (== 10 (itenth .ten))
  (signals cl:error (ilist-ref '() 2)) ;
  (== '(1 2) (call-with-values (lambda () (icar+icdr (ipair 1 2))) #'list))
  (let ((.abcde (iq a b c d e))
        (.dotted (ipair 1 (ipair 2 (ipair 3 'd)))))
  (== (iq a b) (itake .abcde 2))
  (== (iq c d e) (idrop .abcde 2))
  (== (iq c d e) (ilist-tail .abcde 2))
  (== (iq 1 2) (itake .dotted 2))
  (== (ipair 3 'd) (idrop .dotted 2))
  (== (ipair 3 'd) (ilist-tail .dotted 2))
  (== 'd (idrop .dotted 3))
  (== 'd (ilist-tail .dotted 3))
  (== .abcde (iappend (itake .abcde 4) (idrop .abcde 4)))
  (== (iq d e) (itake-right .abcde 2))
  (== (iq a b c) (idrop-right .abcde 2))
  (== (ipair 2 (ipair 3 'd)) (itake-right .dotted 2))
  (== (iq 1) (idrop-right .dotted 2))
  (== 'd (itake-right .dotted 0))
  (== (iq 1 2 3) (idrop-right .dotted 0))
  (== .abcde (call-with-values (lambda () (isplit-at .abcde 3)) #'iappend))
  (== 'c (ilast (iq a b c)))
  (== (iq c) (last-ipair (iq a b c))))))


(test ilists/misc
  (== 0 (ilength '()))
  (== 3 (ilength (iq 1 2 3)))
  (== (iq x y) (iappend (iq x) (iq y)))
  (== (iq a b c d) (iappend (iq a b) (iq c d)))
  (== (iq a) (iappend '() (iq a)))
  (== (iq x y) (iappend (iq x y)))
  (== '() (iappend))
  (== (iq a b c d) (iconcatenate (iq (a b) (c d))))
  (== (iq c b a) (ireverse (iq a b c)))
  (== (iq (e (f)) d (b c) a) (ireverse (iq a (b c) d (e (f)))))
  (== (ipair 2 (ipair 1 'd)) (iappend-reverse (iq 1 2) 'd))
  (== (iq (one 1 odd) (two 2 even) (three 3 odd))
      (izip (iq one two three) (iq 1 2 3) (iq odd even odd)))
  (== (iq (1) (2) (3)) (izip (iq 1 2 3)))
  (== (iq 1 2 3) (iunzip1 (iq (1) (2) (3))))
  (== (iq (1 2 3) (one two three))
      (call-with-values
       (lambda () (iunzip2 (iq (1 one) (2 two) (3 three))))
       #'ilist))
  (== (iq (1 2 3) (one two three) (a b c))
      (call-with-values
       (lambda () (iunzip3 (iq (1 one a) (2 two b) (3 three c))))
       #'ilist))
  (== (iq (1 2 3) (one two three) (a b c) (4 5 6))
      (call-with-values
       (lambda () (iunzip4 (iq (1 one a 4) (2 two b 5) (3 three c 6))))
       #'ilist))
  (== (iq (1 2 3) (one two three) (a b c) (4 5 6) (#t #f #t))
      (call-with-values
       (lambda () (iunzip5 (iq (1 one a 4 #t) (2 two b 5 #f) (3 three c 6 #t))))
       #'ilist))
  (== 3 (icount #'even? (iq 3 1 4 1 5 9 2 5 6)))
  (== 3 (icount #'< (iq 1 2 4 8) (iq 2 4 6 8 10 12 14 16))))


(test ilists/folds
  ;; We have to be careful to == both single-list and multiple-list
  ;; code paths, as they are different in this implementation.
  (let ((.lis (iq 1 2 3)))
  (== 6 (ifold #'+ 0 .lis))
  (== (iq 3 2 1) (ifold #'ipair '() .lis))
  (== 2 (ifold
         (lambda (x count) (if (symbol? x) (+ count 1) count))
         0
         (iq a 0 b)))
  (== 4 (ifold
         (lambda (s max-len) (max max-len (string-length s)))
         0
         (iq "ab" "abcd" "abc")))
  (== 32 (ifold
          (lambda (a b ans) (+ (* a b) ans))
          0
          (iq 1 2 3)
          (iq 4 5 6)))
  (cl:flet ((z (x y ans) (ipair (ilist x y) ans)))
  (== (iq (b d) (a c))
      (ifold #'z '() (iq a b) (iq c d)))
  (== .lis (ifold-right #'ipair '() .lis))
  (== (iq 0 2 4) (ifold-right
                  (lambda (x l) (if (even? x) (ipair x l) l))
                  '()
                  (iq 0 1 2 3 4)))
  (== (iq (a c) (b d))
      (ifold-right #'z '() (iq a b) (iq c d)))
  (== (iq (c) (b c) (a b c))
      (ipair-fold #'ipair '() (iq a b c)))
  (== (iq ((b) (d)) ((a b) (c d)))
      (ipair-fold #'z '() (iq a b) (iq c d)))
  (== (iq (a b c) (b c) (c))
      (ipair-fold-right #'ipair '() (iq a b c)))
  (== (iq ((a b) (c d)) ((b) (d)))
      (ipair-fold-right #'z '() (iq a b) (iq c d)))
  (== 5 (ireduce #'max 0 (iq 1 3 5 4 2 0)))
  (== 1 (ireduce #'- 0 (iq 1 2)))
  (== -1 (ireduce-right #'- 0 (iq 1 2)))
  (let ((.squares (iq 1 4 9 16 25 36 49 64 81 100)))
        
        
  (== .squares
      (iunfold (lambda (x) (> x 10))
               (lambda (x) (* x x))
               (lambda (x) (+ x 1))
               1))
  (== .squares
      (iunfold-right #'zero? 
                     (lambda (x) (* x x))
                     (lambda (x) (- x 1))
                     10))
  (== (iq 1 2 3) (iunfold #'null-ilist? #'icar #'icdr (iq 1 2 3)))
  (== (iq 3 2 1) (iunfold-right #'null-ilist? #'icar #'icdr (iq 1 2 3)))
  (== (iq 1 2 3 4)
      (iunfold #'null-ilist? #'icar #'icdr (iq 1 2) (lambda (x) x (iq 3 4))))
  (== (iq b e h) (imap #'icadr (iq (a b) (d e) (g h))))
  (== (iq b e h) (imap-in-order #'icadr (iq (a b) (d e) (g h))))
  (== (iq 5 7 9) (imap #'+ (iq 1 2 3) (iq 4 5 6)))
  (== (iq 5 7 9) (imap-in-order #'+ (iq 1 2 3) (iq 4 5 6)))
  (let ((.z (let ((count 0)) (lambda (ignored) ignored (set! count (+ count 1)) count))))
  (== (iq 1 2) (imap-in-order .z (iq a b)))
  (== '#(0 1 4 9 16)
      (let ((v (make-vector 5)))
      (ifor-each (lambda (i)
                 (vector-set! v i (* i i)))
                 (iq 0 1 2 3 4))
      v))
  (== '#(5 7 9 11 13)
      (let ((v (make-vector 5)))
      (ifor-each (lambda (i j)
                 (vector-set! v i (+ i j)))
                 (iq 0 1 2 3 4)
                 (iq 5 6 7 8 9))
      v))
  (== (iq 1 -1 3 -3 8 -8)
      (iappend-map (lambda (x) (ilist x (- x))) (iq 1 3 8)))
  (== (iq 1 4 2 5 3 6)
      (iappend-map #'ilist (iq 1 2 3) (iq 4 5 6)))
  (== (vector (iq 0 1 2 3 4) (iq 1 2 3 4) (iq 2 3 4) (iq 3 4) (iq 4))
      (let ((v (make-vector 5)))
      (ipair-for-each (lambda (lis) (vector-set! v (icar lis) lis)) (iq 0 1 2 3 4))
      v))
  (== (vector (iq 5 6 7 8 9) (iq 6 7 8 9) (iq 7 8 9) (iq 8 9) (iq 9))
      (let ((v (make-vector 5)))
      (ipair-for-each (lambda (i j) (vector-set! v (icar i) j))
                      (iq 0 1 2 3 4)
                      (iq 5 6 7 8 9))
      v))
  (== (iq 1 9 49)
      (ifilter-map (lambda (x) (and (number? x) (* x x))) (iq a 1 b 3 c 7)))
  (== (iq 5 7 9)
      (ifilter-map
       (lambda (x y) (and (number? x) (number? y) (+ x y)))
       (iq 1 a 2 b 3 4)
       (iq 4 0 5 y 6 z))))))))


(test ilists/filtering
  (== (iq 0 8 8 -4) (ifilter #'even? (iq 0 7 8 8 43 -4)))
  (== (list (iq one four five) (iq 2 3 6))
      (call-with-values
       (lambda () (ipartition #'symbol? (iq one 2 3 four five 6)))
       #'list))
  (== (iq 7 43) (iremove #'even? (iq 0 7 8 8 43 -4))))


(test ilists/searching
  (== 2 (ifind #'even? (iq 1 2 3)))
  (== #t (iany  #'even? (iq 1 2 3)))
  (== #f (ifind #'even? (iq 1 7 3)))
  (== #f (iany  #'even? (iq 1 7 3)))
  (signals cl:error (cl:eval '(ifind #'even? (ipair (1 (ipair 3 x))))))
  (signals cl:error (cl:eval '(iany  #'even? (ipair (1 (ipair 3 x))))))
  (== 4 (ifind #'even? (iq 3 1 4 1 5 9)))
  (== (iq -8 -5 0 0) (ifind-tail #'even? (iq 3 1 37 -8 -5 0 0)))
  (== (iq 2 18) (itake-while #'even? (iq 2 18 3 10 22 9)))
  (== (iq 3 10 22 9) (idrop-while #'even? (iq 2 18 3 10 22 9)))
  (== (list (iq 2 18) (iq 3 10 22 9))
      (call-with-values
       (lambda () (ispan #'even? (iq 2 18 3 10 22 9)))
       #'list))
  (== (list (iq 3 1) (iq 4 1 5 9))
      (call-with-values
       (lambda () (ibreak #'even? (iq 3 1 4 1 5 9)))
       #'list))
  (== #t (iany #'integer? (iq a 3 b 2.7)))
  (== #f (iany #'integer? (iq a 3.1 b 2.7)))
  (== #t (iany #'< (iq 3 1 4 1 5) (iq 2 7 1 8 2)))
  (== #t (ievery #'integer? (iq 1 2 3 4 5)))
  (== #f (ievery #'integer? (iq 1 2 3 4.5 5)))
  (== #t (ievery #'< (iq 1 2 3) (iq 4 5 6)))
  (== 2 (ilist-index #'even? (iq 3 1 4 1 5 9)))
  (== 1 (ilist-index #'< (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)))
  (== #f (ilist-index #'= (iq 3 1 4 1 5 9 2 5 6) (iq 2 7 1 8 2)))
  (== (iq a b c) (imemq 'a (iq a b c)))
  (== (iq b c) (imemq 'b (iq a b c)))
  (== #f (imemq 'a (iq b c d)))
  (== #f (imemq (ilist 'a) (iq b (a) c)))
  (== (iq (a) c) (imember (ilist 'a) (iq b (a) c)))
  (== (iq 101 102) (imemv 101 (iq 100 101 102))))


(test ilists/deletion
  (== (iq 1 2 4 5) (idelete 3 (iq 1 2 3 4 5)))
  (== (iq 3 4 5) (idelete 5 (iq 3 4 5 6 7) #'<))
  (== (iq a b c z) (idelete-duplicates (iq a b a c a b c z))))


(test ilists/alists
  (let ((.e (iq (a 1) (b 2) (c 3))))
  (== (iq a 1) (iassq 'a .e))
  (== (iq b 2) (iassq 'b .e))
  (== #f (iassq 'd .e))
  (== #f (iassq (ilist 'a) (iq ((a)) ((b)) ((c)))))
  (== (iq (a)) (iassoc (ilist 'a) (iq ((a)) ((b)) ((c)))))
  (let ((e2 (iq (2 3) (5 7) (11 13))))
  (== (iq 5 7) (iassv 5 e2))
  (== (iq 11 13) (iassoc 5 e2 #'<))
  (== (ipair (iq 1 1) e2) (ialist-cons 1 (ilist 1) e2))
  (== (iq (2 3) (11 13)) (ialist-delete 5 e2))
  (== (iq (2 3) (5 7)) (ialist-delete 5 e2 #'<)))))


(test ilists/replacers
  (== (ipair 1 3) (replace-icar (ipair 2 3) 1))
  (== (ipair 1 3) (replace-icdr (ipair 1 2) 3)))


(test ilists/conversion
  (== (ipair 1 2) (pair->ipair '(1 . 2)))
  (== '(1 . 2) (ipair->pair (ipair 1 2)))
  (== (iq 1 2 3) (list->ilist '(1 2 3)))
  (== '(1 2 3) (ilist->list (iq 1 2 3)))
  (== (ipair 1 (ipair 2 3)) (list->ilist '(1 2 . 3)))
  (== '(1 2 . 3) (ilist->list (ipair 1 (ipair 2 3))))
  (== (ipair (ipair 1 2) (ipair 3 4)) (tree->itree '((1 . 2) . (3 . 4))))
  (== '((1 . 2) . (3 . 4)) (itree->tree (ipair (ipair 1 2) (ipair 3 4))))
  (== (ipair (ipair 1 2) (ipair 3 4)) (gtree->itree (cons (ipair 1 2) (ipair 3 4))))
  (== '((1 . 2) . (3 . 4)) (gtree->tree (cons (ipair 1 2) (ipair 3 4))))
  (== 6 (iapply #'+ (iq 1 2 3)))
  (== 15 (iapply #'+ 1 2 (iq 3 4 5))))


;;; *EOF*

