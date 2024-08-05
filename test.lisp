(defpackage aspops.test
  (:use #:cl #:parachute #:aspops)
  (:import-from #:alexandria #:copy-array)
  (:export #:suite))

(in-package #:aspops.test)

(define-test suite)

(define-test array-ops :parent suite)

(define-array-operation nreverse (simple-array (unsigned-byte 8) (* *)))

(define-test array-nreverse :parent array-ops
  (let ((array #2A((1 2 3)
                   (4 5 6)
                   (7 8 9))))
    (is equalp #2A((7 8 9)
                   (4 5 6)
                   (1 2 3))
        (nreverse-2d-ub8-simple-array-0 (copy-array array :element-type '(unsigned-byte 8))))
    (is equalp #2A((3 2 1)
                   (6 5 4)
                   (9 8 7))
        (nreverse-2d-ub8-simple-array-1 (copy-array array :element-type '(unsigned-byte 8))))))

(define-array-operation replace (simple-array (unsigned-byte 8) (* *)))

(define-test array-replace :parent array-ops
  (let ((array-1 #2A((1 2 3)
                     (4 5 6)
                     (7 8 9)))
        (array-2 #2A((1 2)
                     (3 4))))
    (is equalp #2A((1 2 3)
                   (3 4 6)
                   (7 8 9))
        (replace-2d-ub8-simple-array
         (copy-array array-1 :element-type '(unsigned-byte 8))
         (copy-array array-2 :element-type '(unsigned-byte 8))))
    (is equalp #2A((1 2 3)
                   (4 1 2)
                   (7 3 4))
        (replace-2d-ub8-simple-array
         (copy-array array-1 :element-type '(unsigned-byte 8))
         (copy-array array-2 :element-type '(unsigned-byte 8))
         :start1 '(1 1)
         :end1 '(3 3)))
    (is equalp #2A((1 2 3)
                   (2 3 6)
                   (5 6 9))
        (replace-2d-ub8-simple-array
         (copy-array array-1 :element-type '(unsigned-byte 8))
         (copy-array array-1 :element-type '(unsigned-byte 8))
         :start1 '(1 0)
         :start2 '(0 1)))))

(define-array-operation map-into (simple-array (unsigned-byte 8) (* *)))

(define-test array-map-into :parent array-ops
  (let ((array (copy-array
                #2A((1 2 3)
                    (4 5 6)
                    (7 8 9))
                :element-type '(unsigned-byte 8))))
    (is eq array (map-into-2d-ub8-simple-array array #'1+ array))
    (is equalp #2A((2 3 4)
                   (5 6 7)
                   (8 9 10))
        array)))

(define-array-operation subseq (simple-array (unsigned-byte 8) (* *)))

(define-test array-subseq :parent array-ops
  (let ((array (copy-array
                #2A((1 2 3)
                    (4 5 6)
                    (7 8 9))
                :element-type '(unsigned-byte 8))))
    (is equalp #2A((1 2)
                   (4 5))
        (subseq-2d-ub8-simple-array array '(0 0) '(2 2)))
    (is equalp #2A((4 5)
                   (7 8))
        (subseq-2d-ub8-simple-array array '(1 0) '(3 2)))
    (is equalp #2A((5 6)
                   (8 9))
        (subseq-2d-ub8-simple-array array '(1 1)))))

(define-test vector-ops :parent suite)

(define-array-reinterpretation (simple-array (unsigned-byte 8) (*)) (simple-array (unsigned-byte 4) (*)))

(define-test ub/le-vector-reinterpretation :parent vector-ops
  (is equalp #(#x2 #x1 #x4 #x3) (ub8/le-vector-ub4-vector (coerce #(#x12 #x34) '(simple-array (unsigned-byte 8) (*)))))
  (is equalp #(#x12 #x34) (ub4-vector-ub8/le-vector (coerce #(#x2 #x1 #x4 #x3) '(simple-array (unsigned-byte 4) (*))))))
