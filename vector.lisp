(in-package #:aspops)

(defmacro define-unsigned-byte-vector-reinterpretation (nlbits nsbits)
  (let ((sl-function-name (intern (format nil "~A~D~A~D~A" '#:ub nsbits '#:-vector-ub nlbits '#:/le-vector)))
        (ls-function-name (intern (format nil "~A~D~A~D~A" '#:ub nlbits '#:/le-vector-ub nsbits '#:-vector))))
    (multiple-value-bind (count remainder) (floor nlbits nsbits)
      (assert (zerop remainder))
      (with-gensyms (vector result value position base-index index length remainder)
        `(progn
           (declaim (ftype (function ((simple-array (unsigned-byte ,nsbits) (*))) (values (simple-array (unsigned-byte ,nlbits) (*)))) ,sl-function-name))
           (defun ,sl-function-name (,vector)
             (multiple-value-bind (,length ,remainder) (floor (length ,vector) ,count)
               (assert (zerop ,remainder))
               (loop :with ,result :of-type (simple-array (unsigned-byte ,nlbits) (*)) := (make-array ,length :element-type '(unsigned-byte ,nlbits))
                     :for ,value :of-type (unsigned-byte ,nsbits) :across ,vector
                     :for ,position :of-type (mod ,nlbits) := 0 :then (mod (+ ,position ,nsbits) ,nlbits)
                     :for ,index :of-type non-negative-fixnum := 0 :then (if (zerop ,position) (1+ ,index) ,index)
                     :do (setf (ldb (byte ,nsbits ,position) (aref ,result ,index)) ,value)
                     :finally (return ,result))))
           (declaim (ftype (function ((simple-array (unsigned-byte ,nlbits) (*))) (values (simple-array (unsigned-byte ,nsbits) (*)))) ,ls-function-name))
           (defun ,ls-function-name (,vector)
             (loop :with ,result :of-type (simple-array (unsigned-byte ,nsbits) (*)) := (make-array (* (length ,vector) ,count) :element-type '(unsigned-byte ,nsbits))
                   :for ,value :of-type (unsigned-byte ,nlbits) :across ,vector
                   :for ,base-index :of-type non-negative-fixnum :from 0 :by ,count
                   :do (loop :for ,position :of-type (unsigned-byte ,nlbits) :below ,nlbits :by ,nsbits
                             :for ,index :of-type non-negative-fixnum :from ,base-index
                             :do (setf (aref ,result ,index) (ldb (byte ,nsbits ,position) ,value)))
                   :finally (return ,result))))))))

(defmacro define-array-reinterpretation (array-type-1 array-type-2)
  (destructuring-bind ((array-type-1 type1 dimensions1) . (array-type-2 type2 dimensions2)) (cons array-type-1 array-type-2)
    (assert (eq array-type-1 'simple-array))
    (assert (eq array-type-2 'simple-array))
    (assert (equal dimensions1 '(*)))
    (assert (equal dimensions2 '(*)))
    (eswitch ((cons (first type1) (first type2)) :test #'equal)
      ('(unsigned-byte . unsigned-byte)
        `(define-unsigned-byte-vector-reinterpretation
             ,(max (second type1) (second type2))
             ,(min (second type1) (second type2)))))))
