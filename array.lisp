(in-package #:aspops)

(defun array-type (type)
  (first (ensure-list type)))

(defun array-type-rank (type)
  (length (third (ensure-list type))))

(defun array-type-element-type-suffix (type)
  (let ((type (ensure-list (second (ensure-list type)))))
    (case (first type)
      (* "")
      (unsigned-byte (format nil "-~A~D" '#:ub (second type)))
      (signed-byte (format nil "-~A~D" '#:sb (second type)))
      (t (format nil "-~A" (first type))))))

(defun array-type-name (type)
  (format nil "~D~A~A-~A" (array-type-rank type) '#:d (array-type-element-type-suffix type) (array-type type)))

(defmacro define-array-nreverse-operation (array-type
                                           &optional (axis-numbers (iota (array-type-rank array-type)))
                                           &aux (dimensions (array-type-rank array-type)))
  `(progn
     . ,(with-gensyms (array)
          (loop :for axis-number :in (ensure-list axis-numbers)
                :for function-name := (intern (format nil "~A-~A-~D" '#:nreverse (array-type-name array-type) axis-number))
                :nconc `((declaim (ftype (function (,array-type) (values ,array-type)) ,function-name))
                         (defun ,function-name (,array)
                           ,(let* ((indices (make-gensym-list dimensions '#:index))
                                   (rindices (loop :for current-index :in indices
                                                   :for current-axis-number :from 0
                                                   :if (= current-axis-number axis-number)
                                                     :collect (make-gensym '#:rindex)
                                                   :else
                                                     :collect current-index))
                                   (dimensions (make-gensym-list dimensions '#:dimension)))
                              `(let ,(loop :for dimension :in dimensions
                                           :for axis-number :from 0
                                           :collect `(,dimension (array-dimension ,array ,axis-number)))
                                 ,(labels ((recur (axes body)
                                             (if axes
                                                 (destructuring-bind (dimension index rindex) (car axes)
                                                   `(loop :for ,index :below ,dimension
                                                          ,@(unless (eq index rindex) `(:for ,rindex :from (1- ,dimension) :downto 0 :while (< ,index ,rindex)))
                                                          :do ,(recur (cdr axes) body)))
                                                 body)))
                                    (recur (mapcar #'list dimensions indices rindices)
                                           `(rotatef (aref ,array . ,indices) (aref ,array . ,rindices))))))
                           ,array))))))

(defmacro define-array-replace-operation (array-type &aux (dimensions (array-type-rank array-type)))
  (let ((function-name (intern (format nil "~A-~A" '#:replace (array-type-name array-type))))
        (start1 (make-symbol (string 'start1)))
        (end1 (make-symbol (string 'end1)))
        (start2 (make-symbol (string 'start2)))
        (end2 (make-symbol (string 'end2)))
        (source-array (make-symbol (string 'source-array)))
        (target-array (make-symbol (string 'target-array)))
        (index-type (loop :with form := 'null
                          :repeat dimensions
                          :do (setf form `(cons non-negative-fixnum ,form))
                          :finally (return form))))
    `(progn
       (declaim (ftype (function (,array-type
                                  ,array-type
                                  &key
                                  (:start1 ,index-type)
                                  (:end1 ,index-type)
                                  (:start2 ,index-type)
                                  (:end2 ,index-type))
                                 (values ,array-type)) ,function-name))
       (defun ,function-name (,target-array
                              ,source-array
                              &key
                                (,start1 ',(loop :repeat dimensions :collect 0))
                                (,end1 (array-dimensions ,target-array))
                                (,start2 ',(loop :repeat dimensions :collect 0))
                                (,end2 (array-dimensions ,source-array)))
         ,(let* ((source-indices (make-gensym-list dimensions '#:source-index))
                 (target-indices (make-gensym-list dimensions '#:target-index))
                 (source-starts (make-gensym-list dimensions '#:source-start))
                 (source-ends (make-gensym-list dimensions '#:source-end))
                 (target-starts (make-gensym-list dimensions '#:target-start))
                 (target-ends (make-gensym-list dimensions '#:target-end)))
            `(destructuring-bind ,target-starts ,start1
               (destructuring-bind ,target-ends ,end1
                 (destructuring-bind ,source-starts ,start2
                   (destructuring-bind ,source-ends ,end2
                     ,(labels ((recur (axes body)
                                 (if axes
                                     (destructuring-bind (source-index target-index
                                                          source-start source-end
                                                          target-start target-end)
                                         (car axes)
                                       `(loop :for ,source-index :of-type non-negative-fixnum :from ,source-start :below ,source-end
                                              :for ,target-index :of-type non-negative-fixnum :from ,target-start :below ,target-end
                                              :do ,(recur (cdr axes) body)))
                                     body)))
                        (recur (mapcar #'list
                                       source-indices target-indices
                                       source-starts source-ends
                                       target-starts target-ends)
                               `(setf (aref ,target-array . ,target-indices) (aref ,source-array . ,source-indices)))))))))
         ,target-array))))

(defmacro define-array-map-into-operation (array-type &aux (dimensions (array-type-rank array-type)))
  (with-gensyms (array function dest-array)
    (let ((function-name (intern (format nil "~A-~A" '#:map-into (array-type-name array-type)))))
      `(progn
         (declaim (ftype (function (,array-type function ,array-type) (values ,array-type)) ,function-name))
         (defun ,function-name (,dest-array ,function ,array)
           ,(let* ((indices (make-gensym-list dimensions '#:index))
                   (dimensions (make-gensym-list dimensions '#:dimension)))
              `(let ,(loop :for dimension :in dimensions
                           :for axis-number :from 0
                           :collect `(,dimension (array-dimension ,array ,axis-number)))
                 ,(labels ((recur (axes body)
                             (if axes
                                 (destructuring-bind (dimension index) (car axes)
                                   `(loop :for ,index :below ,dimension
                                          :do ,(recur (cdr axes) body)))
                                 body)))
                    (recur (mapcar #'list dimensions indices)
                           `(setf (aref ,dest-array . ,indices) (funcall ,function (aref ,array . ,indices)))))))
           ,dest-array)))))

(defmacro define-array-subseq-operation (array-type)
  (with-gensyms (array start end)
    (let ((function-name (intern (format nil "~A-~A" '#:subseq (array-type-name array-type)))))
      `(progn
         (declaim (ftype (function (,array-type list &optional list) (values ,array-type)) ,function-name))
         (defun ,function-name (,array ,start &optional (,end (array-dimensions ,array)))
           ,(destructuring-bind (replace-function replace-lambda-list &rest replace-body)
                (cdr (find 'defun (macroexpand-1 `(define-array-replace-operation ,array-type)) :key (compose #'car #'ensure-list)))
              `(flet ((,replace-function ,replace-lambda-list . ,replace-body))
                 (declare (inline ,replace-function))
                 (,replace-function
                  (make-array
                   (mapcar #'- ,end ,start)
                   :element-type (array-element-type ,array)
                   :adjustable (adjustable-array-p ,array)
                   :fill-pointer (array-has-fill-pointer-p ,array))
                  ,array :start2 ,start :end2 ,end))))))))

(defmacro define-array-operation (name array-type)
  (if-let ((macro-name (find-symbol (format nil "~A-~A-~A" '#:define-array (lastcar (ensure-list name)) '#:operation) #.*package*)))
    `(,macro-name ,array-type)
    (error "Operation ~A is not implemented yet." name)))
