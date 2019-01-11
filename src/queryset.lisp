;;;; -*- coding: utf-8; -*-
;;;;
;;;; Queryset manipulation
;;;;
(in-package :cl-user)

(defpackage secsrv.queryset
  (:nicknames :qs)
  (:use :cl :alexandria)
  (:import-from :cl-log
                #:log-message)
  (:export #:make-queryset
           #:select))

(in-package :secsrv.queryset)

(define-constant +join+ "JOIN" :test #'string-equal :documentation "Code of normal join in SQL query.")
(define-constant +left-join+ "LEFT JOIN" :test #'string-equal :documentation "Code of left join in SQL query.")
(define-constant +queryset-tablename-prefix+ "T" :test #'string-equal
                 :documentation "Prefix of all table aliases generated by sql<- function.")


(defclass <select-field> ()
  ((queryset :type <queryset>
             :documentation "The queryset this field belongs to.")
   (table-name :type (or string null)
               :documentation "Name of the table this field cames from.")
   (name :type string
         :documentation "Name of the field in TABBLE-ALIAS queryset.")
   (expression :type string
               :initarg :expression
               :accessor field-expression
               :documentation "Selected expresson, e.g. a table.field, constant, or function call.")
   (alias :type string
          :initarg :alias
          :accessor field-alias
          :documentation "How this field is named in the queryset."))
  (:documentation "Description of selected attribute."))

(defun make-select-field (expression alias)
  (make-instance '<select-field>
                 :expression expression :alias alias))

(defclass <join-operand> ()
  ((expression :type (or string null) :initarg :expression :initform nil :accessor join-operand-expression))
  (:documentation "Definition of join operand."))

(defclass <join-field> (<join-operand>)
  ((table-alias :type string :initarg :table-alias :accessor join-field-table-alias)
   (field-name :type string :initarg :field-name :accessor join-field-field-name))
  (:documentation "Description of field used in a join."))

(defmethod initialize-instance :after ((f <join-field>) &key)
  (with-accessors ((expr join-operand-expression))
      f
    (unless expr
      (setf expr (format nil "~A.~A" (join-field-table-alias f) (join-field-field-name f))))))


(defclass <join-relation> ()
  ((left :type <join-operand> :initarg :left :accessor left-operand :initform (error "No left join operand supplied."))
   (operator :type string  :initarg :operator :accessor operator :initform (error "No join relation operator supplied."))
   (right :type <join-operand> :initarg :right :accessor right-operand  :initform (error "No right join operand supplied.")))
  (:documentation "Description of one join relation."))

(defun make-relation (left operator right)
  (make-instance '<join-relation>
    :left (make-instance '<join-operand> :expression left)
    :operator operator
    :right (make-instance '<join-operand> :expression right)))

(defun relation-expression (relation)
  (format nil "~A ~A ~A"
          (join-operand-expression (left-operand relation))
          (operator relation)
          (join-operand-expression (right-operand relation))))



(defun %check-join-type-value (join-type)
  (member join-type (list +join+ +left-join+) :test #'equalp))

(deftype join-mode () '(and string (satisfies %check-join-type-value)))

(defclass <join> ()
  ((mode :type <join-mode>
         :initarg :mode
         :accessor join-mode
         :documentation "Join mode, e.g. JOIN, or LEFT JOIN.")
   (queryset :type (or string <queryset>)
             :initarg :queryset
             :accessor join-queryset)
   (table-alias :type string :initarg :alias :accessor join-alias)
   (relations :initform nil
              :initarg :relations
              :accessor join-relations
              :documentation "Join relations. NIL for natural join on primary/foreign key attributes."))
  (:documentation "A join consists of join-type, queryset and join relation."))

(defun make-join (mode qs alias relation)
  "Create new SQL join expression using single join condition RELATION."
  (let ((join (make-instance '<join>
                             :mode mode
                             :queryset qs
                             :alias alias
                             :relations (list relation))))
    join))


(defclass <queryset> ()
  ((fields
    :accessor queryset-fields
    :initform nil
    :documentation "List of selected expressioins. If NIL, then all
    fileds of the underlying query are assumed selected.")
   (aggregates :initform nil
               :documentation "List of selected aggregated fields.")
   (main-table
    :type (or string <queryset>)
    :initarg :main-table
    :initform (error "Queryset main table is not specified")
    :accessor queryset-main-table
    :documentation "SQL query to be evaluated as master table. It can
    be a model, a subquery defined by a queryset, or a raw SQL query
    specified by string.")
   (joins :initform nil
          :accessor queryset-joins
          :documentation "List of query's joins.")
   (where :initform nil
          :accessor queryset-where
          :documentation "List of statements.")
   (groupby :initform nil
            :documentation "List of selcted fields' aliases.")
   (having :initform nil
           :documentation "Constraints on groups of records.")
   (bindings :initform nil
             :documentation "List of variable bindings.")
   (aliases-seq :type integer
                :initform 0
                :documentation "Internal counter used for generation of unique table aliases."))
  (:documentation "Queryset is a destructured representation of SQL
  query suitable for nesting and similar operations."))

(defgeneric make-queryset (query &key bindings)
  (:documentation "Accepts query - a raw SQL string or model instance
  - and returns queryset instance."))

(defgeneric filter-queryset (queryset filter)
  )

(defun queryset-main-table-alias (qs)
  (declare (ignore qs))
  (format nil "~A0" +queryset-tablename-prefix+))

(defun queryset-propose-alias (qs)
  (let ((seq (incf (slot-value qs 'aliases-seq))))
    (format nil "~A~D" +queryset-tablename-prefix+ seq)))

(defun sql<-queryset (qs)
  "Generate SQL query specified by queryset QS."
  (labels ((from-clause ()
             (etypecase (queryset-main-table qs)
               (string (format nil "~A AS ~A"
                              (queryset-main-table qs)
                              (queryset-main-table-alias qs)))
               (<queryset> (format nil "(~%~A~%) ~A"
                                 (sql<-queryset (queryset-main-table qs))
                                 (queryset-main-table-alias qs)))))
           (join-table (join)
             (etypecase (join-queryset join)
               (<queryset> (format nil "(~A)" (sql<-queryset (join-queryset join))))
               (string (join-queryset join)))))
    (with-output-to-string (s)
      (if (queryset-fields qs)
          (format s "SELECT ~:{~a AS ~a~%~:^       , ~}"
                   (mapcar #'(lambda (f) (list (field-expression f) (field-alias f)))
                           (queryset-fields qs)))
          (format s "SELECT *~%"))
      (format s "~&FROM ~A" (from-clause))
      ;; table joins
      (format s "~:{~&~A ~A ~A ON (~A)~}"
              (mapcar #'(lambda (join)
                          (list (join-mode join)
                                (join-table join)
                                (join-alias join)
                                (relation-expression (first (join-relations join)))))
                      (queryset-joins qs)))
      ;; WHERE constraints
      (format s "~{~#[~:;~&WHERE ~@{~A~#[~:;~&AND ~]~}~]~:}"
              (mapcar #'relation-expression (queryset-where qs)))
      s)))

(defmethod print-object ((qs <queryset>) stream)
  #+(or)(format stream "#<~A: ~{~A ~}>"
          (queryset-main-table qs)
          (mapcar #'field-alias (queryset-fields qs)))
  (format stream (sql<-queryset qs)))

(defun make-default-queryset (model)
  "Create a `queryset' selecting all attributes of the `MODEL'."
  (let ((qs (make-instance '<queryset> :main-table model)))
    qs))


(defun find-qs-attribute (name qs &key (search-available nil))
  "Find attribute by NAME in a queryset QS. If QS's fields is not NIL and SEARCH-AVAILABLE is NIL,
  then search by SQL field alias in the list of fields selected by the
  queryset QS. Otherwise, propagate search request to QS's main
  subquery."
  ;;--- TODO: search in joined tables as well.
  (with-accessors ((main-table queryset-main-table)
                   (fields queryset-fields))
      qs
    (if (and fields (not search-available))
        (find-if #'string-equal fields :key #'field-alias)
        (etypecase main-table
          ;; (model (find-model-attribute name main-table))
          (<queryset> (find-qs-attribute name main-table))))))

(defun expression-dependencies (expression)
  "Find all referenes to fully qualified fields in the
EXPRESSION. Returns list of conses where CAR holds table name and CDR
holds field name."
  (let ((period (position #\. expression :test #'char=)))
    (list
     (if period
         (cons (subseq expression 0 period) (subseq expression (1+ period)))
         (cons nil expression)))))


(defun suggest-expression-alias (expression)
  "Generate alias for given selected SQL EXPRESSION."
  (let ((deps (expression-dependencies expression)))
    (if deps (cdar deps) expression)))

(defun find-join (qs alias)
  "Find a `join' instance used in the `queryset' QS with given ALIAS."
  (find alias (queryset-joins qs) :key #'join-alias :test #'string-equal))

(defun add-simple-join (qs related-name)
  "Add new join to the queryset QS using models relation names."
  (check-type qs <queryset> "a valid qs argument")
  (check-type related-name string "a valid related-name argument")
  (check-type (queryset-main-table qs) string "a valid main-table for simplified join")
  (log-message :trace "Adding join ~A~%" related-name)
  (when-let* ((qs-attribute (find-model-attribute related-name (queryset-main-table qs)))
              (model-to-join (find-related-model related-name (queryset-main-table qs)))
              (alias (queryset-propose-alias qs))
              (qs-join-field (make-instance '<join-field>
                                            :table-alias (queryset-main-table-alias qs)
                                            :field-name (attribute-column-name
                                                         (if (reverse-attribute qs-attribute)
                                                             (model-primary-key (queryset-main-table qs))
                                                             qs-attribute))))
              (related-join-field (make-instance '<join-field>
                                                 :table-alias alias
                                                 :field-name (attribute-column-name
                                                              (if (reverse-attribute qs-attribute)
                                                                  (reverse-attribute qs-attribute)
                                                                  (model-primary-key model-to-join)))))
              (relation (make-instance '<join-relation>
                                       :left qs-join-field
                                       :operator "="
                                       :right related-join-field))
              (join (make-join +join+
                               (make-default-queryset model-to-join)
                               alias ;(model-name model-to-join)
                               relation)))
    (setf (queryset-joins qs)
          (append (queryset-joins qs) (list join)))))

(defun add-select-expression (qs expression &optional (alias nil))
  "Make a select-field and appends it to the list of fields selected by QS."
  (check-type qs <queryset> "a valid qs argument")
  ;;--- TODO: Check for aliases collision
  (let ((field (make-select-field expression alias))
        (deps (expression-dependencies expression)))
    ;; Check that all attributes referenced in the expression are
    ;; available in the QS subqueries or models; add necessary joins
    ;; if the expression reffers to dependent table that were not
    ;; joined the query.
    (loop :for (table-name . field-name) :in deps
       :when table-name
       :do
       (let ((join (find-join qs table-name)))
         (if join
             (unless (find-qs-attribute field-name (join-queryset join))
               (error "Attribute ~A.~A not found." table-name field-name))
             (add-simple-join qs table-name))))
    (setf (queryset-fields qs)
          (append (queryset-fields qs) (list field)))))



(defmacro select (model &rest clauses)
  (let ((main-qs (gensym "main-qs-"))
        (model-value (gensym))
        (queryset (gensym "queryset-")))
    `(let* ((,main-qs ,(typecase model
                                 (string `(find-model ,model))
                                 (t `(let ((,model-value (handler-case ,model
                                                          (unbound-variable (e) (declare (ignore e)) nil))))
                                       (if ,model-value
                                           (etypecase ,model-value
                                             (model ,model-value)
                                             (queryset ,model-value)
                                             (string (find-model ,model-value)))
                                           (find-model ,(string model)))))))
            (,queryset (make-instance 'queryset :main-table ,main-qs)))
       (assert ,main-qs)
       ,@(loop
            :with code = '()
            :for statement :in clauses
            :do
            (alexandria:eswitch ((string-upcase (car statement)) :test #'string=)
              ("FIELDS"
               (dolist (field (cdr statement))
                 (push `(add-select-expression ,queryset ,(to-string field) ,(suggest-expression-alias (to-string field)))
                       code)))
              ("WHERE" (print "WHERE")))
            :finally (return (reverse code)))
       ,queryset)))

;;  (destructuring-bind (command &optional name . declarations) statement


#+or(defun selftest ()
  (clrhash *all-models*)
  (parse-model-definition '(defmodel article
                               (table article) (attributes ((id f_article_id)))))
  (parse-model-definition "(defmodel author (table authora) (attributes ((id f_authora_id) (article f_article_id references article))) )")
  (validate-all-models)
  (let ((qs (select "article"
                    (fields :id :authors.article))))
    (select qs
            (fields :id))))