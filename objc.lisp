;; Copyright (C) 2025 The Calendrical System
;; SPDX-License-Identifier: 0BSD

;; Objective-C to Lisp Binding with Transparent Layer

;; Dependencies:
;; (ql:quickload '(:alexandria :anaphora :closer-mop :cffi :cffi-libffi))

;; Packages:
;; OBJC:         Main Package
;; OBJC-CLASS:   For Class Definitions
;; OBJC-META:    For Metaclass Definitions
;; OBJC-METHOD:  For Obj-C -> Lisp Generic Function Methods
;; OBJC-PROP:    Obj-C Properties -> Lisp Slot Names

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "OBJC")
    (make-package "OBJC" :use (list (find-package "CL")
                                    (find-package "CFFI")
                                    (find-package "ANAPHORA"))))
  (dolist (pak '("OBJC-CLASS" "OBJC-META" "OBJC-METHOD" "OBJC-PROP"))
    (unless (find-package pak)
      (make-package pak :use (list (find-package "OBJC")))))

  (export (intern "ALLOC"     "OBJC-METHOD") "OBJC-METHOD")
  (export (intern "INIT"      "OBJC-METHOD") "OBJC-METHOD")
  (export (intern "PROTOCOL"  "OBJC-CLASS")  "OBJC-CLASS")
  (export (intern "NS-OBJECT" "OBJC-CLASS")  "OBJC-CLASS")
  (export (intern "NS-NUMBER" "OBJC-META")   "OBJC-META")
  (export (intern "NS-STRING" "OBJC-META")   "OBJC-META"))

;; Foreign Library

(in-package "OBJC")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library cocoa
    (:darwin (:framework "Cocoa")))
  (define-foreign-library foundation
    (:darwin (:framework "Foundation")))
  (define-foreign-library appkit
    (:darwin (:framework "AppKit"))))

(use-foreign-library foundation)
(use-foreign-library cocoa)
(use-foreign-library appkit)

;; Raw Bindings (objc-raw.lisp) needs here

(in-package "OBJC")

;; Name Translator

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-words*
    '("NS" "UTF8" "UI" "URL" "HTML" "PDF" "PNG" "RTFD" "RTF" "TIFF"))
  (defvar *special-translations*
    '(("NS-OBJECT" . "NSObject")))
  
  ;; (defmethod cffi:translate-name-from-foreign :around (name package &optional varp)
  ;;   (call-next-method name (find-package package) varp))
  ;; (defmethod cffi:translate-name-to-foreign :around (name package &optional varp)
  ;;   (call-next-method name (find-package package) varp))

  (macrolet ((def-translate-from (pkg-name)
               `(defmethod cffi:translate-name-from-foreign
                    (name (package (eql (find-package ,pkg-name))) &optional varp)
                  (aif (car (rassoc name *special-translations* :test #'equal))
                    (multiple-value-bind (sym status)
                        (intern (if varp (concatenate 'simple-string "*" it "*") it)
                                package)
                      (unless (member status '(:inherited :external))
                        (export sym package))
                      sym)
                    ;; From CFFI source code
                    (let ((name (reduce #'(lambda (s1 s2)
                                            (concatenate 'simple-string s1 "-" s2))
                                        (mapcar #'string-upcase
                                                (cffi::collapse-prefix
                                                 (cffi::split-if #'(lambda (ch)
                                                                     (or (upper-case-p ch)
                                                                         (digit-char-p ch)))
                                                                 (nsubstitute #\. #\: name))
                                                 *special-words*)))))
                      (when varp
                        (setq name (concatenate 'simple-string "*" name "*")))
                      (multiple-value-bind (sym status)
                          (intern name package)
                        (unless (member status '(:inherited :external))
                          (export sym package))
                        sym)))))
             (def-translate-to (pkg-name &optional upper-initial-p)
               `(defmethod cffi:translate-name-to-foreign
                    (name (package (eql (find-package ,pkg-name))) &optional varp)
                  (or (cdr (assoc (symbol-name name) *special-translations* :test #'string=))
                      (let ((name (translate-camelcase-name name
                                                            :special-words *special-words*
                                                            :upper-initial-p ,upper-initial-p)))
                        (setq name (nsubstitute #\: #\. name))
                        (when varp
                          (setq name (subseq name 1 (1- (length name)))))
                        (if (uiop:string-prefix-p "ns" name)
                            (concatenate 'string "NS" (subseq name 2))
                            name))))))
    (def-translate-from "OBJC")
    (def-translate-from "OBJC-CLASS")
    (def-translate-from "OBJC-META")
    (def-translate-from "OBJC-METHOD")
    (def-translate-from "OBJC-PROP")
    (def-translate-to   "OBJC")
    (def-translate-to   "OBJC-CLASS" t)
    (def-translate-to   "OBJC-META" t)
    (def-translate-to   "OBJC-METHOD")
    (def-translate-to   "OBJC-PROP")))


;; Objc type encoding

;;; Giving a objc type encoding,
;;; know the corresponding lisp type for method specifiers,
;;; know the corresponding C type for auto-translating method call and return value

;; https://gcc.gnu.org/onlinedocs/gcc-5.3.0/gcc/Type-encoding.html

(defvar *objc-types-map*
  '((:objc #\c :c :char               :lisp character)
    (:objc #\i :c :int                :lisp fixnum)
    (:objc #\s :c :short              :lisp fixnum)
    (:objc #\l :c :long               :lisp fixnum)
    (:objc #\q :c :long-long          :lisp integer)
    (:objc #\C :c :uchar              :lisp character)
    (:objc #\I :c :uint               :lisp fixnum)
    (:objc #\S :c :ushort             :lisp fixnum)
    (:objc #\L :c :ulong              :lisp fixnum)
    (:objc #\Q :c :unsigned-long-long :lisp integer)
    (:objc #\f :c :float              :lisp single-float)
    (:objc #\d :c :double             :lisp double-float)
    (:objc #\B :c :boolean            :lisp t)
    (:objc #\* :c :string             :lisp string)
    (:objc #\@ :c objc-id             :lisp t)
    (:objc #\# :c objc-class          :lisp objc-class)
    (:objc #\: :c selector            :lisp selector)
    ))

(defun parse-objc-type-encoding-to-lisp (char-or-str)
  (let ((char (if (stringp char-or-str)
                  (char (string-left-trim '(#\r #\n #\N #\o #\O #\R #\V) char-or-str) 0)
                  char-or-str)))
    (find-class
     (aif (find-if (lambda (lst) (eql char (getf lst :objc)))
                   *objc-types-map*)
       (getf it :lisp)
       t))))

(defun parse-objc-type-encoding-to-c (char-or-str)
  (let ((char (if (stringp char-or-str)
                  (char (string-left-trim '(#\r #\n #\N #\o #\O #\R #\V) char-or-str) 0)
                  char-or-str)))
    (aif (find-if (lambda (lst) (eql char (getf lst :objc)))
                  *objc-types-map*)
      (getf it :c)
      :pointer)))

(defun parse-c-type-to-objc-encoding (c-type)
  (aif (find-if (lambda (lst) (eq c-type (getf lst :c)))
                        *objc-types-map*)
       (getf it :objc)
       (if (subtypep c-type 'objc-object)
           #\@
           #\?)))

(defun parse-lisp-type-to-c (lisp-type)
  (aif (find-if (lambda (lst) (and (not (eq (getf lst :lisp) t))
                                   (subtypep lisp-type (find-class (getf lst :lisp)))))
                        *objc-types-map*)
    (getf it :c)
    (if (subtypep lisp-type (find-class 'objc-object))
        'objc-id
        :pointer)))

(defun parse-lisp-type-to-objc-encoding (lisp-type)
  (aif (find-if (lambda (lst) (subtypep lisp-type (find-class (getf lst :lisp))))
                        *objc-types-map*)
    (getf it :objc)
    #\?))

(export '(parse-lisp-type-to-c
          parse-lisp-type-to-objc-encoding
          parse-objc-type-encoding-to-c
          parse-objc-type-encoding-to-lisp
          parse-c-type-to-objc-encoding))


;; Obj-C Object

(defclass objc-object ()
  ((obj :initarg :objc-object
        :accessor objc-obj)))

(defmethod initialize-instance ((instance objc-object) &rest initargs)
  (apply #'shared-initialize instance t initargs)
  (when (and (not (slot-boundp instance 'obj))
             (typep (class-of instance) 'objc-object))
    (setf (objc-obj instance)
          (objc-raw::class-create-instance (objc-obj (class-of instance)) 0))))

(export '(objc-obj objc-object obj))

;; Obj-C Selector

(defclass selector (objc-object function)
  ((name  :type string
          :initarg :name
          :accessor selector-name))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod print-object ((obj selector) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (selector-name obj) stream)))

(defmethod initialize-instance ((selector selector) &key name objc-object)
  (cond (objc-object
         (setf (objc-obj selector) objc-object
               (selector-name selector) (objc-raw::sel-get-name objc-object)))
        (name
         (setf (selector-name selector) name
               (objc-obj selector) (objc-raw::sel-register-name name)))
        (t (error "One of initarg must be provided: :NAME or :OBJC-OBJECT")))
  (c2mop:set-funcallable-instance-function
   selector
   (lambda (object &rest args)
     (apply (ensure-objc-method (object-get-class object) selector)
            object
            args))))

(define-foreign-type objc-selector-type () ()
  (:actual-type :pointer)
  (:simple-parser selector))

(defmethod translate-from-foreign (ptr (type objc-selector-type))
  (make-instance 'selector :objc-object ptr))
(defmethod translate-to-foreign (obj (type objc-selector-type))
  (objc-obj obj))
(defmethod expand-from-foreign (ptr (type objc-selector-type))
  `(make-instance 'selector :objc-object ,ptr))
(defmethod expand-to-foreign (obj (type objc-selector-type))
  `(objc-obj ,obj))

(export '(selector selector-name name objc-selector-type))


;; Obj-C Property <=> Slot Definition

(defclass objc-property-slot (c2mop:standard-direct-slot-definition)
  ((property-name :initarg :property-name)
   (getter-name   :initarg :getter-name)
   (setter-name   :initarg :setter-name)
   (read-only-p   :initarg :read-only-p)
   (return-type   :initarg :return-type)
   (ivar-name     :initarg :ivar-name)
   (copy-p        :initarg :copy-p)
   (retain-p      :initarg :retain-p)
   (non-atomic-p  :initarg :non-atomic-p)
   (dynamic-p     :initarg :dynamic-p)
   (weak-p        :initarg :weak-p)
   (gc-p          :initarg :gc-p)

   (from-objc     :initarg :from-objc)
   (lisp-only     :initform nil
                  :initarg :lisp-only))
  (:documentation
   "properties ::= {(name [[attributes]])}*
attributes ::= {:return-type  ret-type} | ; T
               {:getter-name  name}     | ; G
               {:setter-name  name}     | ; S
               {:ivar-name    name}     | ; V
               {:read-only-p  boolean}  | ; R
               {:copy-p       boolean}  | ; C
               {:retain-p     boolean}  | ; &
               {:non-atomic-p boolean}  | ; N
               {:dynamic-p    boolean}  | ; D
               {:weak-p       boolean}  | ; W
               {:gc-p         boolean}  | ; P
               {:from-objc    boolean}
"))

(defclass objc-property (c2mop:standard-effective-slot-definition)
  ((property-name :initarg :property-name)
   (getter-name   :initarg :getter-name)
   (setter-name   :initarg :setter-name)
   (read-only-p   :initarg :read-only-p)
   (return-type   :initarg :return-type)
   (ivar-name     :initarg :ivar-name)
   (copy-p        :initarg :copy-p)
   (retain-p      :initarg :retain-p)
   (non-atomic-p  :initarg :non-atomic-p)
   (dynamic-p     :initarg :dynamic-p)
   (weak-p        :initarg :weak-p)
   (gc-p          :initarg :gc-p)))

(defmethod print-object ((obj objc-property) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (property-name return-type read-only-p) obj
      (when read-only-p
        (princ "(readonly) " stream))
      (format stream "~A ~A"
              (class-name (parse-objc-type-encoding-to-lisp return-type))
              property-name))))

;;; Foreign structure to be used

(defcstruct objc-property-attribute
  (:name :string)
  (:value :string))


;; Obj-C Class

;;; Foreign Type

(define-foreign-type objc-class-type () ()
  (:actual-type :pointer)
  (:simple-parser objc-class))

(defmethod translate-from-foreign (ptr (type objc-class-type))
  (if (null-pointer-p ptr)
      nil
      (ensure-objc-class ptr)))
(defmethod translate-to-foreign (obj (type objc-class-type))
  (objc-obj obj))
(defmethod expand-from-foreign (ptr (type objc-class-type))
  `(if (null-pointer-p ,ptr)
       nil
       (ensure-objc-class ,ptr)))
(defmethod expand-to-foreign (obj (type objc-class-type))
  `(objc-obj ,obj))


;;; Lisp Class Metaobjects & Methods

(defclass objc-class (standard-class objc-object)
  ((objc-class-name :initarg :objc-class-name
                    :accessor objc-class-name)
   (auto-bind-properties :initform t
                         :initarg :auto-bind-properties)
   (auto-register-methods :initform t
                          :initarg :auto-register-methods)))

(defclass objc-metaclass (objc-class) ())

(defmethod c2mop:validate-superclass ((c1 objc-class) (c2 standard-class)) t)

;;;; Slot

(defmethod c2mop:direct-slot-definition-class ((class objc-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'objc-property-slot))

(defmethod c2mop:effective-slot-definition-class ((class objc-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'objc-property))

;;;; --- Helper ---
(defun objc-property-default-setter-name (property-name)
  (let ((name (copy-seq property-name)))
    (setf (char name 0)
          (char-upcase (char name 0)))
    (concatenate 'string "set" name)))
;;;; --- END helper ---
(defmethod c2mop:compute-effective-slot-definition :around ((class objc-class) name slots)
  (let ((slot      (car slots))
        (class-ptr (slot-value class 'obj)))
    (unless (typep slot 'objc-property-slot)
      (return-from c2mop:compute-effective-slot-definition (call-next-method)))
    (with-slots (property-name getter-name setter-name read-only-p return-type
                 ivar-name copy-p retain-p non-atomic-p dynamic-p weak-p gc-p
                 from-objc lisp-only)
        slot
      (when lisp-only
        (return-from c2mop:compute-effective-slot-definition (call-next-method)))
      (if from-objc
          (let* ((prop-name (or property-name
                                (translate-to-foreign property-name "OBJC-PROP")))
                 (prop (objc-raw::class-get-property class-ptr prop-name)))
            (assert (not (null-pointer-p prop)))
            (make-instance
             'objc-property
             :name            name
             :initform        (c2mop:slot-definition-initform slot)
             :initfunction    (c2mop:slot-definition-initfunction slot)
             :type            (c2mop:slot-definition-type slot)
             :allocation      (c2mop:slot-definition-allocation slot)
             :initargs        (c2mop:slot-definition-initargs slot)
             :documentation   (documentation slot t)
             :property-name   prop-name
             :getter-name     (or (property-copy-attribute-value prop "G")
                                  prop-name)
             :setter-name     (or (property-copy-attribute-value prop "S")
                                  (objc-property-default-setter-name prop-name))
             :read-only-p     (property-copy-attribute-value prop "R")
             :return-type     (parse-objc-type-encoding-to-c
                               (property-copy-attribute-value prop "T"))
             :ivar-name       (property-copy-attribute-value prop "V")
             :copy-p          (property-copy-attribute-value prop "C")
             :retain-p        (property-copy-attribute-value prop "&")
             :non-atomic-p    (property-copy-attribute-value prop "N")
             :dynamic-p       (property-copy-attribute-value prop "D")
             :weak-p          (property-copy-attribute-value prop "W")
             :gc-p            (property-copy-attribute-value prop "P")))
          
          (let (prop-plist)
            (unless property-name
              (setf property-name (translate-name-to-foreign name (find-package "OBJC-PROP"))))
            (push (list :name "T" :value return-type) prop-plist)
            (when getter-name  (push (list :name "G" :value getter-name) prop-plist))
            (when setter-name  (push (list :name "S" :value getter-name) prop-plist))
            (when ivar-name    (push (list :name "V" :value ivar-name)   prop-plist))
            (when read-only-p  (push (list :name "R" :value nil) prop-plist))
            (when copy-p       (push (list :name "C" :value nil) prop-plist))
            (when retain-p     (push (list :name "&" :value nil) prop-plist))
            (when non-atomic-p (push (list :name "N" :value nil) prop-plist))
            (when dynamic-p    (push (list :name "D" :value nil) prop-plist))
            (when weak-p       (push (list :name "W" :value nil) prop-plist))
            (when gc-p         (push (list :name "P" :value nil) prop-plist))
            (let ((len (length prop-plist)))
              (with-foreign-object (attr '(:struct objc-property-attribute) len)
                (dotimes (i len)
                  (setf (mem-aref attr '(:struct objc-property-attribute) i)
                        (nth i prop-plist)))
                (objc-raw::class-add-property class-ptr property-name attr len)))
            (make-instance
             'objc-property
             :name            name
             :initform        (c2mop:slot-definition-initform slot)
             :initfunction    (c2mop:slot-definition-initfunction slot)
             :type            (c2mop:slot-definition-type slot)
             :allocation      (c2mop:slot-definition-allocation slot)
             :initargs        (c2mop:slot-definition-initargs slot)
             :documentation   (documentation slot t)
             :property-name   property-name
             :getter-name     (or getter-name property-name)
             :setter-name     (or setter-name (objc-property-default-setter-name property-name))
             :read-only-p     read-only-p
             :return-type     return-type
             :ivar-name       ivar-name
             :copy-p          copy-p
             :retain-p        retain-p
             :non-atomic-p    non-atomic-p
             :dynamic-p       dynamic-p
             :weak-p          weak-p
             :gc-p            gc-p)))
      )))

(defmethod c2mop:slot-boundp-using-class
    :around ((class objc-class) (object objc-object) (slot objc-property))
  (declare (ignore class))
  (if (slot-boundp slot 'getter-name) t
      (call-next-method)))

(defmethod c2mop:slot-value-using-class
    :around ((class objc-class) (object objc-object) (slot objc-property))
  (declare (ignore class))
  (if (slot-boundp slot 'getter-name)
      (with-slots (property-name getter-name return-type) slot
        (let ((sel (objc-raw::sel-get-uid getter-name)))
          (if (objc-raw::class-responds-to-selector (objc-obj class) sel)
              (eval (list 'foreign-funcall
                          "objc_msgSend"
                          :pointer (slot-value object 'obj)
                          :pointer sel
                          return-type))
              (error "Cannot resolve Obj-C property getter for ~A: ~A does not respond to ~A"
                     property-name
                     object
                     getter-name))))
      (call-next-method)))

(defmethod (setf c2mop:slot-value-using-class)
    :around (new-value (class objc-class) (object objc-object) (slot objc-property))
  (if (slot-boundp slot 'setter-name)
      (with-slots (property-name setter-name read-only-p return-type) slot
        (if read-only-p
            (error "Obj-C property ~A for ~A is read-only"
                   property-name
                   object)
            (let ((sel (objc-raw::sel-get-uid setter-name)))
              (if (objc-raw::class-responds-to-selector (objc-obj class) sel)
                  (eval (list 'foreign-funcall
                              "objc_msgSend"
                              :pointer (slot-value object 'obj)
                              :pointer sel
                              return-type new-value))
                  (error "Cannot resolve Obj-C property setter for ~A: ~A does not respond to ~A"
                         property-name
                         object
                         setter-name))))
        new-value)
      (call-next-method)))


;;; Obj-C class => Lisp class converter

;;;; --- Helper ---
(defun class-ptr-symbol (ptr)
  (declare (inline class-ptr-symbol))
  (let ((name (objc-raw::class-get-name ptr)))
    (if (objc-raw::class-is-meta-class ptr)
      (translate-name-from-foreign name (find-package "OBJC-META"))
      (translate-name-from-foreign name (find-package "OBJC-CLASS")))))
;;;; --- Helper ---
(defun compute-slot-specs-for-properties (class-ptr)
  (with-foreign-object (out-count :uint)
    (let* ((props-ptr (objc-raw::class-copy-property-list class-ptr out-count))
           (len (mem-aref out-count :uint 0)))
      (unwind-protect
           (loop for i below len
                 for prop = (mem-aref props-ptr :pointer i)
                 for prop-name = (property-get-name prop)
                 for slot-name = (translate-name-from-foreign prop-name
                                                              (find-package "OBJC-PROP"))
                 collect (list :name slot-name
                               :property-name prop-name
                               :from-objc t))
        (foreign-free props-ptr)))))
;;;; --- END Helper ---
(defmethod c2mop:ensure-class-using-class
    :around ((class null) name &rest args
             &key direct-superclasses direct-slots metaclass
               objc-class-name objc-object (auto-bind-properties t))
  ;; Pass if it is not intended as an Obj-C class
  (unless (subtypep metaclass 'objc-class)
    (return-from c2mop:ensure-class-using-class (call-next-method)))
  
  (let (class-ptr)
    ;; If called with existing class
    (when (and objc-object (not (cffi:null-pointer-p objc-object)))
      (setq class-ptr objc-object))
    ;; Resolve names
    (if class-ptr
        (unless objc-class-name
          (setq objc-class-name
                (objc-raw::class-get-name class-ptr)))
        (unless objc-class-name
          (setq objc-class-name (translate-name-to-foreign name (find-package "OBJC-CLASS")))))
    ;; Try to find the possibly existing class
    (when (or (null class-ptr) (null-pointer-p class-ptr))
      (let ((ptr (objc-raw::objc-lookup-class objc-class-name)))
        (unless (null-pointer-p ptr)
          (setq class-ptr ptr))))

    (if class-ptr
        ;; Bind existing class to Lisp
        (progn
          (if (objc-raw::class-is-meta-class class-ptr)
              ;; Obj-C Metaclass
              (setq direct-superclasses
                    (let* ((base-class (objc-raw::objc-lookup-class objc-class-name))
                           (base-super (objc-raw::class-get-superclass base-class)))
                      (if (null-pointer-p base-super)
                          (list (find-class 'objc-class))
                          (list  (foreign-funcall
                                 "objc_getMetaClass"
                                 :string (objc-raw::class-get-name base-super)
                                 objc-class))))
                    metaclass (find-class 'objc-metaclass))
              ;; Normal class
              (setq direct-superclasses
                    (let ((super-ptr (objc-raw::class-get-superclass class-ptr)))
                      (if (null-pointer-p super-ptr)
                          (list (find-class 'objc-object))
                          (list (let ((super-name (class-ptr-symbol super-ptr)))
                                  (or (find-class super-name nil)
                                      (c2mop:ensure-class super-name
                                                          :objc-object super-ptr
                                                          :metaclass 'objc-class))))))
                    metaclass
                    (let ((meta-ptr (objc-raw::objc-get-meta-class objc-class-name)))
                      (c2mop:ensure-class (class-ptr-symbol meta-ptr)
                                          :objc-object meta-ptr
                                          :metaclass 'objc-class))))
          ;; Bind Obj-C property to Lisp slot
          (when auto-bind-properties
            (setq direct-slots
                  (nconc direct-slots (compute-slot-specs-for-properties class-ptr))))
          ;; Done
          (apply #'call-next-method class name
                 :direct-superclasses direct-superclasses
                 :direct-slots direct-slots
                 :metaclass metaclass
                 :objc-class-name objc-class-name
                 :objc-object class-ptr
                 args))
        ;; Defining new Obj-C class from Lisp
        (let ((super-ptr (aif (find-if (lambda (class) (typep class 'objc-class))
                                       direct-superclasses)
                              (slot-value it 'obj)
                              (cffi:null-pointer)))
              (meta-ptr (objc-raw::objc-get-meta-class objc-class-name)))
          (setq class-ptr (objc-raw::objc-allocate-class-pair super-ptr objc-class-name 0))
          (objc-raw::objc-register-class-pair class-ptr)
          (setq metaclass (c2mop:ensure-class (class-ptr-symbol meta-ptr)
                                              :objc-object meta-ptr
                                              :metaclass 'objc-class))
          (when auto-bind-properties
            (setq direct-slots
                  (nconc direct-slots (compute-slot-specs-for-properties class-ptr))))
          (apply #'call-next-method class name
                 :direct-superclasses direct-superclasses
                 :direct-slots direct-slots
                 :metaclass metaclass
                 :objc-class-name objc-class-name
                 :objc-object class-ptr
                 args)))))

;;;; For automatic Obj-C method => Lisp method binding
(defmethod initialize-instance :after ((class objc-class) &key)
  (when (slot-value class 'auto-register-methods)
    (loop for ptr across (class-method-list-pointers class)
          do (ensure-objc-method class ptr))))


;;; Helper Functions

(defun ensure-objc-class (object)
  ;; Basically just a typecase
  (cond ((pointerp object)
         (let ((name (class-ptr-symbol object)))
           (or (find-class name nil)
               (c2mop:ensure-class (class-ptr-symbol object)
                                   :objc-object object
                                   :metaclass 'objc-class))))
        ((typep object 'objc-class)
         object)
        ((symbolp object)
         (or (find-class object nil)
             (foreign-funcall
              "objc_lookUpClass"
              :string (translate-name-to-foreign object (find-package "OBJC-CLASS"))
              objc-class)))
        ((stringp object)
         (or (find-class
              (translate-name-from-foreign object (find-package "OBJC-CLASS"))
              nil)
             (foreign-funcall "objc_lookUpClass" :string object objc-class)))
        (t (error "Invalid argument: ~A" object))))

(defun ensure-objc-meta-class (name-or-ptr)
  (cond ((pointerp name-or-ptr)
         (ensure-objc-class name-or-ptr))
        ((symbolp name-or-ptr)
         (or (find-class name-or-ptr nil)
             (objc-get-meta-class
              (translate-name-to-foreign name-or-ptr (find-package "OBJC-META")))))
        ((stringp name-or-ptr)
         (or (find-class
              (translate-name-from-foreign name-or-ptr (find-package "OBJC-META"))
              nil)
             (objc-get-meta-class name-or-ptr)))
        (t (error "Invalid argument: ~A" name-or-ptr))))

;; (defclass test-object (objc-class:ns-object)
;;   ((prop :return-type :pointer))
;;   (:metaclass objc-class:ns-object))


;; Obj-C Object (id) foreign translator

(define-foreign-type objc-object-type () ()
  (:actual-type :pointer)
  (:simple-parser objc-id))

(defgeneric translate-from-objc-id (class pointer)
  (:method ((class objc-object) pointer)
    (make-instance class :objc-object pointer)))
(defgeneric translate-to-objc-id (object)
  (:method ((object objc-object))
    (when object (objc-obj object) (cffi:null-pointer))))

(defmethod translate-from-foreign (ptr (type objc-object-type))
  (unless (null-pointer-p ptr)
    (let ((class (foreign-funcall "object_getClass" :pointer ptr objc-class)))
      (translate-from-objc-id class ptr))))
(defmethod translate-to-foreign (obj (type objc-object-type))
  (translate-to-objc-id obj))
(defmethod expand-from-foreign (ptr (type objc-object-type))
  `(unless (null-pointer-p ,ptr)
     (let ((class (foreign-funcall "object_getClass" :pointer ,ptr objc-class)))
       (translate-from-objc-id class ,ptr))))
(defmethod expand-to-foreign (obj (type objc-object-type))
  `(translate-to-objc-id ,obj))


;; Wrapper for some class-info fetching functions

(macrolet ((copy-list-ptrs (func var)
             `(with-foreign-object (out-count :uint)
                (setf (mem-aref out-count :uint 0) 0)
                (let* ((ptr (,func (ensure-objc-class ,var) out-count))
                       (len (mem-aref out-count :uint 0))
                       (arr (make-array len :fill-pointer 0)))
                  (unwind-protect
                       (progn
                         (dotimes (i len)
                           (vector-push (mem-aref ptr :pointer i) arr))
                         arr)
                    (foreign-free ptr))))))
  (defun class-ivar-list-pointers (class)
    (copy-list-ptrs class-copy-ivar-list class))
  (defun class-property-list-pointers (class)
    (copy-list-ptrs class-copy-property-list class))
  (defun class-method-list-pointers (class)
    (copy-list-ptrs class-copy-method-list class)))


;; Obj-C Method

;;; Foreign Type

(define-foreign-type objc-method-type () ()
  (:actual-type :pointer)
  (:simple-parser objc-method))

(defmethod translate-from-foreign (ptr (type objc-method-type))
  (translate-to-objc-method ptr))
(defmethod translate-to-foreign (obj (type objc-method-type))
  (objc-obj obj))
(defmethod expand-from-foreign (ptr (type objc-method-type))
  `(translate-to-objc-method ,ptr))
(defmethod expand-to-foreign (obj (type objc-method-type))
  `(objc-obj ,obj))

;;; defclass

(defclass objc-method (standard-method objc-object)
  ((selector :initarg :selector)
   (return-type :initarg :return-type)))

;;; Convert to method

(defun translate-to-objc-method (ptr &rest arg-names)
  (unless (null-pointer-p ptr)
    (let* ((sel (foreign-funcall "method_getName" :pointer ptr selector))
           (argnum (- (objc-raw::method-get-number-of-arguments ptr) 2))
           (lambda-list (append arg-names
                                (loop repeat (- argnum (length arg-names))
                                      collect (gentemp "OBJC-METHOD-ARG-"))))
           (type-encodings (loop for i from 2 below (+ argnum 2)
                                 collect (objc-raw::method-copy-argument-type ptr i)))
           (specializers (loop for type in type-encodings
                               collect (parse-objc-type-encoding-to-lisp type)))
           (foreign-types (loop for type in type-encodings
                                collect (parse-objc-type-encoding-to-c type)))
           (ret-type (parse-objc-type-encoding-to-c (objc-raw::method-copy-return-type ptr)))
           (gf-proto     (c2mop:class-prototype (find-class 'standard-generic-function)))
           (method-proto (c2mop:class-prototype (find-class 'objc-method))))
      (multiple-value-bind (form initargs)
          (c2mop:make-method-lambda
           gf-proto method-proto
           `(lambda (objc-object ,@lambda-list)
              (foreign-funcall "method_invoke"
                               :pointer (objc-obj objc-object)
                               :pointer ,ptr
                               ,@(mapcan #'list foreign-types lambda-list)
                               ,ret-type))
           nil)
        (apply #'make-instance
               'objc-method
               :function (compile nil form)
               :lambda-list (cons 'objc-object lambda-list)
               :specializers (cons (find-class 'objc-object) specializers)
               :qualifiers nil
               :selector sel
               :return-type ret-type
               :objc-object ptr
               initargs)))))

;;; Convert to Generic Function

(defun ensure-objc-method (class object &rest arg-names)
  "Name can be a string, symbol, selector or cffi pointer of the method"
  (setq class (ensure-objc-class class))
  (let (name sel)
    (typecase object
      (selector (setq sel object
                      name (translate-name-from-foreign
                            (selector-name sel) (find-package "OBJC-METHOD"))))
      (string (setq sel (make-instance 'selector :name object)
                    name (translate-name-from-foreign object (find-package "OBJC-METHOD"))))
      (symbol (setq sel (make-instance
                         'selector
                         :name (translate-to-foreign object (find-package "OBJC-METHOD")))))
      (t (if (pointerp object)
             (setq sel (foreign-funcall
                        "sel_registerName"
                        :string (objc-raw::sel-get-name (objc-raw::method-get-name object))
                        selector)
                   name (translate-name-from-foreign
                         (selector-name sel) (find-package "OBJC-METHOD")))
             (error "Wrong type of argument: ~A" object))))
    (if (fboundp name) name
        (let ((method-ptr (foreign-funcall "class_getInstanceMethod"
                                           objc-class class
                                           selector sel
                                           :pointer)))
          (assert (not (null-pointer-p method-ptr)))
          (let ((gf (ensure-generic-function name)))
            (add-method gf (apply #'translate-to-objc-method method-ptr arg-names))
            gf)))))

;;; Defining new method

;; FIXME: Redefining old method will not flush some method cache at the Lisp side

(defmacro define-objc-method (name-and-options return-type args &body body)
  (multiple-value-bind (body decls)
      (alexandria:parse-body body)
    (let (lisp-name
          selector-name
          arg-and-types)
      (if (listp name-and-options)
          (setq lisp-name (first name-and-options)
                selector-name (second name-and-options))
          (setq lisp-name name-and-options
                selector-name (translate-name-to-foreign lisp-name (find-package "OBJC"))))
      (setq arg-and-types (append (list (first args)) '((_cmd selector)) (rest args)))
      (let* ((objc-arg-types (let ((types (make-array (1+ (length arg-and-types)) :element-type 'character :fill-pointer 0)))
                               (vector-push (parse-c-type-to-objc-encoding return-type) types)
                               (dolist (arg arg-and-types)
                                 (vector-push (parse-c-type-to-objc-encoding (second arg)) types))
                               types))
             (c-arg-types (loop for arg in arg-and-types
                                collect (list (first arg) (parse-lisp-type-to-c (second arg)))))
             (class-name (ensure-objc-class (second (first args))))
             (class-obj (objc-obj class-name)))
        `(let ((sel (make-instance 'selector :name ,selector-name)))
           (defcallback ,lisp-name ,return-type ,c-arg-types
             ,(if decls (append decls '((ignorable _cmd)))
                  '(declare (ignorable _cmd)))
             ,@body)
           (objc-raw::class-replace-method ,class-obj (objc-obj sel) (callback ,lisp-name) ,objc-arg-types)
           (ensure-objc-method ,class-name (objc-raw::class-get-instance-method ,class-obj (objc-obj sel)))))))
  )

;; Other Obj-C runtime types

(defctype objc-ivar :pointer)
(defctype objc-property :pointer)
(defctype objc-imp :pointer) ;; void (*)(void) IMP (self, selector, ...)

(defcstruct objc-method-description
  (:name :string)
  (:types :string))

;; Basic Obj-C Runtime functions

(defcfun (class-get-name "class_getName" :library foundation)
    :string
  (cls objc-class))

(defcfun (class-get-superclass "class_getSuperclass" :library foundation)
    :pointer
  (cls objc-class))

(defcfun (class-is-meta-class "class_isMetaClass" :library foundation)
    :boolean
  (cls objc-class))

(defcfun (objc-get-instance-size "class_getInstanceSize" :library foundation)
    :unsigned-int
  (cls objc-class))

(defcfun (class-get-instance-variable "class_getInstanceVariable" :library foundation)
    objc-ivar
  (cls objc-class)
  (name :string))

(defcfun (class-get-class-variable "class_getClassVariable" :library foundation)
    objc-ivar
  (cls objc-class)
  (name :string))

(defcfun (class-add-ivar "class_addIvar" :library foundation)
    :boolean
  (class :pointer)
  (name :string)
  (size :ulong)
  (alignment :uint8)
  (types :string))

(defcfun (class-copy-ivar-list "class_copyIvarList" :library foundation)
    (:pointer objc-ivar)
  (cls objc-class)
  (out-count (:pointer :uint)))

(defcfun (class-get-ivar-layout "class_getIvarLayout" :library foundation)
    (:pointer :uint8)
  (cls objc-class))

(defcfun (class-set-ivar-layout "class_setIvarLayout" :library foundation)
    :void
  (cls objc-class)
  (layout (:pointer :uint8)))

(defcfun (class-get-weak-ivar-layout "class_getWeakIvarLayout" :library foundation)
    (:pointer :uint8)
  (cls objc-class))

(defcfun (class-set-weak-ivar-layout "class_setWeakIvarLayout" :library foundation)
    :void
  (cls objc-class)
  (layout (:pointer :uint8)))

(defcfun (class-get-property "class_getProperty" :library foundation)
    objc-property
  (cls objc-class)
  (name :string))

(defcfun (class-copy-property-list "class_copyPropertyList" :library foundation)
    :pointer
  (cls objc-class)
  (out-count (:pointer :uint)))

(defcfun (class-add-method "class_addMethod" :library foundation)
    :boolean
  (cls objc-class)
  (name selector)
  (imp objc-imp)
  (types :string))

(defcfun (class-get-instance-method "class_getInstanceMethod" :library foundation)
    objc-method
  (cls objc-class)
  (name selector))

(defcfun (class-get-class-method "class_getClassMethod" :library foundation)
    objc-method
  (cls objc-class)
  (name selector))

(defcfun (class-copy-method-list "class_copyMethodList" :library foundation)
    :pointer
  (cls objc-class)
  (out-count (:pointer :uint)))

(defcfun (class-replace-method "class_replaceMethod" :library foundation)
    objc-imp
  (cls objc-class)
  (name selector)
  (imp objc-imp)
  (types :string))

(defcfun (class-get-method-implementation "class_getMethodImplementation" :library foundation)
    ;; WTF is the class_getMethodImplementation_stret ?
    objc-imp
  (cls objc-class)
  (name selector))

(defcfun (class-responds-to-selector "class_respondsToSelector" :library foundation)
    :boolean
  (cls objc-class)
  (selector selector))

(defcfun (class-add-property "class_addProperty" :library foundation)
    :pointer
  (cls objc-class)
  (name :string)
  (attributes (:pointer (:struct objc-property-attribute)))
  (attribute-count :unsigned-int))

(defcfun (class-replace-property "class_replaceProperty" :library foundation)
    :void
  (cls objc-class)
  (name :string)
  (attributes (:pointer (:struct objc-property-attribute)))
  (attribute-count :unsigned-int))

(defcfun (class-get-version "class_getVersion" :library foundation)
    :int
  (cls objc-class))

(defcfun (class-set-version "class_setVersion" :library foundation)
    :void
  (cls objc-class)
  (version :int))

(defcfun (class-create-instance "class_createInstance" :library foundation)
    objc-id
  (cls objc-class)
  (extra-bytes :uint))


(defcfun (objc-allocate-class-pair "objc_allocateClassPair" :library foundation)
    objc-class
  (superclass objc-class)
  (name :string)
  (extra-bytes :uint))

(defcfun (objc-dispose-class-pair "objc_disposeClassPair" :library foundation)
    :void
  (cls objc-class))

(defcfun (objc-register-class-pair "objc_registerClassPair" :library foundation)
    :void
  (cls objc-class))

(defcfun (objc-duplicate-class-pair "objc_duplicateClassPair" :library foundation)
    objc-class
  (original objc-class)
  (name :string)
  (extra-bytes :uint))

(defcfun (object-get-indexed-ivar "object_getIndexedIvars" :library foundation)
    :void
  (obj objc-id))

(defcfun (object-get-ivar "object_getIvar" :library foundation)
    objc-id
  (obj objc-id)
  (ivar objc-ivar))

(defcfun (object-set-ivar "object_setIvar" :library foundation)
    :void
  (obj objc-id)
  (ivar objc-ivar)
  (value objc-id))

(defcfun (object-get-class-name "object_getClassName" :library foundation)
    :string
  (obj objc-id))

(defcfun (object-get-class "object_getClass" :library foundation)
    objc-class
  (obj objc-id))

(defcfun (object-set-class "object_setClass" :library foundation)
    objc-class
  (obj objc-id)
  (cls objc-class))

(defcfun (objc-copy-class-list "objc_copyClassList" :library foundation)
    objc-class
  (out-count (:pointer :uint)))

(defcfun (objc-lookup-class "objc_lookUpClass" :library foundation)
    objc-class
  (name :string))

(defcfun (objc-get-class "objc_getClass" :library foundation)
    objc-id
  (name :string))

(defcfun (objc-get-required-class "objc_getRequiredClass" :library foundation)
    objc-class
  (name :string))

(defcfun (objc-get-meta-class "objc_getMetaClass" :library foundation)
    objc-class
  (name :string))


(defcfun (ivar-get-name "ivar_getName" :library foundation)
    :string
  (var objc-ivar))

(defcfun (ivar-get-type-encoding "ivar_getTypeEncoding" :library foundation)
    :string
  (var objc-ivar))

(defcfun (ivar-get-offset "ivar_getOffset" :library foundation)
    :pointer
  (var objc-ivar))

;; objc_setAssociatedObject
;; objc_getAssociatedObject
;; objc_removeAssociatedObjects

(defcfun (method-get-name "method_getName" :library foundation)
    selector
  (method objc-method))

(defcfun (method-get-implementation "method_getImplementation" :library foundation)
    objc-imp
  (method objc-method))

(defcfun (method-get-type-encoding "method_getTypeEncoding" :library foundation)
    :string
  (method objc-method))

(defcfun (method-copy-return-type "method_copyReturnType" :library foundation)
    :string
  (method objc-method))

(defcfun (method-copy-argument-type "method_copyArgumentType" :library foundation)
    :string
  (method objc-method)
  (index :uint))

;; method_getReturnType

(defcfun (method-get-number-of-arguments "method_getNumberOfArguments" :library foundation)
    :uint
  (method objc-method))

;; method_getArgumentType
;; method_getDescription

(defcfun (method-set-implementation "method_setImplementation" :library foundation)
    objc-imp
  (method objc-method)
  (imp objc-imp))

(defcfun (method-exchange-implementation "method_exchangeImplementation" :library foundation)
    :void
  (method1 objc-method)
  (method2 objc-method))

;; objc_copyImageNames
;; class_getImageName
;; objc_copyClassNamesForImage

(defcfun (sel-get-name "sel_getName" :library foundation)
    :string
  (sel selector))

(defcfun (sel-register-name "sel_registerName" :library foundation)
    selector
  (name :string))

(defcfun (sel-get-uid "sel_getUid" :library foundation)
    selector
  (str :string))

;; sel_isEqual

(defcfun (property-get-name "property_getName" :library foundation)
    :string
  (prop objc-property))

(defcfun (property-copy-attribute-value "property_copyAttributeValue" :library foundation)
    :string
  (prop objc-property)
  (name :string))

(defcfun (property-get-attributes "property_getAttributes" :library foundation)
    :string
  (prop objc-property))

(defcfun (property-copy-attribute-list "property_copyAttributeList" :library foundation)
    (:pointer (:struct objc-property-attribute))
  (prop objc-property)
  (out-count (:pointer :uint)))

(defcfun (objc-msg-send "objc_msgSend" :library foundation)
    :pointer
  (instance objc-id)
  (selector selector)
  &rest)

(defcfun (method-invoke "method_invoke" :library foundation)
    :pointer
  (object objc-id)
  (method objc-method)
  &rest)

;; Utils

(defmacro cls (name)
  (typecase name
    (string `(objc-lookup-class ,name))
    (symbol (let ((real-sym (intern (symbol-name name) "OBJC-CLASS")))
              (export real-sym "OBJC-CLASS")
              `(ensure-objc-class ',real-sym)))
    (t `(ensure-objc-class ,name))))

(defmacro meta (name)
  (typecase name
    (string `(objc-get-meta-class ,name))
    (symbol (let ((real-sym (intern (symbol-name name) "OBJC-META")))
              (export real-sym "OBJC-META")
              `(ensure-objc-meta-class ',real-sym)))
    (t `(ensure-objc-meta-class ,name))))

(defun selector (object)
  (cond ((symbolp object)  (make-instance
                            'selector
                            :name (translate-name-to-foreign object (find-package "OBJC"))))
        ((stringp object)  (make-instance 'selector :name object))
        ((pointerp object) (make-instance 'selector :objc-object object))
        (t (error "Wrong type of object for selector: ~A" object))))

(defmacro sel (name)
  (let ((name (if (symbolp name)
                  (translate-name-to-foreign name (find-package "OBJC"))
                  name)))
    `(selector ,name)))

;; Final setting for compile-time availability

(c2mop:finalize-inheritance (find-class 'objc-method))

;; --- From now on the basic runtime is ready ---

;; Obj-C Protocol

;;; Lisp class

(unless (find-class 'objc-class:ns-object nil)
  (defclass objc-class:ns-object () () (:metaclass objc-class)))

(unless (find-class 'objc-class:protocol nil)
  (defclass objc-class:protocol  () () (:metaclass objc-class)))

(defmethod print-object ((obj objc-class::protocol) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (protocol-get-name obj) stream)))

;;; Foreign type
(define-foreign-type objc-protocol-type () ()
  (:actual-type :pointer)
  (:simple-parser objc-protocol))

(defmethod translate-from-foreign (ptr (type objc-protocol-type))
  (if (null-pointer-p ptr)
      (error "Obj-C protocol not found.")
      (make-instance 'objc-class::protocol :objc-object ptr)))
(defmethod translate-to-foreign (obj (type objc-protocol-type))
  (objc-obj obj))
(defmethod expand-from-foreign (ptr (type objc-protocol-type))
  `(if (null-pointer-p ,ptr)
       (error "Obj-C protocol not found.")
       (make-instance 'objc-class::protocol :objc-object ,ptr)))
(defmethod expand-to-foreign (obj (type objc-protocol-type))
  `(objc-obj ,obj))

(defun protocol-method-names (protocol &optional requiredp instance-method-p)
  (with-foreign-object (out-count :uint)
    (let* ((ptr (protocol-copy-method-description-list
                 protocol
                 requiredp instance-method-p
                 out-count))
           (len (mem-aref out-count :uint 0))
           (arr (make-array len :fill-pointer 0)))
      (unwind-protect
           (progn
             (dotimes (i len)
               (vector-push (mem-aref ptr 'selector i) arr))
             arr)
        (foreign-free ptr)))))

;;; Protocol-related Obj-C runtime functions

(defcfun (class-add-protocol "class_addProtocol" :library foundation)
    :boolean
  (class objc-class)
  (protocol objc-protocol))

(defcfun (class-conforms-to-protocol "class_conformsToProtocol" :library foundation)
    :boolean
  (class objc-class)
  (protocol objc-protocol))

(defcfun (objc-get-protocol "objc_getProtocol" :library foundation)
    objc-protocol
  (name :string))

(defcfun (objc-copy-protocol-list "objc_copyProtocolList" :library foundation)
    :pointer
  (out-count (:pointer :unsigned-int)))

(defcfun (objc-allocate-protocol "objc_allocateProtocol" :library foundation)
    objc-protocol
  (name :string))

(defcfun (objc-register-protocol "objc_registerProtocol" :library foundation)
    :void
  (protocol objc-protocol))

(defcfun (protocol-add-method-description "protocol_addMethodDescription" :library foundation)
    :void
  (protocol objc-protocol)
  (name selector)
  (types :string)
  (is-required-method :boolean)
  (is-instance-method :boolean))

(defcfun (protocol-add-protocol "protocol_addProtocol" :library foundation)
    :void
  (protocol objc-protocol)
  (addition objc-protocol))

(defcfun (protocol-get-name "protocol_getName" :library foundation)
    :string
  (protocol objc-protocol))

(defcfun (protocol-is-equal "protocol_isEqual" :library foundation)
    :void
  (protocol1 objc-protocol)
  (protocol2 objc-protocol))

(defcfun (protocol-copy-method-description-list "protocol_copyMethodDescriptionList"
                                                :library foundation)
    (:pointer (:struct objc-method-description))
  (protocol objc-protocol)
  (is-required-method :boolean)
  (is-instance-method :boolean)
  (out-count (:pointer :uint)))

;; (defcfun (protocol-get-method-description "protocol_getMethodDescription" :library foundation)
;;     (:struct objc-method-description)
;;   (protocol objc-protocol)
;;   (selector selector)
;;   (is-required-method :boolean)
;;   (is-instance-method :boolean))

(defcfun (protocol-copy-property-list "protocol_copyPropertyList" :library foundation)
    (:pointer objc-property)
  (protocol objc-protocol)
  (out-count (:pointer :uint)))

(defcfun (protocol-get-property "protocol_getProperty" :library foundation)
    objc-property
  (protocol objc-protocol)
  (name :string)
  (is-required-method :boolean)
  (is-instance-method :boolean))

(defcfun (protocol-copy-protocol-list "protocol_copyProtocolList" :library foundation)
    (:pointer objc-protocol)
  (protocol objc-protocol)
  (out-count (:pointer :uint)))

(defcfun (protocol-conforms-to-protocol "protocol_conformsToProtocol" :library foundation)
    :void
  (protocol objc-protocol)
  (other objc-protocol))


;; Basic Obj-C objects

(eval-when (:load-toplevel :execute)
  (dolist (name '("NSNumber" "NSValue"
                  "NSString" "NSMutableString" "NSArray" "NSMutableArray"
                  "NSSet" "NSMutableSet" "NSDictionary" "NSMutableDictionary"))
    (ensure-objc-class name)))

(defmethod translate-from-objc-id ((class objc-meta::ns-number) ptr)
  (foreign-funcall "objc_msgSend"
                   :pointer ptr
                   selector (sel "doubleValue")
                   :double))
(defmethod translate-to-objc-id ((number integer))
  (foreign-funcall "objc_msgSend"
                   objc-class (cls ns-number)
                   selector (sel "numberWithLongLong:")
                   :long-long number
                   :pointer))
(defmethod translate-to-objc-id ((number single-float))
  (foreign-funcall "objc_msgSend"
                   objc-class (cls ns-number)
                   selector (sel "numberWithSingle:")
                   :float number
                   :pointer))
(defmethod translate-to-objc-id ((number double-float))
  (foreign-funcall "objc_msgSend"
                   objc-class (cls ns-number)
                   selector (sel "numberWithDouble:")
                   :double number
                   :pointer))

(defmethod translate-from-objc-id ((class objc-meta::ns-string) ptr)
  (foreign-funcall "objc_msgSend"
                   :pointer ptr
                   selector (sel "UTF8String")
                   :string))
(defmethod translate-to-objc-id ((obj string))
  (foreign-funcall "objc_msgSend"
                   :pointer (objc-obj (cls ns-string))
                   selector (sel "stringWithUTF8String:")
                   :string obj
                   :pointer))

(defcstruct (ns-range :class ns-range-tclass)
  (location :ulong)
  (length :ulong))

(defmethod translate-from-foreign :around (ptr (type ns-range-tclass))
  (let ((plist (call-next-method)))
    (cons (getf plist 'location) (getf plist 'length))))
(defmethod translate-into-foreign-memory :around (value (type ns-range-tclass) ptr)
  (call-next-method (list 'location (car value) 'length (cdr value)) type ptr))



;; Utils like LispWorks Obj-C bridge

(defun alloc-init-object (class)
  (when (stringp class)
    (setq class (objc-lookup-class class)))
  (make-instance class))

(defun invoke (object sel &rest args)
  (when (stringp object)
    (setq object (objc-lookup-class object)))
  (when (stringp sel)
    (setq sel (selector sel)))
  (apply sel object args))

(setf (fdefinition 'coerce-to-objc-class) #'ensure-objc-class)
(setf (fdefinition 'coerce-to-selector) #'selector)

(defmacro current-super (object)
  (when (stringp object)
    (setq object (objc-lookup-class object)))
  (class-get-superclass object))

(defun can-invoke-p (object sel)
  (when (stringp object)
    (setq object (objc-lookup-class object)))
  (when (stringp sel)
    (setq sel (selector sel)))
  (class-responds-to-selector (class-of object) sel))

(defun string-to-ns-string (string)
  (funcall (sel string-with-utf8-string.) (cls ns-string) string))


;; Fancy time...

(eval-when (:load-toplevel :execute)
  (dolist (name '("NSApplication" "NSWorkspace" "NSPasteboard" "NSPasteboardItem"
                  "NSDocument" "NSDocumentController" "NSPersistentDocument"
                  "NSUserDefaultsController"
                  "NSFilePromiseProvider" "NSFilePromiseReceiver"

                  "NSView" "NSControl" "NSCell" "NSActionCell"
                  "NSGridView" "NSGridCell" "NSGridColumn" "NSGridRow"
                  "NSSplitView" "NSStackView" "NSTabView"
                  "NSScrollView" "NSScroller" "NSClipView" "NSRulerView" "NSRulerMarker"))
    (ensure-objc-class name)))

;; Example: to get the system pasteboard

;; (objc-method:string-for-type.
;;  (slot-value (find-class 'objc-class:ns-pasteboard) 'objc-prop:general-pasteboard)
;;  *ns-pasteboard-type-string*)

(defcvar ("NSPasteboardTypeURL" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeColor" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeFileURL" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeFont" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeHTML" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeMultipleTextSelection" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypePDF" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypePNG" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeRTF" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeRTFD" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeRuler" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeSound" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeString" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeTabularText" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeTextFinderOptions" :read-only t :library appkit)
  objc-id)
(defcvar ("NSPasteboardTypeTIFF" :read-only t :library appkit)
  objc-id)

(defcenum ns-pasteboard-reading-options :data :string :property-list :keyed-archive)


;; TODO:
;; - Parse type encoding for array, structure, union, bits
;; - Helper function & reader for Class, SEL, NSString, NSNumber...
