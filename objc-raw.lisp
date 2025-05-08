;; Copyright (C) 2025 The Calendrical System
;; SPDX-License-Identifier: 0BSD

;; Raw Obj-C Runtime Binding Functions.
;; All symbols are internal, argument referenced by pointer.
;; For Utility.

(defpackage "OBJC-RAW"
  (:use "CL" "CFFI"))

(in-package "OBJC-RAW")

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

(defctype objc-class :pointer)
(defctype objc-selector :pointer)
(defctype objc-id :pointer)
(defctype objc-method :pointer)
(defctype objc-protocol :pointer)
(defctype objc-ivar :pointer)
(defctype objc-property :pointer)
(defctype objc-imp :pointer)
(defcstruct objc-property-attribute
  (:name :string)
  (:value :string))
(defcstruct objc-method-description
  (:name :string)
  (:types :string))

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
  (name objc-selector)
  (imp objc-imp)
  (types :string))

(defcfun (class-get-instance-method "class_getInstanceMethod" :library foundation)
    objc-method
  (cls objc-class)
  (name objc-selector))

(defcfun (class-get-class-method "class_getClassMethod" :library foundation)
    objc-method
  (cls objc-class)
  (name objc-selector))

(defcfun (class-copy-method-list "class_copyMethodList" :library foundation)
    :pointer
  (cls objc-class)
  (out-count (:pointer :uint)))

(defcfun (class-replace-method "class_replaceMethod" :library foundation)
    objc-imp
  (cls objc-class)
  (name objc-selector)
  (imp objc-imp)
  (types :string))

(defcfun (class-get-method-implementation "class_getMethodImplementation" :library foundation)
    ;; WTF is the class_getMethodImplementation_stret ?
    objc-imp
  (cls objc-class)
  (name objc-selector))

(defcfun (class-responds-to-selector "class_respondsToSelector" :library foundation)
    :boolean
  (cls objc-class)
  (selector objc-selector))

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
    objc-selector
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
  (sel objc-selector))

(defcfun (sel-register-name "sel_registerName" :library foundation)
    objc-selector
  (name :string))

(defcfun (sel-get-uid "sel_getUid" :library foundation)
    objc-selector
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
  (selector objc-selector)
  &rest)

(defcfun (method-invoke "method_invoke" :library foundation)
    :pointer
  (object objc-id)
  (method objc-method)
  &rest)
