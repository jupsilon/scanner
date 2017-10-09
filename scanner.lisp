;#!/usr/local/bin/sbcl --script

(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(let ((indent-unit "  ")
      (indent-level 0)
      (output *standard-output*))
  (defun << (&rest args)
    (if (null args)
	(terpri output)
	(progn
	  (dotimes (i indent-level) (princ indent-unit output))
	  (dolist (item args)
	    (princ item output))
	  (terpri output))))
  (defun current-output ()
    output)
  (defun set-indent-unit (new-value)
    (setf indent-unit new-value))
  (defun indent ()
    (incf indent-level))
  (defun unindent ()
    (decf indent-level))
  (defun s/<<-lambda (body)
    (let (prev-output)
      (unwind-protect
	   (with-output-to-string (stream)
	     (setf prev-output output)
	     (setf output stream)
	     (funcall body))
	(setf output prev-output)))))

(defmacro s/<< (&body body)
  `(s/<<-lambda (lambda () ,@body)))

(defmacro indent/<< (&body body)
  `(unwind-protect
	(progn
	  (indent)
	  ,@body)
     (unindent)))

(defun node-get-label (node)
  (car node))

(defun node-get-attributes (node)
  (mapcar (lambda (item) (cons (car item) (cadr item))) (cadr node)))

(defun attributes-get-value (attributes key)
  (cdr (assoc key attributes :test #'string=)))

(defun node-get-value (node key)
  (attributes-get-value (node-get-attributes node) key))

(defun node-get-nodes (node)
  (remove-if #'stringp (cddr node)))

(defun node-get-rest (node)
  (apply #'concat (remove-if-not #'stringp (cddr node))))

(defun nodes-select (nodes label)
  (remove-if-not (lambda (node) (string= (node-get-label node) label)) nodes))

(defun nodes-select-not (nodes label)
  (remove-if     (lambda (node) (string= (node-get-label node) label)) nodes))

(defun preprocess (protocol)
  (remove-if (lambda (item) (if (stringp item) (not (null (ppcre:scan "^\\s+$" item)))))
	     (mapcar (lambda (item)
		       (cond ((listp item) (preprocess item))
			     (t item)))
		     protocol)))

(defun trim-lines (lines)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" lines))))

(defun wire-type (type)
  (cond ((string= type "int"   ) "int32_t"    )
	((string= type "uint"  ) "uint32_t"   )
	((string= type "fixed" ) "wl_fixed_t" )
	((string= type "string") "char const*")
	((string= type "array" ) "wl_array"   )
	((string= type "fd"    ) "int32_t"    )
	(t (error "unknown type ~a" type))))

(defun node-get-brief (node)
  (when (not (null node))
    (node-get-value node "summary")))

(defun node-get-lines (node)
  (when (not (null node))
    (trim-lines (node-get-rest node))))

(defun emit-lines (lines header-first header-rest)
  (when (not (null lines))
    (<< header-first (car lines))
    (dolist (line (cdr lines))
      (<< header-rest line))))

(defun params-formal-parameters-list (params)
  (mapcar (lambda (param) (concat (elt param 0) " " (elt param 1))) params))

(defun emit-formal-parameters-list (formal-parameters-list)
  (if (null formal-parameters-list)
      "()"
      (format nil "(~{~a~^, ~})" formal-parameters-list)))

(defun emit-result-type (result)
  (if (null result)
      "void"
      (elt result 0)))

; params := (type name summary cref-p)
(defun args-get-params (args)
  (mapcar (lambda (arg)
	    (let* ((attributes (node-get-attributes arg))
		   (type       (attributes-get-value attributes "type")))
	      (list (if (string= type "object")
			(let ((interface (attributes-get-value attributes "interface")))
			  (if (null interface)
			      "void*"
			      (concat (attributes-get-value attributes "interface") "_t const&")))
			(wire-type type))
		    (attributes-get-value attributes "name")
		    (attributes-get-value attributes "summary")
		    (and (string= type "object") (not (null (attributes-get-value attributes "interface")))))))
	  (remove-if (lambda (arg) (string= (node-get-value arg "type") "new_id")) args)))

(defun args-get-params-c (args)
  (mapcar (lambda (arg)
	    (let* ((attributes (node-get-attributes arg))
		   (type       (attributes-get-value attributes "type")))
	      (list (if (string= type "object")
			(let ((interface (attributes-get-value attributes "interface")))
			  (if (null interface)
			      "void*"
			      (concat (attributes-get-value attributes "interface") "*")))
			(wire-type type))
		    (attributes-get-value attributes "name")
		    (attributes-get-value attributes "summary")
		    (and (string= type "object") (not (null (attributes-get-value attributes "interface")))))))
	  (remove-if (lambda (arg) (string= (node-get-value arg "type") "new_id")) args)))

(defun args-get-result (args)
  (let ((arg (find-if (lambda (arg) (string= (node-get-value arg "type") "new_id")) args)))
    (when (not (null arg))
      (let* ((attributes (node-get-attributes arg))
	     (interface  (attributes-get-value attributes "interface")))
	(list (if (null interface)
		  "void*"
		  (concat interface "_t"))
	      nil
	      (attributes-get-value attributes "summary"))))))

(defun emit-client-request-decl (request)
  (let* ((attributes   (node-get-attributes request))
	 (name         (attributes-get-value attributes "name"))
	 (type         (attributes-get-value attributes "type"))
	 (children     (node-get-nodes request))
	 (args         (nodes-select children "arg"))
	 (params       (args-get-params args))
	 (result       (args-get-result args))
	 (descriptions (nodes-select children "description"))
	 (brief        (node-get-brief (car descriptions)))
	 (notes        (node-get-lines (car descriptions))))
    (indent/<<
      (when (not (null brief))
	(<<       "/// @brief   " brief))
      (dolist (param params)
	(let ((param-name    (elt param 1))
	      (param-summary (elt param 2)))
	  (if (null param-summary)
	      (<< "/// @param   " param-name)
	      (<< "/// @param   " param-name " " param-summary))))
      (when (not (null result))
	(let ((result-type    (elt result 0))
	      (result-summary (elt result 2)))
	  (<<     "/// @return  " result-type)
	  (when (not (null result-summary))
	    (<<   "/// @retval  " result-summary))))
      (when (and (not (null type)) (string= type "destructor"))
	(<<       "/// @remarks the server destructor"))
      (emit-lines notes
		  "/// @note    "
		  "///          ")
      (let ((formal-list (params-formal-parameters-list params)))
	(if (and (not (null type)) (string= type "destructor"))
	    (<<   (emit-result-type result) " " name (emit-formal-parameters-list formal-list) ";")
	    (<<   (emit-result-type result) " " name (emit-formal-parameters-list formal-list) " const;"))))))

(defun params-parameter-types-list (params)
  (mapcar (lambda (param) (elt param 0)) params))

(defun emit-parameter-types-list (parameter-types-list)
  (if (null parameter-types-list)
      "()"
      (format nil "(~{~a~^, ~})" parameter-types-list)))

(defun emit-client-listener-handler-binder (events interface-name)
  (indent/<<
    (dolist (event events)
      (let* ((name         (node-get-value event "name"))
	     (args         (nodes-select (node-get-nodes event) "arg"))
	     (params       (args-get-params args))
	     (params-c     (args-get-params-c args))
	     (formal-list  (cons "void* data" (cons (concat interface-name "* target") (params-formal-parameters-list params-c))))
	     (actual-list  (cons "self->data_" (cons "cref(target)" (params-actual-arguments-list params)))))
	(<< "[]" (emit-formal-parameters-list formal-list) " -> void {")
	(indent/<<
	  (<< "auto const self  = reinterpret_cast<" interface-name "_t*>(data);")
	  (<< "self->on_" name (emit-actual-arguments-list actual-list) ";"))
	(<< "},")))))

(defun emit-client-listener-handler (event interface-name)
  (let* ((name         (node-get-value event "name"))
	 (children     (node-get-nodes event))
	 (args         (nodes-select children "arg"))
	 (params       (args-get-params args))
	 (result       (args-get-result args))
	 (descriptions (nodes-select children "description"))
	 (brief        (node-get-brief (car descriptions)))
	 (notes        (node-get-lines (car descriptions)))
	 (formal-list  (cons "void* data" (cons (concat interface-name "_t const& self") (params-formal-parameters-list params))))
	 (types-list   (cons "void*" (cons (concat interface-name "_t const&") (params-parameter-types-list params)))))
    (indent/<<
      (when (not (null brief))
	(<<       "/// @brief   " brief))
      (dolist (param params)
	(let ((param-name    (elt param 1))
	      (param-summary (elt param 2)))
	  (if (null param-summary)
	      (<< "/// @param   " param-name)
	      (<< "/// @param   " param-name " " param-summary))))
      (when (not (null result))
	(let ((result-type    (elt result 0))
	      (result-summary (elt result 2)))
	  (<<     "/// @return  " result-type)
	  (when (not (null result-summary))
	    (<<   "/// @retval  " result-summary))))
      (emit-lines notes
		  "/// @note    "
		  "///          ")
      (<<         "virtual " (emit-result-type result) " on_" name (emit-formal-parameters-list formal-list) " const;")
      (<<)
      (<<         "/// @brief   multi-delegator for " name " event")
      (<<         "delegator<" (emit-result-type result) " " (emit-parameter-types-list types-list) "> " name ";"))))

(defun emit-client-interface-members (interface-members interface-name)
  (let ((requests (nodes-select interface-members "request"))
	(events   (nodes-select interface-members "event")))
    (when (not (null requests))
      (<<)
      (<<     "public:")
      (format (current-output)
	      "~{~a~^~%~}"
	      (mapcar (lambda (request) (s/<< (emit-client-request-decl request))) requests)))
    (when (not (null events))
      (<<)
      (<<     "public:")
      (indent/<<
	(<<     "/// @class " interface-name "::listener")
	(<<     "/// @brief wrapper for " interface-name "_listener")
	(<<     "class listener {")
	(<<     "public:")
	(indent/<<
	  (<<     "listener(void* data = nullptr)")
	  (<<     "  : data_(data)")
	  (<<     "{")
	  (indent/<<
	    (<<     "this->listener_ = " interface-name "_listener {")
	    (emit-client-listener-handler-binder events interface-name)
	    (<<     "};"))
	  (<<     "}"))
	(<<)
	(<<     "public:")
	(format (current-output)
		"~{~a~^~%~}"
		(mapcar (lambda (event) (s/<< (emit-client-listener-handler event interface-name))) events))
	(<<)
	(<<     "private:")
	(indent/<<
	  (<<   "void* data_;")
	  (<<    interface-name "_listener listener_;"))
	(<<     "};")))))

(defun emit-client-interface (interface)
  (let* ((attributes   (node-get-attributes interface))
	 (name         (attributes-get-value attributes "name"))
	 (version      (attributes-get-value attributes "version"))
	 (children     (node-get-nodes interface))
	 (members      (nodes-select-not children "description"))
	 (descriptions (nodes-select     children "description"))
	 (brief        (node-get-brief (car descriptions)))
	 (notes        (node-get-lines (car descriptions)))
	 (class        (concat name "_t"))
	 (base-class   (concat "wayland_client_core<" class ", " version ">")))
    (indent/<<
      (<<         "/// @class " class)
      (when (not (null brief))
	(<<       "/// @brief " brief))
      (emit-lines notes
		  "/// @note  "
		  "///        ")
      (<<         "class " class " : public " base-class " {")
      (<<         "public:")
      (indent/<<
	(<<         "using c_struct_type = " name ";")
	(<<         "using base_type     = " base-class ";"))
      (<<)
      (<<         "public:")
      (indent/<<
	(<<         "/// @constructor")
	(<<         "/// @param src source raw pointer")
	(<<          class "(c_struct_type* src = nullptr)")
	(<<         "  : base_type(src)")
	(<<         "{")
	(<<         "}"))
      (emit-client-interface-members members name)
      (<<         "};"))))

(defun params-actual-arguments-list (params)
  (mapcar (lambda (param)
	    (if (elt param 3)
		(concat "cref(" (elt param 1) ")")
		(elt param 1)))
	    params))

(defun emit-actual-arguments-list (actual-arguments-list)
  (if (null actual-arguments-list)
      ""
      (format nil "(~{~a~^, ~})" actual-arguments-list)))

(defun emit-client-request-impl (request interface-name)
  (let* ((attributes   (node-get-attributes request))
	 (name         (attributes-get-value attributes "name"))
	 (type         (attributes-get-value attributes "type"))
	 (children     (node-get-nodes request))
	 (args         (nodes-select children "arg"))
	 (params       (args-get-params args))
	 (result       (args-get-result args))
	 (formal-list  (params-formal-parameters-list params))
	 (actual-list  (cons "this->get()" (params-actual-arguments-list params))))
    (if (and (not (null type)) (string= type "destructor"))
	(<< "inline " (emit-result-type result) " " interface-name "_t::" name (emit-formal-parameters-list formal-list) " {")
	(<< "inline " (emit-result-type result) " " interface-name "_t::" name (emit-formal-parameters-list formal-list) " const {"))
    (indent/<<
      (if (not (null result))
	  (<< "return " interface-name "_" name (emit-actual-arguments-list actual-list) ";")
	  (<< interface-name "_" name (emit-actual-arguments-list actual-list) ";")))
    (<< "}")))

(defun emit-client-listener-ctor (events interface-name)
  (<< events))

(defun emit-client-event-impl (event interface-name)
  (<< event))

(defun emit-client-interface-members-impl (interface-members interface-name)
  (let ((requests (nodes-select interface-members "request"))
	(events   (nodes-select interface-members "event")))
    (format (current-output)
	    "~{~a~^~%~}"
	    (mapcar (lambda (request) (s/<< (emit-client-request-impl request interface-name))) requests))
    (when (not (null events))
      (emit-client-listener-ctor events interface-name)
      (format (current-output)
	      "~{~a~^~%~}"
	      (mapcar (lambda (event) (s/<< (emit-client-event-impl event interface-name))) events)))))

(defun emit-client-interface-impl (interface)
  (let ((name    (node-get-value interface "name"))
	(members (nodes-select-not (node-get-nodes interface) "description")))
    (indent/<<
      (<< "//")
      (<< "// implementaions for " name)
      (<< "//")
      (emit-client-interface-members-impl members name))))

(defun emit-client-interfaces (interfaces)
  (format (current-output)
	  "~{~a~^~%~}"
	  (mapcar (lambda (interface) (s/<< (emit-client-interface interface))) interfaces)))

;(defun emit-client-listeners (interfaces)
;  (let ((interfaces-with-listener (find-if (lambda (interface) (node-get-nodes interface)

(defun emit-client-interfaces-decl (interfaces)
  (indent/<<
    (dolist (interface interfaces)
      (<< "class " (node-get-value interface "name") "_t;"))))

(defun emit-client-listeners-decl (interfaces)
  (indent/<<
    (dolist (interface interfaces)
      (when (not (null (nodes-select (node-get-nodes interface) "event")))
	(<< "class " (node-get-value interface "name") "_listener_t;")))))

(defun emit-client-interfaces-impl (interfaces)
  (format (current-output)
	  "~{~a~^~%~}"
	  (mapcar (lambda (interface) (s/<< (emit-client-interface-impl interface))) interfaces)))

(defun emit-client (root)
  (let ((protocol-name (node-get-value root "name"))
	(nodes (node-get-nodes root)))
    (let* ((descriptions (nodes-select nodes "description"))
	   (brief        (node-get-brief (car descriptions)))
	   (notes        (node-get-lines (car descriptions)))
	   (copyrights   (nodes-select nodes "copyright"))
	   (rights-notes (node-get-lines (car copyrights)))
	   (interfaces   (nodes-select nodes "interface")))
      (<<         "/// @file      " protocol-name "-client.hpp")
      (<<         "/// @brief     " brief)
      (emit-lines notes
		  "/// @note      "
		  "///            ")
      (emit-lines rights-notes
		  "/// @copyright "
		  "///            ")
      (<<)
      (<<         "#ifndef INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_")
      (<<         "#define INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_")
      (<<)
      (<<         "namespace wayland_client_wrappers")
      (<<         "{")
      (when (not (null interfaces))
	(emit-client-interfaces-decl interfaces)
	(emit-client-listeners-decl interfaces)
	(<<)
	(emit-client-interfaces interfaces)
	;(emit-client-listeners interfaces)
	(<<)
	(emit-client-interfaces-impl interfaces))
      (<<         "} // namespace wayland_client_wrappers")
      (<<)
      (<<         "#endif/*INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_*/"))))

;(emit-client (preprocess (cxml:parse      *standard-input*           (cxml-xmls:make-xmls-builder))))

(set-indent-unit "    ")
;(emit-client (preprocess (cxml:parse-file "weston-desktop-shell.xml"         (cxml-xmls:make-xmls-builder))))
;(emit-client (preprocess (cxml:parse-file "wayland.xml"                      (cxml-xmls:make-xmls-builder))))
;(emit-client (preprocess (cxml:parse-file "relative-pointer-unstable-v1.xml" (cxml-xmls:make-xmls-builder))))
(emit-client (preprocess (cxml:parse-file "xdg-foreign-unstable-v1.xml"      (cxml-xmls:make-xmls-builder))))
