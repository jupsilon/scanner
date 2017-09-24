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

(defun attribute (list key)
  (cdr (assoc key
	      (mapcar (lambda (item)
			(unless (= (length item) 2) (error "bad attributes: ~a" list))
			(cons (car item) (cadr item))) list)
	      :test #'string=)))

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

(defun emit-formal-parameters (args)
  (if (null args) "(void) -> void"
      (progn
	(unless (string= (caar args) "arg") (error "missing arg"))
	(let ((retn (find-if   (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args))
	      (prms (remove-if (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args)))
	  (if (null retn)
	      (setf retn "void")
	      (let ((interface-name (attribute (cadr retn) "interface")))
		(if (null interface-name)
		    (setf retn "void*")
		    (setf retn (concat (attribute (cadr retn) "interface") "_t")))))
	  (if (null prms)
	      (setf prms "(void)")
	      (progn
		(setf prms (mapcar (lambda (arg) (if (string= (attribute (cadr arg) "type") "object")
						     (concat  (attribute (cadr arg) "interface") "* "
							      (attribute (cadr arg) "name"))
						     (concat  (wire-type (attribute (cadr arg) "type")) " "
							      (attribute (cadr arg) "name"))))
				   prms))
		(setf prms (format nil "(~{~a~^, ~})" prms))))
	  (concat prms " -> " retn)))))

(defun emit-client-request-decl (member)
  (indent/<<
    (let* ((name         (attribute (cadr member) "name"))
	   (children     (node-get-nodes member))
	   (args         (nodes-select-not children "description"))
	   (descriptions (nodes-select     children "description"))
	   (brief        (node-get-brief (car descriptions)))
	   (notes        (node-get-lines (car descriptions))))
      (when (not (null brief))
	(<<           "/// @brief  " brief                          ))
      (when (not (null args))
	(let ((retn (find-if   (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args))
	      (prms (remove-if (lambda (arg) (string= (attribute (cadr arg) "type") "new_id")) args)))
	  (dolist (prm prms)
	    (let ((param-name    (attribute (cadr prm) "name"))
		  (param-summary (attribute (cadr prm) "summary")))
	      (if (not (null param-summary))
		  (<< "/// @param  " param-name " " param-summary   )
		  (<< "/// @param  " param-name                     ))))
	  (when (not (null retn))
	    (let ((return-summary (attribute (cadr retn) "summary")))
	      (if (not (null return-summary))
		  (<< "/// @return " return-summary                 )
		  (<< "/// @return " #|T.B.D.|#                     ))))))
      (emit-lines notes
		  "/// @note   "
		  "///         ")
      (<<             "auto " name (emit-formal-parameters args) ";"))))

(defun emit-client-interface-members (interface-members)
  (let ((requests (nodes-select interface-members "request")))
    (when (not (null requests))
      (<<          )
      (<< "public:")
      (format (current-output)
	      "~{~a~^~%~}"
	      (mapcar (lambda (request) (s/<< (emit-client-request-decl request))) requests)))))

(defun emit-client-interface (interface)
  (indent/<<
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
      (<<         "/// @class " class                         )
      (when (not (null brief))
	(<<       "/// @brief " brief                         ))
      (emit-lines notes
		  "/// @note  "
		  "///        ")
      (<<         "class " class " : public " base-class " {" )
      (<<         "public:"                                   )
      (indent/<<
	(<<         "using c_struct_type = " name ";"         )
	(<<         "using base_type     = " base-class ";"   ))
      (<<                                                    )
      (<<         "public:"                                   )
      (indent/<<
	(<<         "/// @constructor"                        )
	(<<         "/// @param src the source pointer"       )
	(<<          class "(c_struct_type* src = nullptr)"   )
	(<<         "  : base_type(src)"                      )
	(<<         "{"                                       )
	(<<         "}"                                       ))
      (emit-client-interface-members members)
      (<<         "};"                                        ))))

(defun emit-client-interfaces (interfaces)
  (format (current-output)
	  "~{~a~^~%~}"
	  (mapcar (lambda (interface) (s/<< (emit-client-interface interface))) interfaces)))

(defun emit-client (root)
  (let ((protocol-name (node-get-value root "name"))
	(nodes (node-get-nodes root)))
    (let* ((descriptions (nodes-select nodes "description"))
	   (brief        (node-get-brief (car descriptions)))
	   (notes        (node-get-lines (car descriptions)))
	   (copyrights   (nodes-select nodes "copyright"))
	   (rights-notes (node-get-lines (car copyrights)))
	   (interfaces   (nodes-select nodes "interface")))
      (<<         "/// @file      " protocol-name "-client.hpp"                    )
      (<<         "/// @brief     " brief)
      (emit-lines notes
		  "/// @note      "
		  "///            ")
      (emit-lines rights-notes
		  "/// @copyright "
		  "///            ")
      (<<         ""                                                               )
      (<<         "#ifndef INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_"  )
      (<<         "#define INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_"  )
      (<<         ""                                                               )
      (<<         "namespace " protocol-name "_client"                             )
      (<<         "{"                                                              )
      (emit-client-interfaces interfaces)
      (<<         "}"                                                              )
      (<<         ""                                                               )
      (<<         "#endif/*INCLUDE_" (string-upcase protocol-name) "_CLIENT_HPP_*/"))))

;(emit-client (preprocess (cxml:parse      *standard-input*           (cxml-xmls:make-xmls-builder))))

;(set-indent-unit "    ")
;(emit-client (preprocess (cxml:parse-file "weston-desktop-shell.xml"         (cxml-xmls:make-xmls-builder))))
;(emit-client (preprocess (cxml:parse-file "wayland.xml"                      (cxml-xmls:make-xmls-builder))))
(emit-client (preprocess (cxml:parse-file "relative-pointer-unstable-v1.xml" (cxml-xmls:make-xmls-builder))))

