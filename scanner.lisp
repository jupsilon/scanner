#!/usr/local/bin/sbcl --script

(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun concat (&rest args)
  (apply #'concatenate (cons 'string args)))

(defun << (&rest args)
  (dolist (item args)
    (princ item))
  (terpri))

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

(defun resolve-members (node)
  (let (members
	summary
	text)
    (let ((desc (caddr node)))
      (if (string= (caaddr node) "description")
	  (progn
	    (setf summary (attribute (cadr  desc) "summary"))
	    (setf text               (caddr desc))
	    (setf members (cdddr node)))
	  (progn
	    (setf members desc)))
      (values members summary text))))

(defun trim-note (note)
  (remove-if (lambda (line) (string= line ""))
	     (mapcar (lambda (line) (ppcre:regex-replace "^\\s+" line ""))
		     (ppcre:split "\\n" note))))

(defun emit-client-request-decl (member)
  (let ((name (attribute (cadr member) "name"))
	(type (attribute (cadr member) "type")))
    (multiple-value-bind (args brief note) (resolve-members member)
      (when (not (null brief))
	(<< "    /// @brief " brief))
      (when (not (null note))
	(let ((lines (trim-note note))) (<< "    /// @note  " (car lines))
	     (dolist (rest (cdr lines)) (<< "    ///        " rest))))
      (<< "    void " name "();"))))

(defun emit-client-interface-members (interface-members)
  (dolist (member interface-members)
    (cond ((string= (car member) "request") (emit-client-request-decl member)))))

(defun emit-client-interface (interface)
  (unless (string= (car interface) "interface") (error "interface missing"))
  (let* ((name       (attribute (cadr  interface) "name"))
	 (version    (attribute (cadr  interface) "version"))
	 (class      (concat name "_t"))
	 (base-class (concat "wl_proxy_core<" class ", " version ">")))
    (multiple-value-bind (members brief note) (resolve-members interface)
      (when (not (null brief))
	(<< "  /// @class " class)
	(<< "  /// @brief " brief))
      (when (not (null note))
	(let ((lines (trim-note note))) (<< "  /// @note  " (car lines))
	     (dolist (rest (cdr lines)) (<< "  ///        " rest))))
      (<< "  class " class " : public " base-class " {" )
      (<< "  public:"                                   )
      (<< "    using protocol_type = " name ";"         )
      (<< "    using base_type     = " base-class ";"   )
      (<< ""                                            )
      (<< "  public:"                                   )
      (<< "    " class "(protocol_type* src = nullptr)" )
      (<< "      : base_typeo(src)"                     )
      (<< "    {"                                       )
      (<< "    }"                                       )
      (<< ""                                            )
      (<< "  public:"                                   )
      (emit-client-interface-members members)
      (<< "  };"                                        )
      (terpri))))

(defun emit-client-interfaces (interfaces)
  (dolist (interface interfaces)
    (emit-client-interface interface)))

(defun emit-client-namespace (interfaces protocol-name)
  (terpri)
  (<< "namespace " protocol-name "_client")
  (<< "{"                                 )
  (emit-client-interfaces interfaces)
  (<< "}"                                 )
  (terpri))

(defun emit-client-include-guard (interfaces protocol-name)
  (let ((cap-name (string-upcase protocol-name)))
    (<< "#ifndef INCLUDE_" cap-name "_CLIENT_H_")
    (<< "#define INCLUDE_" cap-name "_CLIENT_H_")
    (emit-client-namespace interfaces protocol-name)
    (<< "#endif/*INCLUDE_" cap-name "_CLIENT_H_*/")))

(defun emit-root (root)
  (unless (string= (car root) "protocol") (error "protocol missing"))
  (let ((protocol-name (attribute (cadr root) "name"))
	(interfaces (cddr root)))
    (emit-client-include-guard interfaces protocol-name)))

(let ((root (preprocess (cxml:parse *standard-input* (cxml-xmls:make-xmls-builder)))))
  (emit-root root))
