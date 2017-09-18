#!/usr/local/bin/sbcl --script

(load "~/.sbclrc")

(require :cl-ppcre)
(require :cl-sxml)

(defun >> (&rest args)
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
			(unless (= (length item) 2) (error "bad attributes"))
			(cons (car item) (cadr item))) list)
	      :test #'string=)))

(defun emit-client-namespace (protocol-name)
  (terpri)
  (>> "namespace " protocol-name "_client")
  (>> "{")
  (>> "}")
  (terpri))

(defun emit-client-include-guard (protocol-name)
  (let ((cap-name (string-upcase protocol-name)))
    (>> "#ifndef INCLUDE_" cap-name "_CLIENT_H_")
    (>> "#define INCLUDE_" cap-name "_CLIENT_H_")
    (emit-client-namespace protocol-name)
    (>> "#endif/*INCLUDE_" cap-name "_CLIENT_H_*/")))

(defun emit-root (root)
  (unless (string= (car root) "protocol") (error "bad format"))
  (let ((protocol-name (attribute (cadr root) "name")))
    (emit-client-include-guard protocol-name)))

(let ((root (preprocess (cxml:parse *standard-input* (cxml-xmls:make-xmls-builder)))))
  (emit-root root))

(terpri)

