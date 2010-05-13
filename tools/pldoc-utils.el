;; pldoc-utils.el --- a major utils for editing pldoc files
;;
;;
;; $Id: pldoc-utils.el,v 1.3 2007/05/03 23:03:40 cmungall Exp $
;;
;;
;; Adapted by Chris Mungall from DNA sequence utils
;;
;;; Installation:
;; --------------------
;; Here are two suggested ways for installing this package.
;; You can choose to autoload it when needed, or load it
;; each time emacs is started.  Put one of the following
;; sections in your .emacs:
;;
;; ---Autoload:
;;  (autoload 'pldoc-utils "pldoc-utils" "Major utils for dna" t)
;;  (add-to-list
;;     'auto-utils-alist
;;     '("\\.\\(pldoc\\|PLDOC\\)\\'" . pldoc-utils))
;;  (add-hook 'pldoc-utils-hook 'turn-on-font-lock)
;;
;; ---Load:
;;  (setq pldoc-do-setup-on-load t)
;;  (load "/pathname/pldoc-utils")


(defvar pldoc-author "Chris Mungall"
  "*author")

(defvar pldoc-license "License"
  "*license")


(defun pldoc-setup (namespace db-prefix)
  "initalises vars"
  (interactive "snamespace: \nsdb-prefix: ")
  (setq pldoc-namespace namespace)
  (setq pldoc-db-prefix db-prefix))

(defun pldoc-add-module-header (name)
  ""
  (insert (format ":- module(%s,[]).\n\n" name)))

(defun pldoc-new-module (name description)
  "Adds structured comments for module"
  (interactive "sName: \nsDescription: ")
  (progn
    (pldoc-add-module-header name)
    (pldoc-add-module-comments description)))


(defun pldoc-add-module-comments (description)
  "Adds structured comments for module"
  (interactive "sDescription: ")
  (let (see
        see-list)
    (setq see-list '("README"))
    (while (progn
             (setq see (read-string "see:"))
             (and
              (> (length see) 0)
              (setq see-list (append see-list (list see))))))
    (insert (format "/** <module> %s\n\n  %s\n%s\n*/\n"
                    description
                    (module-comments-sections)
                    (module-comments-tags see-list)))))

(defun module-comments-synopsis-section ()
  "tags"
  (format "---+ Synopsis\n\n==\n:- use_module(bio(%s)).\n\n%% \ndemo:-\n  nl.\n  \n\n==\n\n"
          (get-prolog-module-name)))

(defun module-comments-details-section ()
  "tags"
  (format "---+ Details\n\n\n"))

(defun module-comments-sections ()
  "Synopsis + Details"
  (format "%s%s" (module-comments-synopsis-section) (module-comments-details-section)))

(defun module-comments-tags (see-list)
  "tags"
  (format "---+ Additional Information\n\nThis module is part of blip. For more details, see http://www.blipkit.org\n\n@author  %s\n@version $Revision$\n@see     %s\n@license %s\n\n"
          pldoc-author
          (mapconcat
           (lambda (x) x)
           see-list
           ", ")
          pldoc-license))

(defun pldoc-add-module-comment-tags ()
  (interactive "")
  (insert (module-comments-tags '("README"))))


(defun get-prolog-module-name ()
  ""
  (car (split-string (car (reverse (split-string (buffer-file-name) "/"))) "\\.")))

(defun pldoc-add-new-predicate (description)
  "Adds structured comments for module"
  (interactive "sDescription: ")
  (insert (format "xxxx")))


