;;; ob-julia --- Org babel support for Julia language

;; Copyright (C) 2020 Frédéric Santos
;; Based on G. J. Kerns' ob-julia.

;; Author: Frédéric Santos
;; Version: 2020-09-25
;; Keywords: babel, julia, literate programming, org
;; URL: https://gitlab.com/f-santos/ob-julia

;; This file is *not* part of GNU Emacs.

;;; Commentary:
;; This package intends to add an elementary support for Julia language
;; in Org mode.  It is still at a very early stage of development.

;;; Code:

;; Required packages:
(require 'cl-lib)
(require 'ess)
(require 'ob)

;; External functions from ESS:
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function ess-wait-for-process "ext:ess-inf"
		  (&optional proc sec-prompt wait force-redisplay))

;; Julia will be called as an ESS process:
(declare-function julia "ext:ess-julia" (&optional start-args))
(defcustom org-babel-julia-command "julia"
  "Name of command to use for executing Julia code."
  :group 'org-babel
  :package-version '(ob-julia . "2020-09-25")
  :version "27.1"
  :type 'string)

;; Defaults for Julia session and headers:
(defvar org-babel-default-header-args:julia '())
(defvar org-babel-julia-default-session "*julia*"
  "Default name given to a fresh new Julia session.")

;; Header args supported for Julia
;; (see `org-babel-insert-result'):
(defconst org-babel-header-args:julia
  '((results . ((file list scalar table vector verbatim)
		(raw html latex)
		(replace append none prepend silent)
		(output graphics value))))
  "Julia-specific header arguments.")

;; Set default extension to tangle Julia code:
(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling Julia sessions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-julia-initiate-session (session params)
  "Create a Julia process if there is no active SESSION yet.
SESSION is a string; check whether the associated buffer is a comint buffer.
If SESSION is `none', do nothing.
PARAMS are user-specified src block parameters."
  (unless (equal session "none")
    (let ((session (or session          ; if user-specified
                       org-babel-julia-default-session))
	  (ess-ask-for-ess-directory
	   (and (and (boundp 'ess-ask-for-ess-directory)
                     ess-ask-for-ess-directory)
		(not (cdr (assoc :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session                       ; session already exists
	(save-window-excursion
          (when (get-buffer session)
	    ;; Session buffer exists, but with dead process
	    (set-buffer session))
	  (progn (julia)                       ; create new Julia comint buffer
	         (rename-buffer
	          (if (bufferp session)
	              (buffer-name session)
	            (if (stringp session)
		        session
	              (buffer-name)))))
	  (current-buffer))))))

;; Retrieve ESS process info:
(defvar ess-current-process-name)       ; dynamically scoped
(defvar ess-local-process-name)         ; dynamically scoped
(defvar ess-ask-for-ess-directory)      ; dynamically scoped

(defun org-babel-edit-prep:julia (info)
"Function to edit Julia code in OrgSrc mode.
(I.e., for use with, and is called by, `org-edit-src-code'.)
INFO is a list as returned by `org-babel-get-src-block-info'."
  (let ((session (cdr (assq :session (nth 2 info)))))
    (when (and session
	       (string-prefix-p "*" session)
	       (string-suffix-p "*" session))
      (org-babel-julia-initiate-session session nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Executing Julia source blocks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-julia-evaluate
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate Julia code in BODY.
This can be done either within an existing SESSION, or with an external process.
This function only makes the convenient redirection towards the targeted
helper function, depending on this parameter."
  (if session
      (org-babel-julia-evaluate-session
       session body result-type result-params column-names-p row-names-p)
    (org-babel-julia-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(defun org-babel-expand-body:julia (body params &optional _graphics-file)
  "Expand BODY according to PARAMS, return the expanded body.
I.e., add :prologue and :epilogue to BODY if required, as well as new Julia
variables declared from :var.  The 'expanded body' is actually the union set
of BODY and of all those instructions."
  (mapconcat 'identity
	     (append
	      (when (cdr (assq :prologue params))
		(list (cdr (assq :prologue params))))
	      ;; TODO: (org-babel-variable-assignments:julia params)
	      (list body)
	      (when (cdr (assq :epilogue params))
		(list (cdr (assq :epilogue params)))))
	     "\n"))

(defun org-babel-execute:julia (body params)
  "Execute a block of Julia code.
The BODY on the block is first refactored with `org-babel-expand-body:julia',
according to user-specified PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((session-name (cdr (assq :session params)))
         (session (org-babel-julia-initiate-session session-name params))
         (expanded-body (org-babel-expand-body:julia body params)))
    (message session-name)))

(provide 'ob-julia)
;;; ob-julia.el ends here
