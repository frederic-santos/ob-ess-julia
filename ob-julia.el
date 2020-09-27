;;; ob-julia --- Org babel support for Julia language

;; Copyright (C) 2020 Frédéric Santos
;; Adapted from G. J. Kerns' ob-julia.

;; Author: Frédéric Santos
;; Version: 2020-09-25
;; Keywords: babel, julia, literate programming, org
;; URL: https://gitlab.com/f-santos/ob-julia

;; This file is *not* part of GNU Emacs.

;;; Commentary:
;; This package intends to add an elementary support for Julia language
;; in Org mode.  It is still at a very early stage of development.

;;; Code:
(require 'cl-lib)
(require 'ess)
(require 'ob)

;; How Julia should be called to execute source blocks:
(defcustom org-babel-julia-command "julia"
  "Name of command to use for executing Julia code."
  :group 'org-babel
  :package-version '(ob-julia . "2020-09-25")
  :version "27.1"
  :type 'string)

;; Defaults for Julia session and headers:
(defvar org-babel-default-header-args:julia '())
(defvar org-babel-julia-default-session
  "*julia*"
  "Default name given to a fresh new Julia session.")

;; Header args supported for Julia
;; (see `org-babel-insert-result'):
(defconst org-babel-header-args:julia
  '((results . ((file list scalar table vector verbatim)
		(raw html latex)
		(replace append none prepend silent)
		(output graphics value))))
  "Julia-specific header arguments.")

;; Extension to tangle Julia source code:
(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

;; Create Julia session:
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
	  (julia)                       ; create new Julia comint buffer
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

(provide 'ob-julia)
;;; ob-julia.el ends here
