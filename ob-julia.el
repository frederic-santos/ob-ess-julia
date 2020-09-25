;;; ob-julia --- Org babel support for Julia language

;; Copyright (C) 2020 Frédéric Santos

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
(defvar org-babel-julia-default-session "*julia*")

;; Extension to tangle Julia source code:
(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

(provide 'ob-julia)
;;; ob-julia.el ends here
