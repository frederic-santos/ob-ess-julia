;;; ob-julia --- Org babel support for Julia language

;; Copyright (C) 2020
;; Credits:
;; - Primarily based on / forked from G. J. Kerns' ob-julia.
;;   See the original version at https://github.com/gjkerns/ob-julia
;; - Also based on ob-R.el by Eric Schulte and Dan Davison,
;;   for consistency with other ob-* backends.

;; Author: Frédéric Santos
;; Version: 2020-10-08
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
(require 'ess-julia)
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
  :package-version '(ob-julia . "2020-10-08")
  :version "27.1"
  :type 'string)

(defun run-julia-and-select-buffer (&optional start-args)
  "Run Julia with ESS and make sure that its inferior buffer will be active.
START-ARGS is passed to `run-ess-julia'."
  (interactive "P")
  (set-buffer (julia start-args)))

;; End of eval markers for org babel:
(defconst org-babel-julia-eoe-indicator "\"org_babel_julia_eoe\""
  "See help of `org-babel-comint-with-output'.")
(defconst org-babel-julia-eoe-output "org_babel_julia_eoe"
  "See help of `org-babel-comint-with-output'.")

(defvar ob-julia-startup
  (concat (file-name-directory (or load-file-name
                                   (buffer-file-name)))
          "ob-julia-startup.jl")
  "File path for startup Julia script.")

(defvar inferior-julia-args)

;; Defaults for Julia session and headers:
(defvar org-babel-default-header-args:julia '())
(defvar org-babel-julia-default-session "*julia*"
  "Default name given to a fresh new Julia session.")

;; Header args supported for Julia
;; (see `org-babel-insert-result'):
(defconst org-babel-header-args:julia
  '((width   . :any)
    (height  . :any)
    (results . ((file list scalar table vector verbatim)
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
    (let* ((session (or session          ; if user-specified
                        org-babel-julia-default-session))
	   (ess-ask-for-ess-directory
	    (and (and (boundp 'ess-ask-for-ess-directory)
                      ess-ask-for-ess-directory)
		 (not (cdr (assoc :dir params)))))
           (path-to-load-file (format "--load=%s" ob-julia-startup))
           (inferior-julia-args (concat inferior-julia-args path-to-load-file)))
      (if (org-babel-comint-buffer-livep session)
	  session                       ; session already exists
	(save-window-excursion
          (when (get-buffer session)
	    ;; Session buffer exists, but with dead process
	    (set-buffer session))
	  (run-julia-and-select-buffer) ; create new Julia comint buffer
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

;; Retrieve ESS process info:
(defun org-babel-julia-associate-session (session)
  "Associate Julia code buffer with an ESS[Julia] session.
See function `org-src-associate-babel-session'.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (setq ess-local-process-name
	(process-name (get-buffer-process session)))
  (ess-make-buffer-current))

(defvar ess-current-process-name)       ; dynamically scoped
(defvar ess-local-process-name)         ; dynamically scoped
(defvar ess-ask-for-ess-directory)      ; dynamically scoped
(defvar ess-eval-visibly-p)

(defun org-babel-prep-session:julia (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-julia-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:julia params)))
    (org-babel-comint-in-buffer
        session                     ; name of buffer for Julia session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session))
            var-lines))
    session))

(defun org-babel-variable-assignments:julia (params)
  "Parse block PARAMS to return a list of Julia statements assigning the variables in `:var'."
  (let ((vars (org-babel--get-vars params)))
    ;; Create Julia statements to assign each variable specified with `:var':
    (mapcar
     (lambda (pair)
       (org-babel-julia-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assoc :colnames params)))
	(equal "yes" (cdr (assoc :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assoc :colname-names params))))
	       (cdr (nth i (cdr (assoc :rowname-names params)))))))
      (number-sequence 0 (1- (length vars)))))))

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

(defun org-babel-expand-body:julia (body params &optional graphics-file)
  "Expand BODY according to PARAMS, return the expanded body.
I.e., add :prologue and :epilogue to BODY if required, as well as new Julia
variables declared from :var.  The 'expanded body' is actually the union set
of BODY and of all those instructions."
  (let ((width (or (cdr (assq :width params))
                   600))
        (height (or (cdr (assq :height params))
                    400)))
    (mapconcat 'identity
	       (append
	        (when (cdr (assq :prologue params))
		  (list (cdr (assq :prologue params))))
	        ;; TODO: (org-babel-variable-assignments:julia params)
	        (list body)
                (when graphics-file
                  (list (format "plot!(size = (%s, %s))" width height)
                        (format "savefig(\"%s\")" graphics-file)))
	        (when (cdr (assq :epilogue params))
		  (list (cdr (assq :epilogue params)))))
	       "\n")))

(defconst org-babel-julia-write-object-command
  "bodycode = %s
filename = \"%s\"
has_header = %s
try
    CSV.write(filename, bodycode, delim = \"\\t\", writeheader = has_header)
catch err
    if isa(err, ArgumentError) | isa(err, MethodError)
        writedlm(filename, bodycode)
    end
end"
  "A template for Julia to evaluate a block of code and write the result to a file.

Has three %s escapes to be filled in:
1. The code to be run (must be an expression, not a statement)
2. The name of the file to write to
3. Column names, \"true\" or\"false\" (used for DataFrames only)")

(defun org-babel-julia-evaluate-external-process
    (body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in an external Julia process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (if (equal result-type 'output)
      (org-babel-eval org-babel-julia-command body)
    ;; else: result-type != "output"
    (when (equal result-type 'value)
      (let ((tmp-file (org-babel-temp-file "julia-")))
        (org-babel-eval
         (concat org-babel-julia-command
                 " "
                 (format "--load=%s" ob-julia-startup))
         (format org-babel-julia-write-object-command
                 (format "begin\n%s\nend" body)
                 (org-babel-process-file-name tmp-file 'noquote)
                 column-names-p))
        (org-babel-julia-process-value-result
	 (org-babel-result-cond result-params
	   (with-temp-buffer
	     (insert-file-contents tmp-file)
	     (buffer-string))
	   (org-babel-import-elisp-from-file tmp-file "\t"))
	 column-names-p)))))

(defun org-babel-julia-evaluate-session
    (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in a given Julia SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (with-temp-buffer
       ;; Evaluate body in iESS[Julia]:
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "julia-")))
       ;; Then write last returned value in a temp file:
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format org-babel-julia-write-object-command
                "ans"            ; Julia command for last Julia result
		(org-babel-process-file-name tmp-file 'noquote)
                column-names-p))
       (org-babel-julia-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (org-babel-chomp (buffer-string) "\n"))
	  (org-babel-import-elisp-from-file tmp-file "\t"))
	column-names-p)))
    (output
     (mapconcat
      'org-babel-chomp
      (butlast
       (delq nil
             (mapcar
              (lambda (line) (when (> (length line) 0) line))
              (mapcar
               ;; function to cleanup extra prompts left in eval output:
	       (lambda (line)
		 (if (string-match
		      "^\\([>+.]\\([ ][>.+]\\)*[ ]\\)"
		      (car (split-string line "\n")))
		     (substring line (match-end 1))
		   line))
               ;; result of evaluation of block in the iESS Julia buffer:
               (org-babel-comint-with-output
                   (session org-babel-julia-eoe-output)
		 (insert (mapconcat 'org-babel-chomp
				    (list body org-babel-julia-eoe-indicator)
				    "\n"))
		 (inferior-ess-send-input))))))
      "\n"))))

(defun org-babel-execute:julia (body params)
  "Execute a block of Julia code.
The BODY on the block is first refactored with `org-babel-expand-body:julia',
according to user-specified PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((session-name (cdr (assq :session params)))
         (session (org-babel-julia-initiate-session session-name params))
         (graphics-file (org-babel-julia-graphical-output-file params))
         (column-names-p (unless graphics-file (cdr (assq :colnames params))))
	 (row-names-p (unless graphics-file (cdr (assq :rownames params))))
         (expanded-body (org-babel-expand-body:julia body params graphics-file))
         (result-params (cdr (assq :result-params params)))
	 (result-type (cdr (assq :result-type params)))
         (result (org-babel-julia-evaluate
                  session expanded-body result-type result-params
                  (if column-names-p "true" "false")
                  ;; TODO: handle correctly the following last args for rownames
                  nil)))
    ;; Return "textual" results, unless they have been written
    ;; in a graphical output file:
    (unless graphics-file
      result)))

;;;;;;;;;;;;;;;;;;;;;
;; Various helpers ;;
;;;;;;;;;;;;;;;;;;;;;

;; Dirty helpers for what seems to be a bug with iESS[Julia] buffers.
;; See https://github.com/emacs-ess/ESS/issues/1053

(defun ob-julia--split-into-julia-commands ()
  "Split the whole active buffer content into a list of valid Julia commands.
Complete commands are elements of the list; incomplete commands (i.e., commands
that are written on several lines) are `concat'enated, and then passed as one
single element of the list.
This workaround avoids what seems to be a bug with iESS[julia] buffers."
  (let* ((lines (split-string (buffer-substring-no-properties
                               (point-min)
                               (point-max))
                              "\n" t))
         (cleaned-lines (mapcar 'org-babel-chomp lines))
         (last-end-char nil)
         (commands nil))
    (while cleaned-lines
      (if (or (not last-end-char)
              ;; matches an incomplete Julia command:
              (not (s-matches? "[(;,]" last-end-char)))
          (progn
            (setq last-end-char (substring (car cleaned-lines) -1))
            (setq commands (cons (pop cleaned-lines) commands)))
        (setq last-end-char (substring (car cleaned-lines) -1))
        (setcar commands (concat (car commands)
                                 " "
                                 (pop cleaned-lines)))))
    (reverse commands)))

(defun ob-julia--execute-line-by-line (session)
  "Execute cleanly the contents of current buffer into a Julia SESSION.
I.e., clean all Julia instructions, and send them one by one into the
active iESS[julia] process."
  (let ((lines (split-into-julia-commands))
        (jul-proc (get-process (process-name (get-buffer-process session)))))
    (with-current-buffer session
      (mapc
       (lambda (line)
         (insert line)
         (inferior-ess-send-input)
         (ess-wait-for-process jul-proc nil 0.2)
         (goto-char (point-max)))
       lines))))

(defun org-babel-julia-process-value-result (result column-names-p)
  "Julia-specific processing for `:results value' output type.
RESULT should have been computed upstream (and is typiclly retrieved
from a temp file).
Insert hline if column names in output have been requested
with COLUMN-NAMES-P.  Otherwise RESULT is unchanged."
  (if (equal column-names-p "true")
      (cons (car result) (cons 'hline (cdr result)))
    result))

(defun org-babel-julia-graphical-output-file (params)
  "Return the name of the file to which Julia should write graphical output.
This name is extracted from user-specified PARAMS of a code block."
  (and (member "graphics" (cdr (assq :result-params params)))
       (org-babel-graphical-output-file params)))

(defun org-babel-load-session:julia (session body params)
  "Load BODY into a given Julia SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:julia session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

(provide 'ob-julia)
;;; ob-julia.el ends here
