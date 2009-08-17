;;; TODO: Proper acknowledgement to Pfenning here!
(defvar twelf2-mode-hook nil)

(add-to-list 'auto-mode-alist
	     '("\\.elf2\\'" . twelf2-mode))

(defvar twelf2-mode-map
  (let ((twelf2-mode-map (make-keymap)))
    (define-key twelf2-mode-map "\C-j" 'newline-and-indent)
    twelf2-mode-map)
    "Keymap for the Twelf2 major mode")

(defvar twelf-declarations
  '("%infix" "%prefix" "%postfix" "%name" "%freeze" "%thaw"
		"%abbrev" "%clause" "%define" "%solve" "%querytabled"
		"%query" "%tabled" "%deterministic" "%mode" "%unique"
		"%block" "%worlds" "%covers" "%total" "%terminates"
		"%reduces" "%theorem" "%prove" "%assert" "%establish"
		"%sig" "%struct" "%trustme" "%where" "%include" "%open"
		"%use"))
(defvar twelf-declarations-regexp (regexp-opt twelf-declarations))

(defvar twelf-keywords
  '("<-" "->" "=" "_")) ; Decide if this should declare 'type'
(defvar twelf-keywords-regexp (regexp-opt twelf-keywords))

(defvar twelf-types '("type"))
(defvar twelf-types-regexp (regexp-opt twelf-types))

(defconst twelf2-font-lock-keywords
  `(
    (,twelf-declarations-regexp . font-lock-preprocessor-face)
    (,twelf-keywords-regexp . font-lock-keyword-face)
    (,twelf-types-regexp . font-lock-type-face))
  "Minimal highlighting for Twelf")

(defvar twelf2-mode-syntax-table
  (let ((twelf2-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" twelf2-mode-syntax-table)
    (modify-syntax-entry ?' "w" twelf2-mode-syntax-table)
    ;; Encode Twelf-style comments into the syntax table
    (modify-syntax-entry ?% "w 124b" twelf2-mode-syntax-table)
    (modify-syntax-entry ?{ "( 2"    twelf2-mode-syntax-table)
    (modify-syntax-entry ?} ") 3"    twelf2-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"   twelf2-mode-syntax-table)
    twelf2-mode-syntax-table)
  "Syntax table for twelf2-mode")


;;; Entry function
(defun twelf2-mode ()
  "Major mode for editing Twelf signatures"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table twelf2-mode-syntax-table)
  (use-local-map twelf2-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(twelf2-font-lock-keywords))
  ;(set (make-local-variable 'indent-line-function)
           ; 'twelf2-indent-line)
  (setq major-mode 'twelf2-mode)
  (setq mode-name "Twelf2")
  (run-hooks 'twelf2-mode-hook))

(provide 'twelf2)

;;;;;;;;;;;;;;;;;;; DUNNO GROUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar twelf-font-patterns
;;  '(
;;    ;; single-line comments
;;    ("%[% \t\f].*$" 0 twelf-font-comment-face)

;;    ;; keywords, omit punctuations for now.
;;    . twelf-font-keyword-face)
;;    ;; declared constants
;;    (twelf-font-find-decl . twelf-font-decl-face)
;;    ;; parameters
;;    (twelf-font-find-parm . twelf-font-parm-face)
;;    ;; quantified existentials
;;    (twelf-font-find-evar . twelf-font-evar-face)
;;    ;; lower-case identifiers (almost = constants)
;;    ;;("\\<\\([a-z!&$^+/<=>?@~|#*`;,]\\|\\-\\|\\\\\\)\\w*\\>"
;;    ;; nil black)
;;    ;; qualified identifiers
;;    ("\\<\\w+\\(\\.\\w+\\)+\\>" . twelf-font-const-face)
;;    ;; upper-case identifiers (almost = variables)
;;    ("\\<[A-Z_]\\w*\\>" . twelf-font-fvar-face)
;;    ;; numbers and quoted identifiers omitted for now
;;    )
;;  "Highlighting patterns for Twelf mode.
;; This generally follows the syntax of the FONT-LOCK-KEYWORDS variable,
;; but allows an arbitrary function to be called instead of just
;; regular expressions."
;;  )

;; (defun twelf-font-highlight (start end face allow-overlap-p)
;;   "Highlight region between START and END with FONT.
;; If already highlighted and ALLOW-OVERLAP-P is nil, don't highlight."
;;   (or (= start end)
;;       ;;(if allow-overlap-p nil (font-lock-any-faces-p start (1- end)))
;;       ;; different in XEmacs 19.16?  font-lock-any-faces-p subtracts 1.
;;       (if allow-overlap-p nil (font-lock-any-faces-p start end))
;;       (font-lock-set-face start end face)))

;; ;; doesn't work yet with LIMIT!!!
;; ;; this should never be done in incremental-highlighting mode
;; (defun twelf-font-find-decl (limit)
;;   "Find an Twelf constant declaration and return (START . END), nil if none."
;;   (let (start
;; 	end
;; 	;; Turn off error messages
;; 	(id (twelf-font-next-decl nil nil)))
;;     ;; ignore limit for now because of global buffer restriction
;;     (if (null id) ; (or (null id) (> (point) limit))
;; 	nil
;;       (skip-chars-backward *whitespace*)
;;       (setq end (point))
;;       ;;(beginning-of-line 1)
;;       (backward-char (length id))
;;       (setq start (point))
;;       (twelf-font-end-of-par)
;;       (cons start end))))

;; (defun twelf-font-find-binder (var-pattern limit occ-face)
;;   "Find Twelf binder whose bound variable matches var-pattern.
;; Returns (START . END) if found, NIL if there is none before LIMIT.
;; Binders have the form [x],[x:A],{y},{y:A}.
;; As a side-effect, it highlights all occurrences of the bound
;; variable using the variable OCC-FACE."
;;   (let (start
;; 	end
;; 	par-end
;; 	scope-start
;; 	scope-end
;; 	word
;; 	(found nil))
;;     ;;; At the moment, ignore limit since restriction is done globally
;;     ;; (save-restriction
;;     ;; (narrow-to-region (point) limit)
;;       (while (not found)
;; 	(skip-chars-forward "^[{%")
;; 	(while (looking-at *twelf-comment-start*)
;; 	  (cond ((looking-at "%{")
;; 		 (condition-case nil (forward-sexp 1)
;; 		   (error (goto-char (point-max))))
;; 		 (or (eobp) (forward-char 1)))
;; 		(t
;; 		 (end-of-line 1)))
;; 	  (skip-chars-forward "^[{%"))
;; 	(if (eobp)
;; 	    (setq found 'eob)
;; 	  (forward-char 1)
;; 	  ;; disable so that module expressions are more likely
;; 	  ;; to be highlighted correctly.   Thu May 24 2001 -fp
;; 	  ;;(skip-chars-forward *whitespace*)
;; 	  (if (looking-at var-pattern)
;; 	      ;;"\\<\\w+\\>"
;; 	      ;;"\\<[-a-z!&$^+/\\<=>?@~|#*`;,]\\w*\\>"
;; 	      (setq found t))))
;;       (if (eq found 'eob)
;; 	  nil
;; 	(setq start (match-beginning 0))
;; 	(setq end (match-end 0))
;; 	(setq word (buffer-substring start end))
;; 	;; find scope of quantifier
;; 	(twelf-end-of-par)
;; 	(setq par-end (point))
;; 	(goto-char end)
;; 	(condition-case nil (up-list 1)	; end of quantifier
;; 	  (error (goto-char par-end)))
;; 	(setq scope-start (min (point) par-end))
;; 	(condition-case nil (up-list 1)	; end of scope
;; 	  (error (goto-char par-end)))
;; 	(setq scope-end (min (point) par-end))
;; 	(goto-char scope-start)
;; 	(while
;; 	    ;; speed here???
;; 	    (search-forward-regexp (concat "\\<" (regexp-quote word) "\\>")
;; 				   scope-end 'limit)
;; 	  ;; Check overlap here!!! --- current bug if in comment
;; 	  (if (font-lock-any-faces-p (match-beginning 0) (match-end 0))
;; 	      ;; no overlap --- ignore comments which are fontified already
;; 	      nil
;; 	    (font-lock-set-face (match-beginning 0) (match-end 0)
;; 				occ-face)))
;; 	(goto-char end)
;; 	(cons start end)))
;;   ;;)
;;   )

;; (defun twelf-font-find-parm (limit)
;;   "Find bound Twelf parameters and return (START . END), NIL if none.
;; Also highlights all occurrences of the parameter.
;; For these purposes, a parameter is a bound, lower-case identifier."
;;   (twelf-font-find-binder "\\<[-a-z!&$^+/\\<=>?@~|#*`;,]\\w*\\>"
;; 			limit 'twelf-font-parm-face))

;; (defun twelf-font-find-evar (limit)
;;   "Find bound Twelf existential variable return (START . END), NIL if none.
;; Also highlights all occurrences of the existential variable.
;; For these purposes, an existential variable is a bound, upper-case identifier."
;;   (twelf-font-find-binder "\\<[A-Z_]\\w*\\>"
;; 			limit 'twelf-font-evar-face))

