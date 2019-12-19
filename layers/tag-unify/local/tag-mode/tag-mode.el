;;based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'dash)

(defgroup tag-mode '() "Group for Tag Mode Variables")
(defvar tag-mode/default-tag-split-dir "tag_splits")

(defgroup tag-mode '() "Tag Mode Customizations")
;;--------------------
;; Mode Variables
;;--------------------

;;--------------------
;;Utilities
;;--------------------
(defun tag-mode/align-tags ()
  (interactive)
  (goto-char (point-min))
  (let* ((ws-regexp "\\(\\s-*\\)")
         (complete-regexp (concat ws-regexp ":"))
         (group 1))
    (align-regexp (point-min) (point-max)
                  complete-regexp
                  group 1 t))
  )
(defun tag-mode/split-alphabetically ()
  (interactive)
  ;;sort alphabetically
  (sort-lines nil (point-min) (point-max))
  ;;split into subheadings
  (goto-char (point-min))
  (insert "* Tags\n")
  (insert "** @\n")
  (let ((letter ?a)
        (end-letter (+ 1 ?z))
        subs)
    (while (and (not (equal letter end-letter))
                (re-search-forward (format "^%s" (char-to-string letter)) nil nil))
      (goto-char (line-beginning-position))
      (insert (format "** %s Tags:\n" (char-to-string letter)))
      (incf letter)
      )
    )
  )
(defun tag-mode/split-into-subfiles ()
  (interactive)
  ;;split-alphabetically
  (tag-mode/split-alphabetically)
  ;;take subheadings into separate files
  (if (f-exists? (f-join default-directory tag-mode/default-tag-split-dir))
      (delete-directory (f-join default-directory tag-mode/default-tag-split-dir) t t))

  (mkdir (f-join default-directory tag-mode/default-tag-split-dir))

  (org-map-entries 'tag-mode/heading-map-f)
  )
(defun tag-mode/heading-map-f ()
  (if (eq 2 (plist-get (cadr (org-element-context)) :level))
      ;;take the entry, put into new file
      (let* ((content (org-get-entry))
             (heading (org-get-heading))
             (file_name (format "%s.org"
                                (replace-regexp-in-string "[ :]" "_" heading)))
             )
        (with-temp-buffer
          (insert (format "* %s\n" heading))
          (insert content)
          (write-file (f-join default-directory
                              tag-mode/default-tag-split-dir
                              file_name))
          )
        )
    )
  )
(defun tag-mode/reverse-format ()
  (interactive)
  (with-output-to-temp-buffer "reversed_tags"
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ ]+\\) : \\([^ ]+\\)$" nil t)
      (princ (format "%s : %s\n" (match-string 2) (match-string 1)))
      )
    )
  )

;; (org-babel-edit-distance S1 S2)
;;--------------------
;;definitions
;;--------------------

;;--------------------
;;face definitions
;; use them as symbols 'blah in font-lock-keywords
;;--------------------

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar tag-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  "Keymap for Tag mode major mode")

;;--------------------
;;Keyword based syntax highlighting
;;Specify as a list of (matcher . facename )
;;Potential keywords: operators +-*/!.()""$~ < > != == -> @
;;--------------------
(defconst tag-font-lock-keywords
  (list
   ;;Rule name
   `("^\\([ a-zA-Z_0-9]+\\): \\([[:digit:]]+\\)$"
     (1 "font-lock-string-face")
     (2 "font-lock-type-face"))
   )
  "Minimal highlighting expressions for tag mode")


;;--------------------
;;Syntax Table
;;--------------------
;;Define the syntax table, for word definitions etc
;;modify-syntax-entry: character, class/flag, syntable)
;;classes/syntax flags: include 'w': word, '.':punctuation,
;; see: C-h ? r elisp manual syntax-tables
(defvar tag-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    st)
  "Syntax table for the tag-mode")

;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.tags\\'" . tag-mode))


;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode tag-mode org-mode "Tag Mode"
  "Major Mode for creating rules using tags"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tag-mode-map)
  (set (make-local-variable 'font-lock-defaults) (list (reverse tag-font-lock-keywords) nil))

  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table tag-mode-syntax-table)
  (setq major-mode 'tag-mode)
  (setq mode-name "TAG")
  (run-mode-hooks)
  (outline-minor-mode)
  )

;;todo later: set no longer needed variables to nil

(provide 'tag-mode)
