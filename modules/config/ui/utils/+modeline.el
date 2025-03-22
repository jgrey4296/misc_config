;;; +modeline.el -*- lexical-binding: t; -*-

;;;; Notes:
;; defined segments are in doom-modeline-fn-alist
;; format-mode-line
;; - doom-modeline-def-modeline quoted-name
;; - doom-modeline-def-env name
;; - doom-modeline-def-segment name
;; global-mode-string
;; mode-line-format
;; header-line-format
;; doom-modeline-mode-alist
;; - Set default mode-line
;; (add-hook 'doom-modeline-mode-hook
;;         (lambda ()
;;                 (doom-modeline-set-modeline 'my-simple-line 'default)))
;; - Configure other mode-lines based on major modes
;; (add-to-list 'doom-modeline-mode-alist '(my-mode . my-simple-line))
;; - Or disable other mode-lines
;; (setq 'doom-modeline-mode-alist nil)

(defvar jg-modeline-test-var "blah")
(doom-modeline-def-segment vbar "a vertical bar" " | ")
(doom-modeline-def-segment test-segment
  "Testing a segment that relies on a variable"
  (format "(%s)" jg-modeline-test-var)
  )

(doom-modeline-def-segment jg-repl
  "For notifying about current repl "
  (when-let ((repl-name (+jg-eval-repl-process-name)))
    (format " [REPL:%s]" repl-name)
    )
  )

(doom-modeline-def-segment jg-dired-marked
  "Notifies how many files are marked in the buffer "
  (when jg-dired-marked-count
    (propertize (format " Marked: %d" jg-dired-marked-count) 'face (doom-modeline-face))
    )
  )

(doom-modeline-def-segment buffer-count
  "Notifies Total Number of buffers"
  (propertize (format " (TB:%d/%d)" (length (doom-real-buffer-list)) (length (buffer-list))))
  )

(doom-modeline-def-modeline 'main-alt
  '(eldoc
    debug
    modals
    buffer-count
    buffer-info-simple ;; mode,state,name
    ;; buffer-info ;; mode,state,name
    buffer-position ;; line:col percentage
    selection-info ;; visual mode selection
    matches ;; search-matches
    word-count ;; for things in doom-modeline-continuous-word-count-modes
    )
  '(
    compilation
    ;; indent-info
    ;; buffer-encoding
    major-mode
    process
    vbar
    jg-repl
    project-name
    vcs ;; current branch
    github
    vbar
    misc-info ;; global-mode-string contents
    )
  )

(doom-modeline-def-modeline 'dired
  '(eldoc
    debug
    modals
    buffer-info ;; mode,state,name
    jg-dired-marked
    )
  '(
    compilation
    ;; indent-info
    ;; buffer-encoding
    ;; major-mode
    process
    vbar
    jg-repl
    project-name
    vcs ;; current branch
    github
    vbar
    misc-info ;; global-mode-string contents
    )
  )

(doom-modeline-def-modeline 'minor-modes
  '(minor-modes)
  '()
  )

(defun +jg-ui-reset-modeline-default ()
  (doom-modeline-set-modeline 'main-alt t)
  )
