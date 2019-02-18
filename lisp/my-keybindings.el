(require 'seq)
;;As a reminder:
;; find . -name "some_pattern" -print0 | xargs -0 -J % mv % target
;; convert to jpg with 'convert {}.png {}.jpg'

;;using http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs#683575
;;key bindings:

(defvar jg-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Main utility
    (define-key map (kbd "C-c [") 'insert-lparen)
    (define-key map (kbd "C-c ]") 'insert-rparen)
    (define-key map (kbd "C-c c r") 'comment-region)
    (define-key map (kbd "C-c u c r") 'uncomment-region)
    (define-key map (kbd "C-c r n") 'my-select-region-by-line-number)
    (define-key map (kbd "C-c k") 'browse-kill-ring)
    (define-key map (kbd "C-c i c") 'indent-to-column)
    (define-key map (kbd "C-c i r") 'indent-region)
    (define-key map (kbd "C-c b") 'browse-url-at-point)
    ;;Mode activations
    (define-key map (kbd "C-c m w s") 'whitespace-mode)
    (define-key map (kbd "C-c m h l") 'hl-line-mode)
    (define-key map (kbd "C-c m v l") 'vline-mode)
    (define-key map (kbd "C-c m c") 'crosshairs-mode)
    (define-key map (kbd "C-c m f ") 'fci-mode)
    (define-key map (kbd "C-c m s") 'scroll-lock-mode)
    (define-key map (kbd "C-c m a") 'abbrev-mode)
    (define-key map (kbd "C-c m A") 'auto-complete-mode)
    (define-key map (kbd "C-c m o") 'dired-omit-mode)
    (define-key map (kbd "C-c m l") 'flycheck-mode)
    ;;Additional utilities:
    (define-key map (kbd "C-c c f") 'crosshairs-flash)
    (define-key map (kbd "C-c f c s") 'set-fill-column)
    (define-key map (kbd "C-c i d") 'insert-author-date-time)
    (define-key map (kbd "C-c u f") 'flycheck-go)
    (define-key map (kbd "C-c u K") (lambda () (interactive) (find-file (expand-file-name "~/github/jg_emacs_files/lisp/my-keybindings.el"))))
    (define-key map (kbd "C-c c u r") 'upcase-region)
    (define-key map (kbd "C-c c d r") 'downcase-region)
    (define-key map (kbd "C-c r s") 'replace-string)
    (define-key map (kbd "C-c r r") 'replace-regexp)
    (define-key map (kbd "C-c d t l") 'delete-trailing-whitespace)
    ;;Window movement
    (define-key map (kbd "C-c C-j") 'windmove-left)
    (define-key map (kbd "C-c C-l") 'windmove-right)
    (define-key map (kbd "C-c C-i") 'windmove-up)
    (define-key map (kbd "C-c C-k") 'windmove-down)
    ;;Registers:
    (define-key map (kbd "C-c r v") 'view-register)
    (define-key map (kbd "C-c r l") 'list-registers)
    (define-key map (kbd "C-c r w") 'window-configuration-to-register)
    (define-key map (kbd "C-c r x") 'copy-to-register)
    (define-key map (kbd "C-c r j") 'jump-to-register)
    (define-key map (kbd "C-c r f") 'frameset-to-register)
    (define-key map (kbd "C-c r i") 'insert-register)
    ;;git
    (define-key map (kbd "C-c g") 'magit-status)
    ;;lisp
    ;;remember: use (message x) not (print x)
    (define-key map (kbd "C-c e b") 'eval-buffer)
    (define-key map (kbd "C-c e e") 'eval-expression)
    (define-key map (kbd "C-c e l") 'my-eval-line)
    (define-key map (kbd "C-c <") 'my-eval-line)
    (define-key map (kbd "C-c e r") 'eval-region)
    ;;org
    (define-key map (kbd "C-c o T") 'org-todo-list)
    (define-key map (kbd "C-c o t") 'tag-occurances)
    (define-key map (kbd "C-c o v") 'org-tags-view)
    (define-key map (kbd "C-c o a a") 'org-agenda-file-to-front)
    (define-key map (kbd "C-c o a r") 'org-remove-file)
    (define-key map (kbd "C-c o a f l") 'list-agenda-files)
    (define-key map (kbd "C-c o a l") 'org-agenda-list)
    (define-key map (kbd "C-c o a w") 'org-agenda-week-view)
    (define-key map (kbd "C-c o a m") 'org-agenda-month-view)
    (define-key map (kbd "C-c o a c") 'org-goto-calendar)
    (define-key map (kbd "C-c o d") 'org-date-from-calendar)
    (define-key map (kbd "C-c o D") 'org-time-stamp)
    (define-key map (kbd "C-c o s c") 'org-edit-src-code)
    (define-key map (kbd "C-c o l s") 'org-store-link)
    (define-key map (kbd "C-c o l i") 'org-insert-link)
    (define-key map (kbd "C-c o l d") 'org-toggle-link-display)
    ;;"C-c C-d" :: org-deadline
    ;;"C-c . "  :: org-timestamp
    ;;"C-c ! "  :: org inactive timestamp
    ;;"C-c C-s  :: org-schedule
    ;;abbreviations
    (define-key map (kbd "C-c a l") 'edit-abbrevs)
    (define-key map (kbd "C-c a w") 'write-abbrev-file)
    (define-key map (kbd "C-c a r") 'read-abbrev-file)
    (define-key map (kbd "C-c a a") 'add-global-abbrev)
    (define-key map (kbd "C-c a A") 'add-move-abbrev)
    (define-key map (kbd "C-c a k") 'kill-all-abbrevs)
    ;;Override
    (define-key map (kbd "C-x C-j") 'windmove-left)
    (define-key map (kbd "C-x C-l") 'windmove-right)
    (define-key map (kbd "C-x C-i") 'windmove-up)
    (define-key map (kbd "C-x C-k") 'windmove-down)

    ;;Summary
    ;;(define-key map (kbd "C-c C-j") 'imenu)
    ;;Useful:
    ;;"C-h l" :: Show last keystrokes
    ;;"C-x C-h" :: Describe prefix bindings
    ;; toggle-truncate-lines   to turn off line wrapping
    ;;simple proof that multi key sequences of keybindings DO work
    (define-key map (kbd "C-c p a b c") 'insert-author-date-time)
    map)
  "jg-keys-minor-mode keymap")

(define-minor-mode jg-keys-minor-mode
  "A custom minor mode for my preferred keybindings"
  :init-value t
  :lighter " jg-keys")
(jg-keys-minor-mode 1)

;; from https://stackoverflow.com/questions/14066526/
(setq yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "C-c ;") 'yas-expand)
        (define-key map (kbd "C-c >") 'yas-insert-snippet)
        (define-key map (kbd "C-c y n") 'yas-new-snippet)
        (define-key map (kbd "C-c y d") 'yas-describe-tables)
        map
        )
      )




(defvar sclang-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l") `sclang-eval-line)
    (define-key map (kbd "C-c R") `sclang-eval-region)
    map)
  "sclang-keys-minor-mode keymap")

(define-minor-mode sclang-keys-minor-mode
  "A simple set of additions to the sclang keybindings"
  :init-value t
  :lighter " sclang-keys")


;; ;custom keybindings for dvorak
;; ;(global-set-key (kbd "C-;") ctl-x-map)
;; ;(global-set-key (kbd "C-x C-h") help-map)
;; ;(global-set-key (kbd "C-h") 'previous-line)
;; ;(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)

                                        ;org mode bindings:
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; ;(global-set-key (kbd "C-c b") 'org-iswitchb)

;; ;flycheck
;; (global-set-key (kbd "C-c f") 'flycheck-go)


;; Elisp notes:
;; use def* to get clisp functionality, such as &key's
;; use message, instead of print, in scratch
;; use (message (format "%S" (macroexpand x)))
