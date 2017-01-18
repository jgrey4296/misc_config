;;using http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs#683575
;;key bindings:

(defvar jg-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Main utility
    (define-key map (kbd "C-c [") 'insert-lparen)
    (define-key map (kbd "C-c ]") 'insert-rparen)
    (define-key map (kbd "C-c r") 'my-select-region-by-line-number)
    (define-key map (kbd "C-c k") 'browse-kill-ring)
    (define-key map (kbd "C-c i") 'indent-to-column)
    ;;Additional utilities:
    (define-key map (kbd "C-c u d") 'insert-author-date-time)
    (define-key map (kbd "C-c u f") 'flycheck-go)
    (define-key map (kbd "C-c u K") (lambda () (interactive) (find-file "~/github/jg_emacs/files/lisp/my-keybindings.el")))
    ;;Window movement
    (define-key map (kbd "C-x C-j") 'windmove-left)
    (define-key map (kbd "C-x C-l") 'windmove-right)
    (define-key map (kbd "C-x C-i") 'windmove-up)
    (define-key map (kbd "C-x C-k") 'windmove-down)
    ;;git
    (define-key map (kbd "C-x g") 'magit-status)
    ;;lisp
    (define-key map (kbd "C-c e b") (lambda () (interactive) (message "Eval'd Buffer") 'eval-buffer))
    (define-key map (kbd "C-c e l") 'my-eval-line)
    (define-key map (kbd "C-c <") 'my-eval-line)
    ;;org
    (define-key map (kbd "C-c o T") 'org-todo-list)
    (define-key map (kbd "C-c o t") 'tag-occurances)
    (define-key map (kbd "C-c o v") 'org-tags-view)
    (define-key map (kbd "C-c o a") 'org-agenda-file-to-front)
    (define-key map (kbd "C-c o l") 'org-agenda-list)
    (define-key map (kbd "C-c o f") 'list-agenda-files)
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
