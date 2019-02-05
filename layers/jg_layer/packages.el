;; jg_layer packages.el
;; loads second

(defconst jg_layer-packages
  '(
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;;(some-package :excluded t)
    helm
    org
    yasnippet
    abbrev
    evil
    (smartparens :excluded t)
    ibuffer
    fci
    rainbow-mode
    (shell :lcoation builtin)
    (git-gutter :excluded t)
    (git-gutter+ :excluded t)
    )
  )

;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun jg_layer/post-init-evil ()
  (setq-default evil-escape-delay 0.3 )
  (global-set-key (kbd "<backtab>") 'evil-normal-state)

  ;; Set up faces for hl-line colour sync to status
  (defface evil-normal-state '((t :background  "#000000")) "The Evil Normal State Hl-line")
  (defface evil-insert-state '((t :background  "#005f00")) "The Evil Insert State Hl-line")
  (defface evil-visual-state '((t :background  "#005fff")) "The Evil Visual State Hl-line")
  (defface evil-motion-state '((t :background  "#5f0000")) "The Evil Motion State Hl-line")
  (defface evil-emacs-state '((t :background  "#5f00ff"))  "The Evil Emacs State Hl-line")
  (defface evil-replace-state '((t :background  "#8700ff")) "The Evil Replace State Hl-line")
  (defface evil-hybrid-state '((t :background  "#0087ff")) "The Evil Hybrid State Hl-line")
  (defface evil-evilified-state '((t :background  "#5f5f00")) "The Evil Evilified State Hl-line")
  (defface evil-lisp-state '((t :background  "#875fff")) "The Evil Lisp State Hl-line")
  (defface evil-iedit-state '((t :background  "#8700af")) "The Evil iedit State Hl-line")
  (defface evil-iedit-insert-state '((t :background  "#8700af")) "The Iedit Insert state Hl-line")

  ;; hooks for evil state entry hooks to change hl-line colour
  (add-hook 'evil-normal-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-normal-state))))
  (add-hook 'evil-insert-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-insert-state))))
  (add-hook 'evil-visual-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-visual-state))))
  (add-hook 'evil-motion-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-motion-state))))
  (add-hook 'evil-emacs-state-entry-hook    (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-emacs-state))))
  (add-hook 'evil-replace-state-entry-hook  (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-replace-state))))
  (add-hook 'evil-hybrid-state-entry-hook   (lambda () (interactive) (if (overlayp global-hl-line-overlay)     (overlay-put global-hl-line-overlay 'face 'evil-hybrid-state))))
  (add-hook 'evil-evilified-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-evilified-state))))
  (add-hook 'evil-lisp-state-entry-hook      (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-lisp-state))))
  (add-hook 'evil-iedit-state-entry-hook     (lambda () (interactive) (if (overlayp global-hl-line-overlay)    (overlay-put global-hl-line-overlay 'face 'evil-iedit-state))))
  (add-hook 'evil-iedit-insert-state-entry-hook (lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'evil-iedit-insert-state))))


  (defvar evil-ex-commands nil
    " Custom Evil-ex commands")

  ;; definition of said commands, adapted from evil-maps

  (evil-ex-define-cmd "cl" 'spacemacs/comment-or-uncomment-lines)
  (evil-ex-define-cmd "t[ag]" 'org-set-tags-command)
  (evil-ex-define-cmd "to" 'jg_layer/tag-occurances)
  (evil-ex-define-cmd "mw" 'spacemacs/window-manipulation-transient-state/body)
  (evil-ex-define-cmd "mb" 'spacemacs/buffer-transient-state/body)
  (evil-ex-define-cmd "os" 'org-store-link)
  (evil-ex-define-cmd "oi" 'org-insert-last-stored-link)
  (evil-ex-define-cmd "oo" 'org-open-at-point)

  ;; TODO: registers
  ;; TODO: yasnippet

  ;; file / buffer loading
  (evil-ex-define-cmd "e[dit]" 'evil-edit)
  (evil-ex-define-cmd "w[rite]" 'evil-write)
  (evil-ex-define-cmd "wa[ll]" 'evil-write-all)
  (evil-ex-define-cmd "sav[eas]" 'evil-save)
  (evil-ex-define-cmd "r[ead]" 'evil-read)
  (evil-ex-define-cmd "b[uffer]" 'evil-buffer)

  (evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
  (evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
  (evil-ex-define-cmd "bN[ext]" "bprevious")
  (evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
  (evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
  (evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
  (evil-ex-define-cmd "sbN[ext]" "sbprevious")

  ;; buffer and file listings
  (evil-ex-define-cmd "buffers" 'buffer-menu)
  (evil-ex-define-cmd "files" 'evil-show-files)
  (evil-ex-define-cmd "ls" "buffers")

  ;; deleting / yanking
  ;; (evil-ex-define-cmd "c[hange]" 'evil-change)
  (evil-ex-define-cmd "c[opy]" 'evil-copy)
  (evil-ex-define-cmd "m[ove]" 'evil-move)
  (evil-ex-define-cmd "d[elete]" 'evil-ex-delete)
  (evil-ex-define-cmd "y[ank]" 'evil-ex-yank)
  ;;goto top of buffer
  (evil-ex-define-cmd "top" 'evil-goto-char)
  ;; bring a line up
  (evil-ex-define-cmd "j[oin]" 'evil-ex-join)
  ;; alignment
  (evil-ex-define-cmd "le[ft]" 'evil-align-left)
  (evil-ex-define-cmd "ri[ght]" 'evil-align-right)
  (evil-ex-define-cmd "ce[nter]" 'evil-align-center)
  ;; windows/buffer creation
  (evil-ex-define-cmd "sp[lit]" 'evil-window-split)
  (evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
  (evil-ex-define-cmd "new" 'evil-window-new)
  (evil-ex-define-cmd "ene[w]" 'evil-buffer-new)
  (evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
  ;; window deletion
  (evil-ex-define-cmd "clo[se]" 'evil-window-delete)
  (evil-ex-define-cmd "on[ly]" 'delete-other-windows)
  ;; quitting emacs
  (evil-ex-define-cmd "q[uit]" 'evil-quit)
  (evil-ex-define-cmd "wq" 'evil-save-and-close)
  (evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
  (evil-ex-define-cmd "qa[ll]" "quitall")
  (evil-ex-define-cmd "cq[uit]" 'evil-quit-all-with-error-code)
  (evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
  (evil-ex-define-cmd "xa[ll]" "wqall")
  (evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
  (evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
  (evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
  (evil-ex-define-cmd "bw[ipeout]" 'evil-delete-buffer)
  ;; state change
  (evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
  (evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
  (evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
  ;; substitution
  (evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
  (evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
  (evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
  (evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
  (evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
  ;; registers and marks
  (evil-ex-define-cmd "registers" 'evil-show-registers)
  (evil-ex-define-cmd "marks" 'evil-show-marks)
  (evil-ex-define-cmd "delm[arks]" 'evil-delete-marks)
  (evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
  (evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
  (evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
  ;; shifting / aligning
  (evil-ex-define-cmd "<" 'evil-shift-left)
  (evil-ex-define-cmd ">" 'evil-shift-right)
  ;; print last line number
  (evil-ex-define-cmd "=" 'evil-ex-line-number)
  (evil-ex-define-cmd "!" 'evil-shell-command)
  (evil-ex-define-cmd "@:" 'evil-ex-repeat)
  (evil-ex-define-cmd "mak[e]" 'evil-make)
  ;; errors
  (evil-ex-define-cmd "cc" 'evil-goto-error)
  (evil-ex-define-cmd "cfir[st]" 'first-error)
  (evil-ex-define-cmd "cr[ewind]" 'first-error)
  (evil-ex-define-cmd "cn[ext]" 'next-error)
  (evil-ex-define-cmd "cp[revious]" 'previous-error)
  (evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
  (evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
  ;;sorting
  (evil-ex-define-cmd "sor[t]" 'evil-ex-sort)
  ;; window resizing
  (evil-ex-define-cmd "res[ize]" 'evil-ex-resize)
  )

(defun jg_layer/post-init-helm ()
  ;;add in keybinding to kill line in completion window
  (with-eval-after-load 'helm
    (helm-autoresize-mode 0)
    (define-key helm-map (kbd "C-K") 'kill-line)
    )
  )

(defun jg_layer/post-init-org ()
  ;;ORG SETUP
  (setq-default
   org-agenda-files `(,(expand-file-name "~/.spacemacs.d/setup_files/base_agenda.org"))
   org-fast-tag-selection-single-key nil
   org-from-is-user-regexp "\\<John Grey\\>"
   org-group-tags nil
   org-use-fast-tag-selection t
   org-tags-column -80
   )

  ;; add in keybinding to call tag-occurances
  (spacemacs/declare-prefix "o" "Org")
  (spacemacs/declare-prefix "o t" "Tags")
  (spacemacs/declare-prefix "o a" "Agenda")
  (spacemacs/declare-prefix "o c" "Calendar")
  (spacemacs/declare-prefix "o s" "Source")
  (spacemacs/declare-prefix "o l" "Links")
  (spacemacs/set-leader-keys
    "o T"     'org-todo-list
    ;; TAGS
    "o t o"   'jg_layer/tag-occurances
    "o t v"   'org-tags-view
    "o t s"   'org-set-tags
    ;; AGENDA
    "o a a"   'org-agenda-file-to-front
    "o a r"   'org-remove-file
    "o a l"   'org-agenda-list
    "o a w"   'org-agenda-week-view
    "o a m"   'org-agenda-month-view
    "o a f"   'jg_layer/list-agenda-files
    "o a d"   'org-deadline
    "o a s"   'org-schedule
    ;; CALENDAR
    "o c c"   'org-goto-calendar
    "o c d"   'org-date-from-calendar
    "o c t"   'org-time-stamp
    "o c i"   'org-inactive-timestamp
    ;; SRC CODE
    "o s c"   'org-edit-src-code
    ;; LINKS
    "o l s"   'org-store-link
    "o l i"   'org-insert-link
    "o l d"   'org-toggle-link-display
    "o l o"   'jg_layer/open_link_in_buffer
    )
  )

(defun jg_layer/post-init-yasnippet ()
  ;;yasnippet
  (setq-default yas-snippet-dirs `( ,(expand-file-name "~/.spacemacs.d/snippets/")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")))
  (spacemacs/declare-prefix "y" "Snippets/Abbrevs")
  (spacemacs/set-leader-keys
    "y y"    'yas-expand
    "y i"    'yas-insert-snippet
    "y n"    'yas-new-snippet
    "y d"    'yas-describe-tables
    )
  (global-set-key (kbd "C-c ;") 'expand-abbrev)
  (global-set-key (kbd "C-c >") 'yas-new-snippet)
  )

(defun jg_layer/post-init-abbrev ()
  ;;abbrev-file complaint quieting
  (setq-default
   abbrev-file-name (expand-file-name "~/.spacemacs.d/layers/jg_layer/abbrevs_defs")
   )
  (spacemacs/set-leader-keys
    "y e"  'edit-abbrevs
    "y w"  'write-abbrev-file
    "y r"  'read-abbrev-file
    "y a"  'add-global-abbrev
    "y A"  'add-mode-abbrev
    "y k"  'kill-all-abbrevs
    )
  )


(defun jg_layer/post-init-ibuffer ()
  )

(defun jg_layer/post-init-erlang ()
  ;; (also has a load path set in root el file)
  erlang-root-dir "/usr/local/opt/erlang"
  exec-path (cons "/usr/local/opt/erlang/bin" exec-path)

  )

(defun jg_layer/post-init-python ()
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil )
  )

(defun jg_layer/post-init-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg_layer/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    :config (progn
              (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
              (add-hook 'prog-mode-hook 'rainbow-mode))
    )
  )

(defun jg_layer/init-nlinum ()
  (use-package nlium
    :commands (nlinum-mode)
    )
  )

(defun jg_layer/post-init-shell ()
  ;; shell default can be: shell, eshell, term, ansi-term
  (setq-default shell-default-shell 'shell
                shell-protect-eshell-prompt 0
                shell-enable-smart-eshell t
                )
  (remove-hook 'term-mode-hook 'spacemacs/disable-hl-line-mode)
  (remove-hook 'comint-mode-hook 'spacemacs/disable-hl-line-mode)
)

