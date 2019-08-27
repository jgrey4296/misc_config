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
    yasnippet
    abbrev
    evil
    ;; ibuffer
    fci
    rainbow-mode
    flycheck
    shell
    python
    academic-phrases
    dired-quick-sort
    dired
    shell-pop
    org
    org-ref
    org-pomodoro
    (org-drill :location built-in)
    ;; buffer-manage
    ;; (buffer-sets :location (recipe :fetcher git :url "https://git.flintfam.org/swf-projects/buffer-sets.git"))
    buffer-utils
    ;; (filesets+ :location (recipe :fetcher github :repo "emacsmirror/filesets-plus"))
    ;; helm-filesets
    evil-string-inflection
    evil-quickscope
    free-keys
    fsm
    highlight-parentheses
    origami
    vlf
    plantuml-mode
    flycheck-plantuml
    ;; ggtags
    ;; (helm-gtags :toggle (configuration-layer/package-usedp 'helm))
    ;; xcscope
    ;; (helm-cscope :toggle (configuration-layer/package-usedp 'helm))

    (git-gutter :excluded t)
    (git-gutter+ :excluded t)
    (smartparens :excluded t)
    (show-paren-mode :excluded t)
    (erc :excluded t)
    (jabber :excluded t)
    (rcirc :excluded t)
    (slack :excluded t)
    (default-ivy-config :excluded t)
    (ido :excluded t)
    (ido-vertical-mode :excluded t)
    (eyebrowse :excluded t)
    (ivy :excluded t)
    (persp-mode :excluded t)
    (swiper :excluded t)
    (ace-link :excluded t)
    (desktop :excluded t)
    (doc-view :excluded t)
    (flx-ido :excluded t)
    (open-junk-file :excluded t)
    (paradox :excluded t)
    (fancy-battery :excluded t)
    (golden-ratio :excluded t)
    (zoom-frm :excluded t)
    (company :excluded t)
    (projectile :excluded t)
    (smex :excluded t)
    (2048-game :excluded t)
    (tetris :excluded t)
    (sudoku :excluded t)
    (helm-games :excluded t)
    (typit :excluded t)
    (selectric-mode :excluded t)
    (xkcd :excluded t)
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

(defun jg_layer/pre-init-helm ()
  (defun jg_layer/helm-open-random-external-action (candidate)
    " Open a random file in an external program, optionally specifying wildcard "
    (interactive)
    (let* ((pattern (car (last (f-split candidate))))
           (pattern-r (wildcard-to-regexp pattern))
           (files (helm-get-candidates (helm-get-current-source)))
           (all_matches (if (string-match-p "\*\." pattern)
                            (seq-filter (lambda (x) (string-match-p pattern-r x)) files)
                          (f-files candidate)))
           (selected (seq-random-elt all_matches)))
      (spacemacs//open-in-external-app selected)
      ))

  (defun jg_layer/helm-open-random-exploration-action (candidate)
    " Randomly choose a directory until an openably file is found (wildcard optional)"
    ;; TODO
    (interactive)
    (let* ((pattern (car (last (f-split candidate))))
           (pattern-r (wildcard-to-regexp pattern))
           (files (helm-get-candidates (helm-get-current-source)))
           (all_matches (if (string-match-p "\*\." pattern)
                            (seq-filter
                             (lambda (x) (string-match-p pattern-r x))
                             files)
                          (f-files candidate)))
           (selected (seq-random-elt all_matches)))
      (spacemacs//open-in-external-app selected)
      ))

  (spacemacs|use-package-add-hook helm
    :post-config
    (helm-autoresize-mode 0)
    ;;add in keybinding to kill line in completion window
    (define-key helm-map (kbd "C-k") 'kill-line)
    (define-key helm-map (kbd "<backtab>") 'helm-select-action)
    (setq helm-find-files-actions (cons (car helm-find-files-actions)
                                        (cons '("Open Random" . jg_layer/helm-open-random-action)
                                              (cons '("Open Random External" . jg_layer/helm-open-random-external-action)
                                                    (cdr helm-find-files-actions)))))
    )
  )

(defun jg_layer/post-init-org ()
  (require 'jg_layer/clean-org "~/.spacemacs.d/layers/jg_layer/clean_org.el")
  ;;ORG SETUP
  (setq-default
   org-agenda-files `(,(expand-file-name "~/.spacemacs.d/setup_files/base_agenda.org"))
   org-archive-location (string-join `(,(expand-file-name "~/.spacemacs.d/setup_files/archive.org")
                                       "* Main Archive") "::")
   org-fast-tag-selection-single-key nil
   org-from-is-user-regexp "\\<John Grey\\>"
   org-group-tags nil
   org-use-fast-tag-selection t
   org-tags-column 80
   )

  (push 'org-indent-mode minor-mode-list)

  (defun jg_layer/org-mod-keymap ()
    (define-key org-mode-map (kbd "C-c [") nil)
    (define-key org-mode-map (kbd "C-c ]") nil)
    (define-key org-mode-map (kbd "z i") 'org-indent-mode)
    )



  (add-hook 'org-mode-hook 'jg_layer/org-mod-keymap)

  (spacemacs/set-leader-keys "a o a" nil)
  (spacemacs/set-leader-keys "a o l" nil)
  (spacemacs/set-leader-keys "a o /" nil)
  (spacemacs/set-leader-keys "a o e" nil)
  (spacemacs/set-leader-keys "a o m" nil)
  (spacemacs/set-leader-keys "a o o" nil)
  (spacemacs/set-leader-keys "a o s" nil)
  (spacemacs/set-leader-keys "a o t" nil)
  (spacemacs/set-leader-keys "a o c" nil)
  (spacemacs/set-leader-keys "a o #" nil)

  ;; add in keybinding to call tag-occurances
  (spacemacs/declare-prefix "a o a" "Agenda")
  (spacemacs/declare-prefix "a o i" "Insert")
  (spacemacs/declare-prefix "a o l" "Links")
  (spacemacs/set-leader-keys
    ;; AGENDA
    "a o a a"   'org-agenda
    "a o a /"   'org-occur-in-agenda-files
    "a o a f"   'org-agenda-file-to-front
    "a o a r"   'org-remove-file
    "a o a l"   'org-agenda-list
    "a o a F"   'jg_layer/list-agenda-files
    "a o a t"   'org-tags-view
    ;; Agenda -> Calendar
    "a o a c"   'org-goto-calendar
    "a o i t"   'org-time-stamp
    ;; LINKS
    "a o l s"   'org-store-link
    "a o l i"   'org-insert-link
    )

  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "v w"   'org-agenda-week-view
    "v  m"   'org-agenda-month-view
    )
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "." nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    ;; SRC CODE
    ". e"   'org-edit-src-code
    ". E"   'org-babel-execute-src-block
    ;; Links
    ". d"   'org-toggle-link-display
    ". o"   'jg_layer/open_link_in_buffer
    ". O"   'jg_layer/open_link_externally
    ". n"   'jg_layer/change_link_name
    ;; Formatting
    ". c"     'jg_layer/clean-org
    "i t"   'jg_layer/insert-heading-trio
    ;; Citation
    "i c" 'org-reftex-citation
    )

  ;;TODO add function to insert a bibliography
  ;;plus keybind it
  ;; #+BIBLIOGRAPHY: ~/github/writing/mendeley_library plain
  ;;keybind
  )

(defun jg_layer/post-init-yasnippet ()
  ;;yasnippet
  (setq-default yas-snippet-dirs `( ,(expand-file-name "~/.spacemacs.d/snippets/")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
                                    ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")))
  (spacemacs/declare-prefix "y" "Snippets/Abbevs")
  (spacemacs/set-leader-keys
    "y y"    'yas-expand
    "y i"    'yas-insert-snippet
    "y n"    'yas-new-snippet
    "y d"    'yas-describe-tables
    "y v"    'yas-visit-snippet-file
    )

  (spacemacs/set-leader-keys-for-major-mode 'edit-abbrevs-mode
    "y s" 'abbrev-edit-save-buffer
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

(defun jg_layer/post-init-erlang ()
  ;; (also has a load path set in root el file)
  erlang-root-dir "/usr/local/opt/erlang"
  exec-path (cons "/usr/local/opt/erlang/bin" exec-path)

  )

(defun jg_layer/post-init-python ()
  (print "Post init python")
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil
                python-shell-interpreter-args "-i"
                python-shell-interpreter "python"
                )

  (defun jg_layer/toggle-all-defs ()
    (interactive)
    ;; goto start of file
    (let* ((open-or-close 'evil-close-fold)
           (current (point))
           )
      (save-excursion
        (goto-char (point-min))
        (python-nav-forward-defun)
        (while (not (equal current (point)))
          (setq current (point))
          (if (jg_layer/line-starts-with? "def ")
              (funcall open-or-close))
          (python-nav-forward-defun)
          )
        )
      )
    )

  (defun jg_layer/close-class-defs ()
    (interactive )
    (save-excursion
      (let* ((current (point)))
        (python-nav-backward-defun)
        (while (and (not (jg_layer/line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-backward-defun)
          )
        )
      )
    (save-excursion
      (let* ((current (point)))
        (python-nav-forward-defun)
        (while (and (not (jg_layer/line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-forward-defun)
          )
        )
      )
    )

  (defun jg_layer/setup-python-mode ()
    (evil-define-key 'normal python-mode-map
      (kbd "z d") 'jg_layer/toggle-all-defs
      (kbd "z C") 'jg_layer/close-class-defs
      ))

  (add-hook 'python-mode-hook 'jg_layer/setup-python-mode)
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  )

(defun jg_layer/post-init-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg_layer/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    :config (progn
              ;; (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
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

(defun jg_layer/post-init-flycheck ()
  (setq flycheck-display-errors-function nil
        flycheck-help-echo-function nil
        flycheck-process-error-functions nil
        )

  (defun flycheck-finish-checker-process
      (checker exit-status files output callback cwd)
    """ Custom flycheck finisher to stop annoying 'suspicious' errors """
    (let ((errors (flycheck-parse-output output checker (current-buffer))))
      (funcall callback 'finished
               ;; Fix error file names, by substituting them backwards from the
               ;; temporaries.
               (seq-map (lambda (e) (flycheck-fix-error-filename e files cwd))
                        errors))))
  )

(defun jg_layer/init-academic-phrases ()
  (use-package academic-phrases
    :config
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "i A p" 'academic-phrases
      "i A s" 'academic-phrases-by-section)
    )
  )

(defun jg_layer/init-buffer-utils ()
  (use-package buffer-utils
    :defer t)
  )

(defun jg_layer/init-evil-string-inflection ()
  (use-package evil-string-inflection
    :config (define-key evil-normal-state-map "g'" 'evil-operator-string-inflection))
  )

(defun jg_layer/init-free-keys ()
  (use-package free-keys
    :config (spacemacs/set-leader-keys "a f k" 'free-keys
              "a f p" 'free-keys-set-prefix))
  )

(defun jg_layer/init-fsm ()
  (use-package fsm
    :defer t)
  )

;; (defun jg_layer/init-filesets+ ()
;;   (use-package filesets+
;;     :init (filesets-init))
;;   )

(defun jg_layer/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :init (dired-quick-sort-setup)
    )
  )

(defun jg_layer/post-init-highlight-parentheses ()
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )

(defun jg_layer/post-init-org-ref ()
  (spacemacs/set-leader-keys "a r" 'jg_layer/bibtex-load-random)

    (defun jg_layer/org-ref-open-bibtex-pdf ()
      "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
      (interactive)
      (save-excursion
        (bibtex-beginning-of-entry)
        (let* ((bibtex-expand-strings t)
               (entry (bibtex-parse-entry t))
               (key (reftex-get-bib-field "file" entry))
               (pdf (string-join `("/" ,(car (split-string key ":" 't))))))
          (message pdf)
          (if (file-exists-p pdf)
              (org-open-link-from-string (format "[[file:%s]]" pdf))
            (ding)))))

  (with-eval-after-load 'org-ref
    (setq org-ref-open-bibtex-pdf 'jg_layer/org-ref-open-bibtex-pdf)
    )
)

;; (use-package highlight-parentheses
;;   :init
;;   (progn
;;     (when (member dotspacemacs-highlight-delimiters '(all current))
;;       (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
;;     (setq hl-paren-delay 0.2)
;;     (spacemacs/set-leader-keys "tCp" 'highlight-parentheses-mode)
;;     (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
;;           hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3")))
;;   :config
;;   (spacemacs|hide-lighter highlight-parentheses-mode)
;;   (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun jg_layer/init-origami ()
  (use-package origami))

(defun jg_layer/post-init-origami ()

  (require 'jg_layer/origami-python-parser "~/.spacemacs.d/layers/jg_layer/local/origami-parser.el")
  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
  (add-to-list 'origami-parser-alist '(python-mode . jg_layer/origami-python-parser))
  )

(defun jg_layer/init-vlf ()
  (use-package vlf-setup
    :config (progn
              (define-key evil-normal-state-map (kbd "] A") 'vlf-next-batch-from-point)
              (define-key evil-normal-state-map (kbd "] a") 'vlf-next-batch)
              (define-key evil-normal-state-map (kbd "[ a") 'vlf-prev-batch)
              (spacemacs/declare-prefix "a v" "VLF")
              (spacemacs/set-leader-keys "a v b" 'vlf-set-batch-size))
    )
  )

(defun jg_layer/init-evil-quickscope ()

  (defun jg_layer/toggle-quickscope-always ()
    (interactive)
    (evil-quickscope-always-mode (if (eq nil evil-quickscope-always-mode)
                                     1
                                   0))
    )


  (use-package evil-quickscope
    :init
    (spacemacs/set-leader-keys
      "t q" 'jg_layer/toggle-quickscope-always)
    :config
    (global-evil-quickscope-always-mode 1)
    )
  )

(defun jg_layer/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    )
  )

(defun jg_layer/init-flycheck-plantuml ()
  (use-package flycheck-plantuml
    :defer t
    :config
    (flycheck-plantuml-setup)
    )
  )

(defun jg_layer/post-init-org-pomodoro ()
  (print "Setting up pomodoro")
  ;; set pomodoro log variable
  (defcustom jg_layer/pomodoro-log-file "~/.spacemacs.d/setup_files/pomodoro_log.org"
    "The Location of the Pomodoro Log File")
  (defcustom jg_layer/pomodoro-buffer-name "*Pomodoro Log*"
    "The name of the Pomodoro Log Buffer to record what I did in")
  (defcustom jg_layer/pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
    "The message to add to the log buffer to spur comments")


  ;; add a startup hook for pomodoro to tweet the end time
  (defun jg_layer/pomodoro-start-hook ()
    ;; tweet out start and end points
    ;; use org-pomodoro-end-time
    (jg_twitter/twitter-tweet-text
     (format "Emacs Pomodoro Timer Session to end: %s"
             (format-time-string "%H:%M (%d, %b, %Y)" org-pomodoro-end-time)))
    )

  (add-hook 'org-pomodoro-started-hook 'jg_layer/pomodoro-start-hook)
  ;; add a finished hook to ask for a recap of what was done,
  ;; and store it in a pomodoro log file
  (defun jg_layer/pomodoro-end-hook ()
    ;; create the temp buffer
    (progn
      (evil-window-new (get-buffer-window (current-buffer))
                       jg_layer/pomodoro-buffer-name)
      (set (make-local-variable 'backup-inhibited) t)
      (auto-save-mode -1)
      (evil-window-set-height 10)
      (evil-initialize-local-keymaps)
      (evil-local-set-key 'normal (kbd "C-c C-c")
                          'jg_layer/pomodoro-finish)
      (insert jg_layer/pomodoro-log-message)
      (insert "Pomodoro Session: ")
      (redraw-display)
      )
    )

  (defun jg_layer/pomodoro-finish ()
    ;; get the text
    (interactive)
    (let* ((text (buffer-substring (length jg_layer/pomodoro-log-message) (point-max)))
           (time (format-time-string "(%Y/%b/%d) %H:%M" (current-time)))
           (formatted (format "** %s\n    %s\n" time (string-trim text)))
           )
      ;; tweet it
      (jg_twitter/twitter-tweet-text text nil '(jg_twitter/tweet_sentinel))
      ;;add it to the pomodoro log file
      (append-to-file formatted nil (expand-file-name jg_layer/pomodoro-log-file))
      )
    )

  (add-hook 'org-pomodoro-finished-hook 'jg_layer/pomodoro-end-hook)
  (print "done pomodoro")
  )

(defun jg_layer/init-org-drill ()
  (use-package org-drill))

(defun jg_layer/post-init-dired ()
  (spacemacs/set-leader-keys
    "ad" nil
    )
  (evil-define-key 'normal dired-mode-map (kbd "M-n") 'jg_layer/dired-auto-move)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  )

(defun jg_layer/post-init-shell-pop ()
  (spacemacs/set-leader-keys
    "as" nil)
  )

