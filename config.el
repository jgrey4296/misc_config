;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Grey"
      user-mail-address "johngrey4296@gmail.com")

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq +doom-quit-messages nil
      ;; auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
      backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
      dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$\\|^flycheck__.+\\.py\\|^\\.mypy_cache$"
      dired-omit-verbose nil
      display-buffer-alist '(("*shell*" display-buffer-same-window (nil)) (undo-tree-visualizer-buffer-name display-buffer-at-bottom (nil)))
      display-line-numbers-width 4
      evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-snipe-override-mode nil
      evil-snipe-repeat-scope nil
      ispell-personal-dictionary (expand-file-name "~/.doom.d/setup_files/.ispell_english")
      line-move-ignore-invisible t
      overflow-newline-into-fringe t
      pyvenv-default-virtual-env-name "~/anaconda3/envs/"
      tab-always-indent t
      tab-width 4
      which-key-idle-secondary-delay 0.05
      which-key-sort-order 'which-key-key-order-alpha
      custom-theme-load-path (cons "~/.doom.d/packages/jg-themes" custom-theme-load-path)
      highlight-indent-guides-suppress-auto-error t
      outline-blank-line nil
      line-move-ignore-invisible t
      ibuffer-old-time 2
      )

(progn
  (defun doom-highlight-non-default-indentation-h () )
  (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
  )

;; TODO: figure this out:
;; (push '("Scholar" "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%s") +lookup-provider-url-alist)
(smartparens-global-mode 0)
;;(window-ring-minor-mode)
;; (global-whitespace-mode)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-Iosvkem)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/github/writing/orgfiles/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
