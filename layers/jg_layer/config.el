;; jg_emacs config.el
;; loaded fourth

(xterm-mouse-mode 0)
(setq-default
 ;; AUTO SAVES
 auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
 backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
 display-buffer-alist (quote (("*shell*" display-buffer-same-window (nil))))
 icicle-Completions-text-scale-decrease 0
 ;;personal iSpell dictionary
 ispell-personal-dictionary (expand-file-name "~/.spacemacs.d/setup_files/.ispell_english")
 dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$"

 ;;use spaces instead of tabs
 tab-width 4
 indent-tabs-mode nil
 )
;; force utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(eval-after-load 'dired-mode
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  ;;(setq dired-listing-switches "-alh")
)
