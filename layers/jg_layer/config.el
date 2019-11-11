;; jg_emacs config.el
;; loaded fourth

(setq-default auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
              backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
              display-buffer-alist (quote (("*shell*" display-buffer-same-window (nil))))
              icicle-Completions-text-scale-decrease 0
              ;;personal iSpell dictionary
              ispell-personal-dictionary (expand-file-name "~/.spacemacs.d/setup_files/.ispell_english")
              ;;Dired customisation
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$\\|^flycheck__.+\\.py"
              dired-omit-verbose nil
              dired-listing-switches "-lha"
              ;;use spaces instead of tabs
              tab-width 4
              indent-tabs-mode nil
              ;;bookmark location
              jg-bookmark-location "~/github/writing/other_files/main_bookmarks.html"
               ;;Flycheck
              flycheck-display-errors-function nil
              flycheck-help-echo-function nil
              flycheck-process-error-functions nil
              ;;evil cursor control:
              evil-move-beyond-eol t
)
;; force utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
