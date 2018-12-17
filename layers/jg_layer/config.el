;; jg_emacs config.el
;; loaded fourth

(defun silence ()
  (interactive))

;;(add-hook 'change-major-mode-after-body-hook 'fci-mode)
;;(neotree-show)
(xterm-mouse-mode 0)
(setq-default evil-escape-delay 0.3
              ;; AUTO SAVES
              auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/`" t)))
              backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
              display-buffer-alist (quote (("*shell*" display-buffer-same-window (nil))))
              icicle-Completions-text-scale-decrease 0
              ;;ORG SETUP
              org-agenda-files `(,(expand-file-name "~/github/jg_shell/shellnotes.org"))
              org-fast-tag-selection-single-key nil
              org-from-is-user-regexp "\\<John Grey\\>"
              org-group-tags nil
              org-use-fast-tag-selection t
              ;;abbrev-file complaint quieting
              abbrev-file-name (expand-file-name "~/.spacemacs.d/setup_files/abbrevs_defs")

              ;;setting up erlang
              ;; (also has a load path set in root el file)
              erlang-root-dir "/usr/local/opt/erlang"
              exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
              ;;yasnippet
              yas-snippet-dirs `( ,(expand-file-name "~/.spacemacs.d/snippets/")
                                  ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
                                  ,(expand-file-name "~/github/otherLibs/yasnippet-snippets"))
              ;;personal iSpell dictionary
              ispell-personal-dictionary (expand-file-name "~/.spacemacs.d/setup_files/.ispell_english")
              dired-omit-files "^\\.?#\\|^\\.$\\|^\\.DS_Store$\\|^\\.git$\\|^__pycache__$"

              ;;use spaces instead of tabs
              tab-width 4
              indent-tabs-mode nil
              ;;python
              python-indent-offset 4
              python-indent-guess-indent-offset nil
              )
;; force utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
