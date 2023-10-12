;;; +vars.el -*- lexical-binding: t; -*-

(defalias 'number-list (symbol-function 'number-sequence))

(defvar +default-want-RET-continue-comments t "If non-nil, RET will continue commented lines.")

;; General
(setq save-silently (not noninteractive)
      xterm-set-window-title t
      visible-cursor nil

      auto-revert-verbose t ; let us know when it happens
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      revert-without-query (list ".") ;; Only prompts for confirmation when buffer is unsaved.

      tramp-default-method "ssh" ;; faster than the default scp
      )


(setq recentf-auto-cleanup nil     ; Don't. We'll auto-cleanup on shutdown
      recentf-max-saved-items 200) ; default is 20

(setq savehist-save-minibuffer-history t
      savehist-autosave-interval nil     ; save on kill only
      savehist-additional-variables '(kill-ring                        ; persist clipboard
                                      register-alist                   ; persist macros
                                      mark-ring global-mark-ring       ; persist marks
                                      search-ring regexp-search-ring) ; persist searches
)


(setq auth-sources (list (expand-file-name "~/.config/secrets/emacs/authinfo.asc")))


;; Ftp
(setq ftp-program "git-ftp")

(setq-default diary-file (expand-file-name "~/github/jgrey4296.github.io/orgfiles/main.diary")
              major-mode #'emacs-lisp-mode
              )

(spec-handling-add! auto-modes
                    '(vim
                      ("\\.vimrc\\'" . vimrc-mode)
                      )
                    )
