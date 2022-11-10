;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;-- Me
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Grey"
      user-mail-address "johngrey4296 at gmail.com"
      user-url "https://jgrey4296.github.io/")
;;-- end Me

;;-- Text Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method "greek")
;;-- end Text Encoding

;;-- locations
(setq backup-directory-alist          (quote ((".*" . "~/.emacs.d/backups/")))
      ispell-personal-dictionary      (expand-file-name "~/.ispell_english")
      pyvenv-default-virtual-env-name "~/anaconda3/envs/"
      org-directory                   "~/github/writing/orgfiles/"
      org-agenda-files               `(,(expand-file-name "setup_files/base_agenda.org" doom-user-dir))
      org-archive-location            (string-join `(,(expand-file-name "setup_files/archive.org" doom-user-dir) "* Main Archive") "::")
      initial-buffer-choice           (expand-file-name "setup_files/base_agenda.org" doom-user-dir)
      doom-fallback-buffer-name       "base_agenda.org"
      bookmark-default-file           (expand-file-name "bookmarks" doom-user-dir)
      )
;;-- end locations

;;-- evil
(setq evil-collection-setup-minibuffer t
      evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-snipe-repeat-scope nil
 )
;;-- end evil

;;-- doom settings
(setq +doom-quit-messages nil
      doom-theme 'jg-Iosvkem)
;; Override doom's whitespace mode settings:
(fset 'doom-highlight-non-default-indentation-h #'(lambda () nil))
;;-- end doom settings

;;-- which key
(setq which-key-idle-secondary-delay 0.05
      which-key-sort-order 'which-key-key-order-alpha
 )
;;-- end which key

;;-- global modes
(setq flycheck-global-modes nil
      )

(add-hook! doom-first-buffer
   #'global-highlight-parentheses-mode
   #'global-autohide-minor-mode
   )

;; (remove-hook! doom-first-buffer
;;   #'smartparens-global-mode
;;   )
(remove-hook! 'after-change-major-mode-hook
  #'+ligatures-init-buffer-h
  #'global-flycheck-mode-enable-in-buffers
  )
(remove-hook! 'doom-init-ui-hook
  #'+ligatures-init-h
  )
;;-- end global modes

;;-- misc variables
(setq +lsp-defer-shutdown 10
      display-line-numbers-width 4
      highlight-indent-guides-suppress-auto-error t
      ibuffer-old-time 2
      outline-blank-line nil
      overflow-newline-into-fringe t
      )
;;-- end misc variables

;;-- defaults
(setq-default line-move-ignore-invisible t
              avy-all-windows t
              display-line-numbers-type t
              tab-always-indent t
              indent-tabs-mode nil
              LaTeX-enable-toolbar nil
              whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)
              )
;;-- end defaults

;;-- Byte Compilation
;; from https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(not cl-functions))
;;-- end Byte Compilation

;;-- lookup
(setq +lookup-provider-url-alist '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")

                                   ("Scholar" "https://scholar.google.com/scholar?hl=en&q=%s")
                                   ("DBLP"    "https://dblp1.uni-trier.de/search?q=%s")
                                   ("Doi"     "https://doi.org/%s")
                                   ("Github" "https://github.com/search?ref=simplesearch&q=%s")
                                   ("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")

                                   ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
                                   ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
                                   ("DevDocs.io" "https://devdocs.io/#q=%s")
                                   ("StackOverflow" "https://stackoverflow.com/search?q=%s")

                                   ("Twitter" "https://twitter.com/%s")
                                   ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                                   ("Archive.org" "https://archive.org/search.php?query=%s")
                                   ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                                   ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
                                   ("Amazon UK" "https://www.amazon.co.uk/s?k=%s")
                                   ("Amazon US" "https://www.amazon.com/s?k=%s")

                                   ("Wolfram alpha" "https://wolframalpha.com/input/?i=%s")
                                   ("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
                                   ("Google images" "https://www.google.com/images?q=%s")
                                   ("Raw" "%s")
                                   )
      )
;;-- end lookup
