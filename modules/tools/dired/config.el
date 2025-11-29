;;; tools/dired/config.el -*- lexical-binding: t; -*-

(defer-load! (jg-bindings-total jg-dired) "+bindings")
(defer-load! "+extra")
(defer-load! "+dirvish")
(defer-load! "+fontlock")
(defer-load! "+omit")

(advice-add 'read-file-name-default :around #'+jg-dired-find-file-with-insert-plus-a)
(advice-add 'counsel-find-file :around #'+jg-dired-find-file-with-insert-plus-a)

(use-package! dired
  :commands dired-jump
  :config
  (provide 'jg-dired)
  (after! (evil evil-snipe)
    (push 'dired-mode evil-snipe-disabled-modes)
    )
  (put 'dired-find-alternate-file 'disabled nil)

  (advice-add 'dired-buffer-stale-p :before-while #'+dired--no-revert-in-virtual-buffers-a)

  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (add-hook! 'dired-before-readin-hook #'(lambda () (when (file-remote-p default-directory) (setq dired-actual-switches "-al"))))

  (setq-hook! 'dired-mode-hook
    evil-disable-insert-state-bindings t
    )
  )


(speckler-setq! dired ()
  dired-listing-switches (string-join '("-ahlD" "-v" "--group-directories-first") " ")
  insert-directory-program (or (executable-find "gls") "ls")
  read-file-name-function #'read-file-name-default

  dired-auto-revert-buffer #'dired-buffer-stale-p
  dired-clean-confirm-killing-deleted-buffers nil
  dired-create-destination-dirs 'ask
  dired-dwim-target t
  dired-hide-details-hide-symlink-targets nil
  dired-omit-verbose nil
  dired-recursive-copies  'always
  dired-recursive-deletes 'top
  dired-vc-rename-file t
  dired-quick-sort-group-directories-last ?y
  )
(speckler-add! fold ()
  `(dired
    :modes (dired-mode)
    :priority 50
    :triggers (:open-all   nil
               :close-all  nil
               :toggle     #'(lambda () (dired-hide-subdir 1))
               :open       nil
               :open-rec   nil
               :close      nil
               )
    )
  )
(speckler-add! popup ()
  '(dired
    ("^\\*image-dired" :slot 20 :size 0.8 :select t :quit nil :ttl 0)
    ("^\\*ranger" :ignore t)
    ("^\\*CookieCutter\\*" :side bottom :select nil :quit t :ttl 0)
    ("^\\*file-hashes\\*" :side bottom :select nil :quit t :ttl 3)
    ("^\\*File Metadata\\*\\'" :width 80 :side left :select nil :quit t :ttl 3)
    )
  )
(speckler-setq! filetypes ()
  dired-guess-shell-alist-user `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'"               ,(if (eq system-type 'darwin) "open -a Preview -nF" "evince"))
                                 ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'"               "eog")
                                 ("\\.\\(?:mp3\\|flac\\)\\'"                            "xdg-open")
                                 ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\)\\(?:\\.part\\)?\\'" "xdg-open")
                                 ("\\.\\(?:rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"       "xdg-open")
                                 ("\\.\\(?:xcf\\)\\'"                                   "xdg-open")
                                 ("\\.csv\\'"                                           "xdg-open")
                                 ("\\.html?\\'"                                         "xdg-open")
                                 ("\\.md\\'"                                            "xdg-open")
                                 ("\\.svg\\'"                                           "eog")
                                 ("\\.tex\\'"                                           "xdg-open")
                                 )
  )
