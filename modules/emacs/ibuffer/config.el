;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+specs")
(load! "ivy/+ivy")
(after! (evil jg-bindings-total ibuffer)
  (load! "+bindings")
 )

(use-package! ibuffer
  :config
  (load! "+format")
  (load! "+sorting")
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (setq-hook! 'ibuffer-hook
    evil-disable-insert-state-bindings t
    )
  )

(use-package! ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  )

(use-package! ibuffer-vc
  :after ibuffer
  )

(after! (ibuffer ivy)
  (defadvice! +ibuffer--use-counsel-maybe-a (_file &optional _wildcards)
    "Use `counsel-find-file' instead of `find-file'."
    :override #'ibuffer-find-file
    (interactive
     (let* ((buf (ibuffer-current-buffer))
            (default-directory (if (buffer-live-p buf)
                                   (with-current-buffer buf
                                     default-directory)
                                 default-directory)))
       (list (counsel--find-file-1 "Find file: " nil
                                   #'identity
                                   'counsel-find-file) t)))
    (find-file _file _wildcards))
  )

(spec-handling-new! ibuffer-filters ibuffer-saved-filters nil collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-groups ibuffer-saved-filter-groups nil collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-formats ibuffer-formats nil collect
                    val
                    )
