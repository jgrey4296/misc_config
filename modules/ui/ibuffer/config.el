;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(load! "+filters")
(load! "+funcs")
(after! (evil jg-bindings-total ibuffer)
  (load! "+bindings")
 )
(load! "+vars")

(use-package! ibuffer
  :hook (ibuffer . #'+jg-ui-ibuffer-update )

  :config
  (when (modulep! :completion ivy)
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
      (find-file _file _wildcards)))

  (load! "+format")

  )


(use-package! ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
        (if (modulep! +icons)
            (concat (all-the-icons-octicon
                     "file-directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " ")
          "Project: ")))
