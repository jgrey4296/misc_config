;;; tools/lookup/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+backends")
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! xref
  :config
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defadvice! +lookup--projectile-find-tag-a (fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall fn)))

  )

(use-package! ivy-xref
  :when (modulep! :completion ivy)
  :config
  ;; HACK Fix #4386: `ivy-xref-show-xrefs' calls `fetcher' twice, which has
  ;; side effects that breaks in some cases (i.e. on `dired-do-find-regexp').
  (defadvice! +lookup--fix-ivy-xrefs (fn fetcher alist)
    :around #'ivy-xref-show-xrefs
    (when (functionp fetcher)
      (setf (alist-get 'fetched-xrefs alist)
            (funcall fetcher)))
    (funcall fn fetcher alist))

  )

(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)
  )

(use-package! osx-dictionary
  :when IS-MAC
  )

(use-package! browse-url)

(spec-handling-new! lookup-url +lookup-provider-url-alist nil append
                    val
                    )

(spec-handling-new! browse-handler browse-url-handlers nil append
                    val
                    )

(spec-handling-new-hooks! lookup-handler
                          (setq-local +lookup-definition-functions      (plist-get val :definition)
                                      +lookup-implementations-functions (plist-get val :implementations)
                                      +lookup-type-definition-functions (plist-get val :type-definition)
                                      +lookup-references-functions      (plist-get val :references)
                                      +lookup-documentation-functions   (plist-get val :documentation)
                                      +lookup-file-functions            (plist-get val :file)
                                      )
                          )

(spec-handling-new-hooks! lookup-regular
                          ;; Val : alist of (name . url)
                          (setq-local lookup-regular-targets val)
                          )
