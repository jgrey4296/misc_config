;;; lang/org/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+hooks")
(after! evil
  (load! "+bindings")
  (load! "+bindings_2")
  )
;;
;;; Packages
(use-package! toc-org ; auto-table of contents
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh")

  (defadvice! +org-inhibit-scrolling-a (orig-fn &rest args)
    "Prevent the jarring scrolling that occurs when the-ToC is regenerated."
    :around #'toc-org-insert-toc
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1 (apply orig-fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil)))))
(use-package! org-crypt ; built-in
  :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
  :hook (org-reveal-start . org-decrypt-entry)
  :preface
  ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
  ;; is a better default than the empty string `org-crypt-key' defaults to.
  (defvar org-crypt-key nil)
  (after! org
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")
    (add-hook! 'org-mode-hook
      (add-hook 'before-save-hook 'org-encrypt-entries nil t))))
(use-package! org-clock ; built-in
  :commands org-clock-save
  :init
  (setq org-clock-persist-file (concat doom-etc-dir "org-clock-save.el"))
  (defadvice! +org--clock-load-a (&rest _)
    "Lazy load org-clock until its commands are used."
    :before '(org-clock-in
              org-clock-out
              org-clock-in-last
              org-clock-goto
              org-clock-cancel)
    (org-clock-load))
  :config
  (setq org-clock-persist 'history
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t)
  (add-hook 'kill-emacs-hook #'org-clock-save))
(use-package! org-pdftools
  :when (featurep! :tools pdf)
  :commands org-pdftools-export
  :init
  (after! org
    (org-link-set-parameters (or (bound-and-true-p org-pdftools-link-prefix) "pdf")
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook! 'org-open-link-functions
      (defun +org-open-legacy-pdf-links-fn (link)
        "Open pdftools:* and pdfviews:* links as if they were pdf:* links."
        (let ((regexp "^pdf\\(?:tools\\|view\\):"))
          (when (string-match-p regexp link)
            (org-pdftools-open (replace-regexp-in-string regexp "" link))
            t))))))
(use-package! evil-org
  :when (featurep! :editor evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             ;; #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
  )
(use-package! evil-org-agenda
  :when (featurep! :editor evil +everywhere)
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  (evil-define-key* 'motion evil-org-agenda-mode-map
    (kbd doom-leader-key) nil))

;;
;;; Bootstrap

(use-package! org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  ;; Set to nil so we can detect user changes to them later (and fall back on
  ;; defaults otherwise).
  (defvar org-directory nil)
  (defvar org-attach-id-dir nil)

  (setq org-publish-timestamp-directory (concat doom-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
        ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
        org-list-allow-alphabetical t)

  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay. I sincerely doubt most users use them all.
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))

  ;;; Custom org modules
  (dolist (flag doom--current-flags)
    (load! (concat "contrib/" (substring (symbol-name flag) 1)) nil t))

  ;; Add our general hooks after the submodules, so that any hooks the
  ;; submodules add run after them, and can overwrite any defaults if necessary.
  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'doom-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace'; shows a lot of false positives
             #'doom-disable-show-trailing-whitespace-h
             #'+org-enable-auto-reformat-tables-h
             #'+org-enable-auto-update-cookies-h
             #'+org-make-last-point-visible-h)

  (add-hook! 'org-load-hook
             #'+org-init-org-directory-h
             #'+org-init-appearance-h
             #'+org-init-agenda-h
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-habit-h
             #'+org-init-hacks-h
             #'+org-init-keybinds-h
             #'+org-init-popup-rules-h
             #'+org-init-protocol-h
             #'+org-init-protocol-lazy-loader-h
             #'+org-init-smartparens-h)

  ;; (Re)activate eldoc-mode in org-mode a little later, because it disables
  ;; itself if started too soon (which is the case with `global-eldoc-mode').
  (add-hook 'org-mode-local-vars-hook #'eldoc-mode)

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (unless doom-reloading-p
      (message "`org' was already loaded by the time lang/org loaded, this may cause issues"))
    (run-hooks 'org-load-hook))

  :config
  (set-company-backend! 'org-mode 'company-capf 'company-dabbrev)
  (set-eval-handler! 'org-mode #'+org-eval-handler)
  (set-lookup-handlers! 'org-mode
    :definition #'+org-lookup-definition-handler
    :references #'+org-lookup-references-handler
    :documentation #'+org-lookup-documentation-handler)

  ;; Save target buffer after archiving a node.
  (setq org-archive-subtree-save-file-p t)

  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)

  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  (add-hook 'org-open-at-point-functions #'doom-set-jump-h))
