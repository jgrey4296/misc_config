;;; +hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        ;; `showeverything' is org's default, but it doesn't respect
        ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
        ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
        ;; respects these settings.
        org-startup-folded nil)

  (plist-put org-format-latex-options :scale 1.5) ; larger previews

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
  ;; Automatic indent detection in org files is meaningless

  )

;;;###autoload
(defun +org-init-attachments-h ()
  "Sets up org's attachment system."
  (setq org-attach-store-link-p 'attached     ; store link after attaching files
        org-attach-use-inheritance t) ; inherit properties from parent nodes

  ;; Autoload all these commands that org-attach doesn't autoload itself
  (use-package! org-attach
    :commands (org-attach-new
               org-attach-open
               org-attach-open-in-emacs
               org-attach-reveal-in-emacs
               org-attach-url
               org-attach-set-directory
               org-attach-sync)
    :config
    (unless org-attach-id-dir
      ;; Centralized attachments directory by default
      (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
    (after! projectile
      (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

  ;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))

;;;###autoload
(defun +org-init-custom-links-h ()
  ;; Modify default file: links to colorize broken file links red
  (org-link-set-parameters
   "file" :face (lambda (path)
                  (if (or (file-remote-p path)
                          ;; filter out network shares on windows (slow)
                          (if IS-WINDOWS (string-prefix-p "\\\\" path))
                          (file-exists-p path))
                      'org-link
                    '(warning org-link))))

  ;; Additional custom links for convenience
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
            '("doom-repo"   . "https://github.com/doomemacs/doomemacs/%s")
            `("emacsdir"    . ,(doom-path doom-emacs-dir "%s"))
            `("doomdir"     . ,(doom-path doom-user-dir "%s")))

  (+org-define-basic-link "org" 'org-directory)
  (+org-define-basic-link "doom" 'doom-emacs-dir)
  (+org-define-basic-link "doom-docs" 'doom-docs-dir)
  (+org-define-basic-link "doom-modules" 'doom-modules-dir)

  ;; Add "lookup" links for packages and keystrings; useful for Emacs
  ;; documentation -- especially Doom's!
  (letf! ((defun -call-interactively (fn)
            (lambda (path _prefixarg)
              (funcall
               fn (or (intern-soft path)
                      (user-error "Can't find documentation for %S" path))))))
    (org-link-set-parameters
     "kbd"
     :follow (lambda (ev)
               (interactive "e")
               (minibuffer-message "%s" (+org-link-doom--help-echo-from-textprop
                                         nil (current-buffer) (posn-point (event-start ev)))))
     :help-echo #'+org-link-doom--help-echo-from-textprop
     :face 'help-key-binding)
    (org-link-set-parameters
     "var"
     :follow (-call-interactively #'helpful-variable)
     :activate-func #'+org-link--var-link-activate-fn
     :face 'org-code)
    (org-link-set-parameters
     "fn"
     :follow (-call-interactively #'helpful-callable)
     :activate-func #'+org-link--fn-link-activate-fn
     :face 'org-code)
    (org-link-set-parameters
     "face"
     :follow (-call-interactively #'describe-face)
     :activate-func #'+org-link--face-link-activate-face
     :face '(font-lock-type-face underline))
    (org-link-set-parameters
     "cmd"
     :follow (-call-interactively #'describe-command)
     :activate-func #'+org-link--command-link-activate-command
     :face 'help-key-binding
     :help-echo #'+org-link-doom--help-echo-from-textprop)
    (org-link-set-parameters
     "doom-package"
     :follow #'+org-link-follow-doom-package-fn
     :activate-func #'+org-link--doom-package-link-activate-fn
     :face (lambda (_) '(:inherit org-priority :slant italic))
     :help-echo #'+org-link-doom--help-echo-from-textprop)
    (org-link-set-parameters
     "doom-module"
     :follow #'+org-link-follow-doom-module-fn
     :activate-func #'+org-link--doom-module-link-activate-fn
     :face #'+org-link--doom-module-link-face-fn
     :help-echo #'+org-link-doom--help-echo-from-textprop)
    (org-link-set-parameters
     "doom-executable"
     :activate-func #'+org-link--doom-executable-link-activate-fn
     :help-echo #'+org-link-doom--help-echo-from-textprop
     :face 'org-verbatim)
    (org-link-set-parameters
     "doom-ref"
     :follow (lambda (link)
               (let ((link (+org-link-read-desc-at-point link))
                     (url "https://github.com")
                     (doom-repo "doomemacs/doomemacs"))
                 (save-match-data
                   (browse-url
                    (cond ((string-match "^\\([^/]+\\(?:/[^/]+\\)?\\)?#\\([0-9]+\\(?:#.*\\)?\\)" link)
                           (format "%s/%s/issues/%s" url
                                   (or (match-string 1 link)
                                       doom-repo)
                                   (match-string 2 link)))
                          ((string-match "^\\([^/]+\\(?:/[^/]+\\)?@\\)?\\([a-z0-9]\\{7,\\}\\(?:#.*\\)?\\)" link)
                           (format "%s/%s/commit/%s" url
                                   (or (match-string 1 link)
                                       doom-repo)
                                   (match-string 2 link)))
                          ((user-error "Invalid doom-ref link: %S" link)))))))
     :face (lambda (link)
             (let ((link (+org-link-read-desc-at-point link)))
               (if (or (string-match "^\\([^/]+\\(?:/[^/]+\\)?\\)?#\\([0-9]+\\(?:#.*\\)?\\)" link)
                       (string-match "^\\([^/]+\\(?:/[^/]+\\)?@\\)?\\([a-z0-9]\\{7,\\}\\(?:#.*\\)?\\)" link))
                   'org-link
                 'error))))
    (org-link-set-parameters
     "doom-user"
     :follow (lambda (link)
               (browse-url
                (format "https://github.com/%s"
                        (string-remove-prefix
                         "@" (+org-link-read-desc-at-point link)))))
     :face (lambda (_) 'org-priority))
    (org-link-set-parameters
     "doom-changelog"
     :follow (lambda (link)
               (find-file (doom-path doom-docs-dir "changelog.org"))
               (org-match-sparse-tree nil link))))

  ;; Add "lookup" links for packages and keystrings; useful for Emacs
  ;; documentation -- especially Doom's!

  ;; Allow inline image previews of http(s)? urls or data uris.
  ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn)

  ;; Add support for youtube links + previews
  (require 'org-yt nil t)

  )

;;;###autoload
(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))

  ;; Unlike the stock showNlevels options, these will also show the parents of
  ;; the target level, recursively.
  (pushnew! org-startup-options
            '("show2levels*" org-startup-folded show2levels*)
            '("show3levels*" org-startup-folded show3levels*)
            '("show4levels*" org-startup-folded show4levels*)
            '("show5levels*" org-startup-folded show5levels*))

  (after! org-eldoc
    ;; HACK Fix #2972: infinite recursion when eldoc kicks in in 'org' or
    ;;      'python' src blocks.
    ;; TODO Should be reported upstream!
    (puthash "org" #'ignore org-eldoc-local-functions-cache)
    (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
    (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

  (defun +org--restart-mode-h ()
    "Restart `org-mode', but only once."
    (quiet! (org-mode-restart))
    (delq! (current-buffer) org-agenda-new-buffers)
    (remove-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                 'local)
    (run-hooks 'find-file-hook))

  )
