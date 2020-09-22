;;; lang/org/+hooks.el -*- lexical-binding: t; -*-

;;
;;; `org-load' hooks

(defun +org-init-org-directory-h ()
  (unless org-directory
    (setq org-directory "~/org"))
  (setq org-id-locations-file (expand-file-name ".orgids" org-directory)))
(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   ;; Different colors for different priority levels
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   ;; Shift the agenda to show the previous 3 days and the next 7 days for
   ;; better context on your week. The past is less important than the future.
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"
   ;; Optimize `org-agenda' by inhibiting extra work while opening agenda
   ;; buffers in the background. They'll be "restarted" if the user switches to
   ;; them anyway (see `+org-exclude-agenda-buffers-from-workspace-h')
   org-agenda-inhibit-startup t))
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
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 8
        ;; Sub-lists should have different bullets
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{})

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (plist-put org-format-latex-options :scale 1.5) ; larger previews
  (add-hook! 'doom-load-theme-hook
    (defun +org-refresh-latex-background-h ()
      "Previews are rendered with the incorrect background.
This forces it to read the background before rendering."
      (plist-put! org-format-latex-options
                  :background
                  (face-attribute (if-let (remap (cadr (assq 'default face-remapping-alist)))
                                      (if (keywordp (car-safe remap))
                                          (plist-get remap :background)
                                        remap)
                                      'default)
                                  :background nil t))))

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))

  (defadvice! +org-display-link-in-eldoc-a (&rest _)
    "Display full link in minibuffer when cursor/mouse is over it."
    :before-until #'org-eldoc-documentation-function
    (when-let (link (org-element-property :raw-link (org-element-context)))
      (format "Link: %s" link)))

  ;; Automatic indent detection in org files is meaningless
  (add-to-list 'doom-detect-indentation-excluded-modes 'org-mode)

  (set-ligatures! 'org-mode
    :name "#+NAME:"
    :name "#+name:"
    :src_block "#+BEGIN_SRC"
    :src_block "#+begin_src"
    :src_block_end "#+END_SRC"
    :src_block_end "#+end_src"
    :quote "#+BEGIN_QUOTE"
    :quote "#+begin_quote"
    :quote_end "#+END_QUOTE"
    :quote_end "#+end_quote"))
(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t     ; we do this ourselves
        ;; You don't need my permission (just be careful, mkay?)
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        ;; Show src buffer in popup, and don't monopolize the frame
        org-src-window-setup 'other-window
        ;; Our :lang common-lisp module uses sly, so...
        org-babel-lisp-eval-fn #'sly-eval)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
  ;;   "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
  ;;   :after #'org-return
  ;;   (when (and indent
  ;;              org-src-tab-acts-natively
  ;;              (org-in-src-block-p t))
  ;;     (org-babel-do-in-edit-buffer
       ;; (call-interactively #'indent-for-tab-command))))

  (defadvice! +org-inhibit-mode-hooks-a (orig-fn datum name &optional initialize &rest args)
    "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
    :around #'org-src--edit-element
    (apply orig-fn datum name
           (if (and (eq org-src-window-setup 'switch-invisibly)
                    (functionp initialize))
               ;; org-babel-do-in-edit-buffer is used to execute quick, one-off
               ;; logic in the context of another major mode, but initializing a
               ;; major mode with expensive hooks can be terribly expensive.
               ;; Since Doom adds its most expensive hooks to
               ;; MAJOR-MODE-local-vars-hook, we can savely inhibit those.
               (lambda ()
                 (let ((doom-inhibit-local-var-hooks t))
                   (funcall initialize)))
             initialize)
           args))

  ;; Refresh inline images after executing src blocks (useful for plantuml or
  ;; ipython, where the result could be an image)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  (after! python
    (setq org-babel-python-command python-shell-interpreter)))
(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun +org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                 (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defadvice! +org--export-lazy-load-library-h ()
    "Lazy load a babel package when a block is executed during exporting."
    :before #'org-babel-exp-src-block
    (+org--babel-lazy-load-library-a (org-babel-get-src-block-info)))

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))

  ;; This also works for tangling
  (defadvice! +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    :after-while #'org-babel-confirm-evaluate
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (+org--babel-lazy-load lang (assq :async (nth 2 info)))
      t))

  (advice-add #'org-babel-do-load-languages :override #'ignore))
(defun +org-init-capture-defaults-h ()
  "Sets up some reasonable defaults, as well as two `org-capture' workflows that
I like:

1. The traditional way: invoking `org-capture' directly, via SPC X, or through
   the :cap ex command.
2. Through a org-capture popup frame that is invoked from outside Emacs (the
   ~/.emacs.d/bin/org-capture script). This can be invoked from qutebrowser,
   vimperator, dmenu or a global keybinding."
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  ;; HACK Doom doesn't support `customize'. Best not to advertise it as an
  ;;      option in `org-capture's menu.
  (defadvice! +org--remove-customize-option-a (orig-fn table title &optional prompt specials)
    :around #'org-mks
    (funcall orig-fn table title prompt
             (remove '("C" "Customize org-capture-templates")
                     specials)))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (add-hook! 'org-capture-mode-hook
    (defun +org-show-target-in-capture-header-h ()
      (setq header-line-format
            (format "%s%s%s"
                    (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                'face 'font-lock-string-face)
                    org-eldoc-breadcrumb-separator
                    header-line-format)))))
(defun +org-init-capture-frame-h ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture-cleanup-frame-h)

  (defadvice! +org-capture-refile-cleanup-frame-a (&rest _)
    :after #'org-capture-refile
    (+org-capture-cleanup-frame-h))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))
(defun +org-init-attachments-h ()
  "Sets up org's attachment system."
  (setq org-attach-store-link-p t     ; store link after attaching files
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
      (setq org-attach-id-dir (expand-file-name ".attach/" org-directory)))
    (after! projectile
      (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

  ;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))
(defun +org-init-custom-links-h ()
  ;; Highlight broken file links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   (file-exists-p path))
               'org-link
             'error)))

  ;; Add custom link types
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
            '("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s"))

  (+org-define-basic-link "org" 'org-directory)
  (+org-define-basic-link "doom" 'doom-emacs-dir)
  (+org-define-basic-link "doom-docs" 'doom-docs-dir)
  (+org-define-basic-link "doom-modules" 'doom-modules-dir)

  ;; Allow inline image previews of http(s)? urls or data uris.
  ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn)

  ;; Add support for youtube links + previews
  (require 'org-yt nil t)

  (defadvice! +org-dont-preview-if-disabled-a (&rest _)
    "Make `org-yt' respect `org-display-remote-inline-images'."
    :before-while #'org-yt-image-data-fun
    (not (eq org-display-remote-inline-images 'skip))))
(defun +org-init-export-h ()
  "TODO"
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t)

  (when (featurep! :lang markdown)
    (add-to-list 'org-export-backends 'md))

  (use-package! ox-hugo
    :when (featurep! +hugo)
    :after ox)

  (use-package! ox-pandoc
    :when (featurep! +pandoc)
    :when (executable-find "pandoc")
    :after ox
    :init
    (add-to-list 'org-export-backends 'pandoc)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://revealjs.com"))))

  (defadvice! +org--dont-trigger-save-hooks-on-export-a (orig-fn &rest args)
    "`org-export-to-file' triggers save hooks, which may inadvertantly change
the exported output (i.e. formatters)."
    :around #'org-export-to-file
    (let (before-save-hook after-save-hook)
      (apply orig-fn args)))

  (defadvice! +org--fix-async-export-a (orig-fn &rest args)
    :around #'org-export-to-file
    (if (not org-export-in-background)
        (apply orig-fn args)
      (setq org-export-async-init-file (make-temp-file "doom-org-async-export"))
      (with-temp-file org-export-async-init-file
        (prin1 `(progn (setq org-export-async-debug ,debug-on-error
                             load-path ',load-path)
                       (load ,user-init-file nil t))
               (current-buffer)))
      (apply orig-fn args))))
(defun +org-init-habit-h ()
  (add-hook! 'org-agenda-mode-hook
    (defun +org-habit-resize-graph-h ()
      "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
      (when (featurep 'org-habit)
        (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
               (preceding-days-ratio (/ org-habit-preceding-days total-days))
               (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
               (preceding-days (floor (* graph-width preceding-days-ratio)))
               (following-days (- graph-width preceding-days))
               (graph-column (- (window-width) (+ preceding-days following-days)))
               (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                          (- graph-column +org-habit-graph-padding)
                                        nil)))
          (setq-local org-habit-preceding-days preceding-days)
          (setq-local org-habit-following-days following-days)
          (setq-local org-habit-graph-column graph-column-adjusted))))))
(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; HACK Org is known to use a lot of unicode symbols (and large org files tend
  ;;      to be especially memory hungry). Compounded with
  ;;      `inhibit-compacting-font-caches' being non-nil, org needs more memory
  ;;      to be performant.
  (setq-hook! 'org-mode-hook gcmh-high-cons-threshold (* 2 gcmh-high-cons-threshold))

  (defadvice! +org--recenter-after-follow-link-a (&rest _args)
    "Recenter after following a link, but only internal or file links."
    :after '(org-footnote-action
             org-follow-timestamp-link
             org-link-open-as-file
             org-link-search)
    (when (get-buffer-window)
      (recenter)))

  (defadvice! +org--strip-properties-from-outline-a (orig-fn path &optional width prefix separator)
    "Remove link syntax and fix variable height text (e.g. org headings) in the
eldoc string."
    :around #'org-format-outline-path
    (funcall orig-fn
             (cl-loop for part in path
                      ;; Remove full link syntax
                      for fixedpart = (replace-regexp-in-string org-link-any-re "\\4" (or part ""))
                      for n from 0
                      for face = (nth (% n org-n-level-faces) org-level-faces)
                      collect
                      (org-add-props fixedpart
                          nil 'face `(:foreground ,(face-foreground face nil t) :weight bold)))
             width prefix separator))

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
                 'local))

  (add-hook! 'org-agenda-finalize-hook
    (defun +org-exclude-agenda-buffers-from-workspace-h ()
      "Prevent temporary agenda buffers being associated with current
workspace."
      (when (and org-agenda-new-buffers
                 (bound-and-true-p persp-mode)
                 (not org-agenda-sticky))
        (let (persp-autokill-buffer-on-remove)
          (persp-remove-buffer org-agenda-new-buffers
                               (get-current-persp)
                               nil))))
    (defun +org-defer-mode-in-agenda-buffers-h ()
      "Org agenda opens temporary agenda incomplete org-mode buffers.
These are great for extracting agenda information from, but what if the user
tries to visit one of these buffers? Then we remove it from the to-be-cleaned
queue and restart `org-mode' so they can grow up to be full-fledged org-mode
buffers."
      (dolist (buffer org-agenda-new-buffers)
        (with-current-buffer buffer
          (add-hook 'doom-switch-buffer-hook #'+org--restart-mode-h
                    nil 'local)))))

  (defadvice! +org--exclude-agenda-buffers-from-recentf-a (orig-fn file)
    "Prevent temporarily opened agenda buffers from polluting recentf."
    :around #'org-get-agenda-file-buffer
    (let ((recentf-exclude (list (lambda (_file) t)))
          (doom-large-file-p t)
          find-file-hook
          org-mode-hook)
      (funcall orig-fn file)))

  ;; HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
  ;;      image previews broke for users with imagemagick support built in. This
  ;;      reverses the problem, but should be removed once it is addressed
  ;;      upstream (if ever).
  (defadvice! +org--fix-inline-images-for-imagemagick-users-a (orig-fn &rest args)
    :around #'org-display-inline-images
    (letf! (defun create-image (file-or-data &optional type data-p &rest props)
             (let ((type (if (plist-get props :width) type)))
               (apply create-image file-or-data type data-p props)))
      (apply orig-fn args)))

  (defadvice! +org--fix-inconsistent-uuidgen-case-a (uuid)
    "Ensure uuidgen always produces lowercase output regardless of system."
    :filter-return #'org-id-new
    (if (eq org-id-method 'uuid)
        (downcase uuid)
      uuid)))
(defun +org-init-keybinds-h ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  (add-hook 'doom-escape-hook #'+org-remove-occur-highlights-h)

  ;; C-a & C-e act like `doom/backward-to-bol-or-indent' and
  ;; `doom/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)

  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)

  (add-hook! 'org-tab-first-hook
             #'+org-yas-expand-maybe-h
             #'+org-indent-maybe-h)

  (add-hook 'doom-delete-backward-functions
            #'+org-delete-backward-char-and-realign-table-maybe-h)

)
(defun +org-init-popup-rules-h ()
  (set-popup-rules!
    '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
      ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
       :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0)
      ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
      ("^\\*Org Agenda"     :ignore t)
      ("^\\*Org Src"        :size 0.4  :quit nil :select t :autosave t :modeline t :ttl nil)
      ("^\\*Org-Babel")
      ("^CAPTURE-.*\\.org$" :size 0.25 :quit nil :select t :autosave t))))
(defun +org-init-protocol-lazy-loader-h ()
  "Brings lazy-loaded support for org-protocol, so external programs (like
browsers) can invoke specialized behavior from Emacs. Normally you'd simply
require `org-protocol' and use it, but the package loads all of org for no
compelling reason, so..."
  (defadvice! +org--server-visit-files-a (args)
    "Advise `server-visit-flist' to invoke `org-protocol' lazily."
    :filter-args #'server-visit-files
    (cl-destructuring-bind (files proc &optional nowait) args
      (catch 'greedy
        (let ((flist (reverse files)))
          (dolist (var flist)
            (when (string-match-p ":/+" (car var))
              (require 'server)
              (require 'org-protocol)
              ;; `\' to `/' on windows
              (let ((fname (org-protocol-check-filename-for-protocol
                            (expand-file-name (car var))
                            (member var flist)
                            proc)))
                (cond ((eq fname t) ; greedy? We need the t return value.
                       (setq files nil)
                       (throw 'greedy t))
                      ((stringp fname) ; probably filename
                       (setcar var fname))
                      ((setq files (delq var files)))))))))
      (list files proc nowait)))

  ;; Disable built-in, clumsy advice
  (after! org-protocol
    (ad-disable-advice 'server-visit-files 'before 'org-protocol-detect-protocol-server)))
(defun +org-init-protocol-h ()
  ;; TODO org-board or better link grabbing support
  ;; TODO org-capture + org-protocol instead of bin/org-capture
  )
(defun +org-init-smartparens-h ()
  ;; Disable the slow defaults
  (provide 'smartparens-org))
