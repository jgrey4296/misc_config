;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org--server-visit-files-a (fn files &rest args)
  "Advise `server-visit-files' to load `org-protocol' lazily."
  ;; Wait until an org-protocol link is opened via emacsclient to load
  ;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
  ;; but the package loads all of org for no compelling reason, so...
  (if (not (cl-loop with protocol =
                    (if IS-WINDOWS
                        ;; On Windows, the file arguments for `emacsclient'
                        ;; get funnelled through `expand-file-path' by
                        ;; `server-process-filter'. This substitutes
                        ;; backslashes with forward slashes and converts each
                        ;; path to an absolute one. However, *all* absolute
                        ;; paths on Windows will match the regexp ":/+", so we
                        ;; need a more discerning regexp.
                        (regexp-quote
                         (or (bound-and-true-p org-protocol-the-protocol)
                             "org-protocol"))
                      ;; ...but since there is a miniscule possibility users
                      ;; have changed `org-protocol-the-protocol' I don't want
                      ;; this behavior for macOS/Linux users.
                      "")
                    for var in files
                    if (string-match-p (format "%s:/+" protocol) (car var))
                    return t))
      (apply fn files args)
    (require 'org-protocol)
    (apply #'org--protocol-detect-protocol-server fn files args)))

;;;###autoload
(defun +org--fail-gracefully-a (&rest _)
  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.

    (file-writable-p org-id-locations-file))

;;;###autoload
(defun +org-inhibit-scrolling-a (fn &rest args)
    "Prevent the jarring scrolling that occurs when the-ToC is regenerated."
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1 (apply fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil))))

;;;###autoload
(defun +org-fix-window-excursions-a (fn &rest args)
"Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used.
  HACK Fix #6061. Seems `org-babel-do-in-edit-buffer' has the side effect of
  deleting side windows. Should be reported upstream! This advice
  suppresses this behavior wherever it is known to be used.
"
(save-window-excursion (apply fn args)))

;;;###autoload
(defun +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
"Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
(when (and indent
            org-src-tab-acts-natively
            (org-in-src-block-p t))
    (save-window-excursion
    (org-babel-do-in-edit-buffer
        (call-interactively #'indent-for-tab-command)))))

;;;###autoload
(defun +org-inhibit-mode-hooks-a (fn datum name &optional initialize &rest args)
"Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
(apply fn datum name
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

;;;###autoload
(defun +org--export-lazy-load-library-a (&optional element)
  "Lazy load a babel package when a block is executed during exporting."
  (+org--babel-lazy-load-library-a (org-babel-get-src-block-info nil element))
)

;;;###autoload
(defun +org--src-lazy-load-library-a (lang)
  "Lazy load a babel package to ensure syntax highlighting."
  (or (cdr (assoc lang org-src-lang-modes))
      (+org--babel-lazy-load lang))
  )

;;-- babel

;;;###autoload
(defun +org-babel-disable-async-maybe-a (fn &optional orig-fn arg info params)
    "Use ob-comint where supported, disable async altogether where it isn't.

We have access to two async backends: ob-comint or ob-async, which have
different requirements. This advice tries to pick the best option between them,
falling back to synchronous execution otherwise. Without this advice, they die
with an error; terrible UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
    (if (null orig-fn)
        (funcall fn orig-fn arg info params)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (params (org-babel-merge-params (nth 2 info) params)))
        (if (or (assq :sync params)
                (not (assq :async params))
                (member (car info) ob-async-no-async-languages-alist)
                ;; ob-comint requires a :session, ob-async does not, so fall
                ;; back to ob-async if no :session is provided.
                (unless (member (alist-get :session params) '("none" nil))
                  (unless (memq (let* ((lang (nth 0 info))
                                       (lang (cond ((symbolp lang) lang)
                                                   ((stringp lang) (intern lang)))))
                                  (or (alist-get lang +org-babel-mode-alist)
                                      lang))
                                +org-babel-native-async-langs)
                    (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                             (car info))
                    (sleep-for 0.2))
                  t))
            (funcall orig-fn arg info params)
          (funcall fn orig-fn arg info params)))))

;;;###autoload
(defun +org--babel-lazy-load-library-a (info)
  "Load babel libraries lazily when babel blocks are executed."
  (let* ((lang (nth 0 info))
         (lang (cond ((symbolp lang) lang)
                     ((stringp lang) (intern lang))))
         )
    (+org--babel-lazy-load lang (and (not (assq :sync (nth 2 info)))
                                     (assq :async (nth 2 info))))
    t)
  )

(defun +org--babel-lazy-load (lang &optional async)
  "lazy load of babel languages"
  (let ((transformed (or (cdr (assq lang +org-babel-mode-alist)) lang))
        )
    (cond ((cdr (assq lang org-babel-load-languages))
             t)
          ((null transformed)
             (warn "Could not retrieve babel langauge information: %s : %s" lang transformed))
          (async
             ;; ob-async has its own agenda for lazy loading packages
             t)
          ((plist-get transformed :func) ;; Custom load function
             (apply (plist-get transformed :func))
             (add-to-list 'org-babel-load-languages (cons lang t))
             t)
          ((plist-get transformed :lib)  ;; named lib symbol
           (require (plist-get transformed :lib) nil t)
           (add-to-list 'org-babel-load-languages (cons lang t))
           t)
          ((plist-get transformed :name) ;; just name provided, tranform into ob-name
           (require (intern "ob-%s" (plist-get transformed :name)) nil t)
           (add-to-list 'org-babel-load-languages (cons lang t))
           t)
          (t (warn "Unrecognized babel language" lang))
          )
    )
  )

;;-- end babel

;;;###autoload
(defun +org--remove-customize-option-a (fn table title &optional prompt specials)
  " HACK Doom doesn't support `customize'. Best not to advertise it as an
     option in `org-capture's menu.
"
  (funcall fn table title prompt
           (remove '("C" "Customize org-capture-templates")
                   specials))
  )

;;;###autoload
(defun +org--capture-expand-variable-file-a (file)
  "If a variable is used for a file path in `org-capture-template', it is used
as is, and expanded relative to `default-directory'. This changes it to be
relative to `org-directory', unless it is an absolute path."
  (if (and (symbolp file) (boundp file))
      (expand-file-name (symbol-value file) org-directory)
    file)
  )

;;;###autoload
(defun +org-capture-refile-cleanup-frame-a (&rest _)
  (+org-capture-cleanup-frame-h)
  )

;;;###autoload
(defun +org--follow-search-string-a (fn link &optional arg)
"Support ::SEARCH syntax for id: links."
(save-match-data
  (cl-destructuring-bind (id &optional search)
      (split-string link "::")
    (prog1 (funcall fn id arg)
      (cond ((null search))
            ((string-match-p "\\`[0-9]+\\'" search)
             ;; Move N lines after the ID (in case it's a heading), instead
             ;; of the start of the buffer.
             (forward-line (string-to-number option)))
            ((string-match "^/\\([^/]+\\)/$" search)
             (let ((match (match-string 1 search)))
               (save-excursion (org-link-search search))
               ;; `org-link-search' only reveals matches. Moving the point
               ;; to the first match after point is a sensible change.
               (when (re-search-forward match)
                 (goto-char (match-beginning 0)))))
            ((org-link-search search))))))
)

;;;###autoload
(defun +org--dont-trigger-save-hooks-a (fn &rest args)
    "Exporting and tangling trigger save hooks; inadvertantly triggering
mutating hooks on exported output, like formatters."
    (let (before-save-hook after-save-hook)
      (apply fn args))
    )

;;;###autoload
(defun +org--fix-async-export-a (fn &rest args)
  ""
  (let ((old-async-init-file org-export-async-init-file)
        (org-export-async-init-file (make-temp-file "doom-org-async-export")))
    (doom-file-write
     org-export-async-init-file
     `((setq org-export-async-debug ,(or org-export-async-debug debug-on-error)
             load-path ',load-path)
       (unwind-protect
           (let ((init-file ,old-async-init-file))
             (if init-file
                 (load init-file nil t)
               (load ,early-init-file nil t)
               (require 'doom-start)))
         (delete-file load-file-name))))
    (apply fn args))
  )

;;;###autoload
(defun +org--recursive-org-persist-mkdir-a (fn &rest args)
  "`org-persist-write:index' does not recursively create
`org-persist-directory', which causes an error if it's a parent doesn't exist."
  (make-directory org-persist-directory t)
  )

;;;###autoload
(defun +org--more-startup-folded-options-a ()
  "Adds support for 'showNlevels*' startup options.
Unlike showNlevels, this will also unfold parent trees."
  (when-let (n (pcase org-startup-folded
                 (`show2levels* 2)
                 (`show3levels* 3)
                 (`show4levels* 4)
                 (`show5levels* 5)))
    (org-fold-show-all '(headings))
    (save-excursion
      (goto-char (point-max))
      (save-restriction
        (narrow-to-region (point-min) (or (re-search-forward org-outline-regexp-bol nil t) (point-max)))
        (org-fold-hide-drawer-all))
      (goto-char (point-max))
      (let ((regexp (if (and (wholenump n) (> n 0))
                        (format "^\\*\\{%d,%d\\} " (1- n) n)
                      "^\\*+ "))
            (last (point)))
        (while (re-search-backward regexp nil t)
          (when (or (not (wholenump n))
                    (= (org-current-level) n))
            (org-fold-core-region (line-end-position) last t 'outline))
          (setq last (line-end-position 0)))))
    t))

;;;###autoload
(defun +org--respect-org-auto-align-tags-a (&rest _)
  " Some uses of `org-fix-tags-on-the-fly' occur without a check on
`org-auto-align-tags', such as in `org-self-insert-command' and
`org-delete-backward-char'. "
:before-while #'org-fix-tags-on-the-fly
org-auto-align-tags)

;;;###autoload
(defun +org--recenter-after-follow-link-a (&rest _args)
    "Recenter after following a link, but only internal or file links."
    (when (get-buffer-window)
      (recenter)))

;;;###autoload
(defun +org--strip-properties-from-outline-a (fn &rest args)
    "Fix variable height faces in eldoc breadcrumbs."
    :around #'org-format-outline-path
    (let ((org-level-faces
           (cl-loop for face in org-level-faces
                    collect `(:foreground ,(face-foreground face nil t)
                              :weight bold))))
      (apply fn args)))

(defvar recentf-exclude)

;;;###autoload
(defun +org--optimize-backgrounded-agenda-buffers-a (fn file)
  "Prevent temporarily opened agenda buffers from polluting recentf."
  (let ((recentf-exclude (list (lambda (_file) t)))
        (doom-inhibit-large-file-detection t)
        org-startup-indented
        org-startup-folded
        vc-handled-backends
        org-mode-hook
        find-file-hook)
    (funcall fn file)))

;;;###autoload
(defun +org--fix-inline-images-for-imagemagick-users-a (fn &rest args)
  " HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
     image previews broke for users with imagemagick support built in. This
     reverses the problem, but should be removed once it is addressed
     upstream (if ever). "
(letf! (defun create-image (file-or-data &optional type data-p &rest props)
            (let ((type (if (plist-get props :width) type)))
            (apply create-image file-or-data type data-p props)))
  (apply fn args))
)

;;;###autoload
(defun +org--fix-inconsistent-uuidgen-case-a (uuid)
    "Ensure uuidgen always produces lowercase output regardless of system."
    :filter-return #'org-id-new
    (if (eq org-id-method 'uuid)
        (downcase uuid)
      uuid))

;;;###autoload
(defun +org--clock-load-a (&rest _)
    "Lazy load org-clock until its commands are used."
    (org-clock-load)
    )
