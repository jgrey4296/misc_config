;; trie funcs.el
;; loaded third.

(defun trie/trie-ide-running-p ()
  " Tests whether the ide is running or not "
  trie/trie-ide-is-running
  )
(defun trie/toggle-trie-ide ()
  (interactive)
  (if (trie/trie-ide-running-p)
      (trie/stop-trie-ide)
    (trie/start-trie-ide))
  )
(defun trie/start-trie-ide ()
  " Start the trie ide, setting up windows, etc "
  (interactive)
  ;; Get the directory to work with
  (let ((location (read-file-name "Institution Location:"))
        (windows (trie/setup-trie-mode-windows))
        inst-name
        inst-buffer
        )

    ;;if a dir chosen, get a name for the
    ;;inst, create a stub, create a data dir
    (if (f-dir? location)
        (progn
          ;;get name for inst
          (setq inst-name (read-string "Institution Name: ")
                trie/ide-data-loc (f-join location (format "%s-data" inst-name)))
          )
      ;;else if an org chosen, load it and its data dir
      (progn
        (assert (equal (f-ext location) "org"))
        (setq inst-name (f-base location)
              location (f-parent location)
              trie/ide-data-loc (f-join location (format "%s-data" inst-name))
              )
        ))

    ;;TODO: If doesn't exist, make the data location subdirectories
    (if (not (f-exists? trie/ide-data-loc))
        (progn (mkdir trie/ide-data-loc)
               (mapc (lambda (x) (mkdir (f-join trie/ide-data-loc x)))
                     (list "rules" "types" "crosscuts" "patterns" "tests"))
               )
      )

    ;;create inst stub
    (if (not (f-exists? (f-join location (format "%s.org" inst-name))))
        (progn
          (setq inst-buffer (create-file-buffer (f-join location (format "%s.org" inst-name))))
          (with-current-buffer inst-buffer
            ;; insert default institution contents
            (trie-mode)
            (org-mode)
            (yas-expand-snippet (yas-lookup-snippet "pipeline" 'trie-mode))
            (write-file (f-join location (format "%s.org" inst-name)))
            )
          )
      (setq inst-buffer (find-file (f-join location (format "%s.org" inst-name))))
      )

    (trie/load-directory-and-pipeline inst-buffer trie/ide-data-loc)

    (window--display-buffer (generate-new-buffer trie/inputs-buffer-name)  (plist-get windows :prior)'window)
    (window--display-buffer (generate-new-buffer trie/outputs-buffer-name)  (plist-get windows :post) 'window)
    (window--display-buffer (generate-new-buffer "rule")  (plist-get windows :rule) 'window)
    (window--display-buffer inst-buffer (plist-get windows :miscL) 'window)
    (window--display-buffer (generate-new-buffer trie/logging-buffer-name)  (plist-get windows :miscR) 'window)
    (window--display-buffer (generate-new-buffer trie/working-group-buffer-name)  (plist-get windows :miscC) 'window)
    ;;Save the window configuration
    (setq trie/window-confguration (current-window-configuration))
    ;;start python server
    (trie/run-python-server)
    ;;setup windows and their modes
    ;; (in trie | trie-select |
    ;; org | pipeline | explore | sequence)

    )

  (setq trie/trie-ide-running t)
  )
(defun trie/stop-trie-ide ()
  (interactive)
  ;;Clear windows, unload data
  (trie/dump-to-files)
  (setq trie/trie-ide-running nil)
  )
(defun trie/reset-windows ()
  (interactive)
  (if (and (trie/trie-ide-running-p) (window-configuration-p trie/window-configuration))
      (set-window-configuration trie/window-configuration)
    )
  )
(cl-defun trie/setup-trie-mode-windows ()
  """ Setup rule editing windows """
  ;; (terminals - ) priors - rule - posts (terminals)
  ;;                       defeaters
  ;;       upstream stats  - alts - downstream stats
  (interactive)
  (let (prior post rule miscL miscC miscR)
    (delete-other-windows)
    ;; split in half
    (setq prior (selected-window))
    (setq miscL (split-window-below))
    ;;Top half:
    ;; Split into three: priors, rule, posts
    (setq rule (split-window-right))
    (select-window rule)
    (setq post (split-window-right))
    ;;Bottom Half
    ;; Split into three: upstream, alts, downstream
    (select-window miscL)
    (setq miscC (select-window (split-window-right)))
    (setq miscR (split-window-right))

    (list :prior prior :post post :rule rule :miscL miscL :miscC miscC :miscR miscR)
    )
  )
(defun trie/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )
(defun trie/load-directory-and-pipeline (inst-buffer loc)
  " Given a location, load into ide "
  (let ((files (f-files loc nil t)))
    (loop for file in files do
          (let ((ftype (f-ext file)))
            ;;Handle each file type and store it in its management hash-table
            (cond ((equal ftype "rule"    ) )
                  ((equal ftype "type"    ) )
                  ((equal ftype "cc"      ) )
                  ((equal ftype "pattern" ) )
                  ((equal ftype "test"    ) )
                  )
            )
          )
    )
  )
(defun trie/dump-to-files ()
  (interactive)
  ;;Get all trie-* mode buffers, and the pipeline spec
  ;;and write to subdirs of trie/ide-data-loc



  )
(defun trie/run-python-server ()
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test" 


  )
(defun trie/trigger-tests ()
  " Trigger a Bank of tests "
  (interactive)
  ;;with buffer rule logs
  ;;clear?
  ;;get tests for working group
  ;;run tests
  ;;print results


  )
(defun trie/no-op ()
  (interactive)
  )

;; (defun trie/toggle-all-defs ()
;;   (interactive)
;;   ;; goto start of file
;;   (let* ((open-or-close 'evil-close-fold)
;;          (current (point))
;;          )
;;     (save-excursion
;;       (goto-char (point-min))
;;       (python-nav-forward-defun)
;;       (while (not (equal current (point)))
;;         (setq current (point))
;;         (if (trie/line-starts-with? "def ")
;;             (funcall open-or-close))
;;         (python-nav-forward-defun)
;;         )
;;       )
;;     )
;;   )
;; (defun trie/close-class-defs ()
;;   (interactive )
;;   (save-excursion
;;     (let* ((current (point)))
;;       (python-nav-backward-defun)
;;       (while (and (not (trie/line-starts-with? "class "))
;;                   (not (equal current (point))))
;;         (evil-close-fold)
;;         (setq current (point))
;;         (python-nav-backward-defun)
;;         )
;;       )
;;     )
;;   (save-excursion
;;     (let* ((current (point)))
;;       (python-nav-forward-defun)
;;       (while (and (not (trie/line-starts-with? "class "))
;;                   (not (equal current (point))))
;;         (evil-close-fold)
;;         (setq current (point))
;;         (python-nav-forward-defun)
;;         )
;;       )
;;     )
;;   )
;; (defun trie/setup-python-mode ()
;;   (evil-define-key 'normal python-mode-map
;;     (kbd "z d") 'trie/toggle-all-defs
;;     (kbd "z C") 'trie/close-class-defs
;;     ))

;; (src (helm-make-source "My Find" 'helm-source-ffiles))
;; (helm-ff-setup-update-hook)
;; (setq location (helm :sources (helm-make-source "My Find" 'helm-source-ffiles
;;                                 :action (helm-make-actions "Default" 'identity))
;;                      :input (expand-file-name (helm-current-directory))
;;                      :case-fold-search helm-file-name-case-fold-search
;;                      :ff-transformer-show-only-basename
;;                      helm-ff-transformer-show-only-basename
;;                      :prompt "Find my files"
;;                      :buffer "*helm my find*"
;;                      )
