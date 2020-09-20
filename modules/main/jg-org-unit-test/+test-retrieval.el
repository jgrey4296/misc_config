
;; Test retrieval
(defun jg-org-test-find-tests ()
  "Iterate through current org file, extracting any __org-unit-test-layer__ drawers contents."
  (save-excursion
    (goto-char (point-min))
    (org-map-entries 'jg-org-test-extract-tests-from-entry)
    )
  )
(defun jg-org-test-extract-tests-from-entry ()
  (let ((start (point)) end text heading element link)
    (setq heading (substring-no-properties (org-get-heading t t t t))
          link (org-store-link nil)
          end (plist-get :contents-end (cadr (org-element-at-point))))
    (if (and (re-search-forward org-drawer-regexp end t)
             (s-equals? "__org-unit-test-layer__" (match-string 1))
             (s-equals? heading (org-get-heading t t t t)))
        (progn (setq element (cadr (org-element-at-point))
                     text (string-trim (buffer-substring-no-properties (plist-get element :contents-begin)
                                                                       (plist-get element :contents-end))))
               (if (not (string-empty-p text))
                   (make-jg-org-test-test-extracts :start start :end end :heading heading :link link :text text)
                 nil)
               )
      nil
      )))
