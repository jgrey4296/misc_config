;;; +clean-funcs.el --- summary -*- lexical-binding: t -*-
;;
;; Main Function
(defun +jg-org-clean (&optional skipfck)
  " "
  (interactive)
  (message "Starting Org Clean")
  (+jg-org-add-twitter-property)
  ;; Initial clean: Wrap links
  (goto-char (point-min))
  (org-show-all)
  (org-cycle-hide-drawers 'all)
  (+jg-org-wrap-non-link-urls)
  ;; Indent the buffer
  (indent-region (point-min) (point-max))
  ;; Fill Paragraphs from bottom up
  (+jg-org-fill-paragraph-reverse)
  ;; Cleanup Whitespace
  (whitespace-cleanup)
  ;; Clean picture links
  (+jg-org-pic-twitter-clean)
  ;; Map over entries, removing extra space between entries
  (setq jg-org-clean-marker (make-marker))
  (org-map-entries '+jg-org-map-entries-clean-whitespace t nil)
  (set-marker jg-org-clean-marker nil)
  ;; Tidy all links:
  (+jg-org-property-drawer-clean-links)
  (+jg-org-clean-property-blocks)
  ;; Re-indent
  (indent-region (point-min) (point-max))
  ;; Re-clean whitespace
  (whitespace-cleanup)
  (setq jg-org-clean-marker nil)

  ;; Connect links together again
  (+jg-org-refill-links)

  ;; Deal with bad links
  (if (not skipfck)
      (+jg-org-find-bad-links)
    )

  ;; Hide Drawers
  (org-cycle-hide-drawers 'all)
  ;; Goto start
  (goto-char (point-min))
  (message "Org Clean Finished")
  )

;; Utils
(defun +jg-org-fill-paragraph-reverse ()
  " from END to START of buffer, fill paragraphs "
  (interactive)
  (goto-char (point-max))
  (let ((prog (lambda ()
                (let ((elem (cadr (org-element-at-point))))
                  (fill-region-as-paragraph
                   (cl-getf elem :begin) (cl-getf elem :end)))))
        curr-elem
        return-to
        )
    (while (not (eq (point) (point-min)))
      (setq curr-elem (org-element-at-point)
            return-to (cl-getf (cadr curr-elem) :begin))
      (cond ((eq (car curr-elem) 'property-drawer)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            ((eq (car curr-elem) 'headline)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            (t
             (goto-char (cl-getf (cadr curr-elem) :begin))
             (funcall prog)
             (insert "\n")
             )
            )
      (goto-char (- return-to 1))
      )
    )
  )
(defun +jg-org-pic-twitter-clean ()
  ;Find all pic.twitter's and ensure on new line
  (goto-char (point-min))
  (message "Finding pic.twitter's")
  (while (search-forward "pic.twitter" nil t)
    (let ((sub (buffer-substring (line-beginning-position) (point))))
      (if (not (eq 0 (string-match "^[[:space:]]+pic.twitter" sub)))
          (progn
            (backward-char (+ 1 (length "pic.twitter")))
            (insert "\n\n")))
      (progn
        (while (eq 0 (string-match "^[[:space:]]*$"
                                   (buffer-substring (line-beginning-position -0)
                                                     (line-end-position -0))))
          (join-line))
        (goto-char (line-beginning-position))
        (insert "\n")
        (forward-line)
        (end-of-line)
        )
      )
    )
  )

(defun +jg-org-refill-links ()
  ;;Find and replace
  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )
)
(defun +jg-org-find-bad-links ()
  (progn
    (goto-char (point-min))
    (while (re-search-forward  "file:.+?%.+$" nil t)
      (if (+jg-org-link-not-exists-p)
          (progn
            (goto-char (line-beginning-position))
            (insert "--->")
            (if (s-equals? (read-string "Delete line? ") "y")
                (delete-region (line-beginning-position) (line-end-position))
              (progn
                (delete-region (line-beginning-position)
                               (+ (line-beginning-position) (length "--->")))
                (forward-line))
              ))
        )
      )
    )
  )


;; Map Entries
(defun +jg-org-map-entries-clean-whitespace ()
  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker jg-org-clean-marker (line-end-position))
  (if (not (eq (point) (point-min)))
      (progn
        (while (eq 0 (string-match "^[[:space:]]*$"
                                   (buffer-substring
                                    (line-beginning-position 0)
                                    (line-end-position 0))))
          (join-line))
        (if (not (string-equal "*" (buffer-substring
                                    (line-beginning-position 0)
                                    (+ 1 (line-beginning-position 0)))))
            (insert "\n"))
        (setq org-map-continue-from jg-org-clean-marker)
        )
    )
  )
;; Properites blocks cleaning
(defun +jg-org-clean-property-blocks ()
  " Find Property blocks and clean newlines in them "
  ;; TODO merge with +jg-org-fix-properties-drawers
  (goto-char (point-min))
  ;; Find properties block
  (let ((start-marker (make-marker))
        (end-marker (make-marker)))
  (while (search-forward-regexp ":PROPERTIES:" nil t)
    (set-marker start-marker (point))
    (search-forward ":END:" nil t)
    (goto-char (nth 0 (match-data)))
    (set-marker end-marker (point))
    (goto-char start-marker)
    (while (< (point) end-marker)
      (join-line 1)
      )
    (goto-char start-marker)
    ;; Loop over adding new lines appropriately
    (while (and (< (point) end-marker) (search-forward-regexp "\\(:[_A-Z]+:\\)" nil t))
      (replace-match "\n\\&")
      )
    )
  )
  )
(defun +jg-org-fix-properties-drawers ()
  " Force properties drawer formatting of single lines "
  ;; TODO merge with +jg-org-clean-property-blocks
  (interactive)
  (goto-char (point-min))
  ;; Find properties drawer
  (while (re-search-forward "^\s*:PROPERTIES:$" nil t)
    ;; for each :TERM:, ensure it is on its own line
    (let* ((context (org-element-context))
           (drawer-end (plist-get (cadr context) :contents-end))
           (drawer-type (car context))
           )
      (assert (or (equal 'property-drawer drawer-type) (equal 'drawer drawer-type)))
      (while (re-search-forward "\\(:[[:upper:]_]+:\\)" drawer-end t)
        (save-excursion
          (goto-char (match-beginning 0))
          (if (not (equal (point) (line-beginning-position)))
              (insert "\n"))
          )
        (if (looking-at-p "$")
            (join-line -1))
        )
      )
    )
  )
(defun +jg-org-property-drawer-clean-links ()
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil t)
    (set-marker jg-org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond  ((eq 0 (string-match "^[[:space:]]+:PERMALINK:" prev-line))
              (join-line))
             ((eq 0 (string-match "^[[:space:]]+:PROPERTIES:" prev-line))
              nil)
             ((eq 0 (string-match "^[[:space:]]+:URL: " (buffer-substring (line-beginning-position) (point))))
              nil)
             ((not (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                         (line-beginning-position)
                                                         (point)))))
              (insert "\n")
              )
             (t
              (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                           (line-beginning-position 0)
                                                           (line-end-position 0))))
                (join-line)
                )
              )
             )
      )
    (goto-char jg-org-clean-marker)
    )




  )

(defun +jg-org-add-twitter-property ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-set-property "TWITTER-BUFFER" "t")
    )
  )
