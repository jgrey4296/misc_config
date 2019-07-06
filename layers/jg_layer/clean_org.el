(require 'dash)
(provide 'jg_layer/clean-org)
;; clean and format org files automatically


(defun jg_layer/clean-org ()
  (interactive)
  ;; indent region
  (evil-indent (point-min) (point-max))
  (whitespace-cleanup)
  ;; fill
  (evil-fill (point-min) (point-max))
  ;;Get Links from link sections
  (goto-char (point-min))
  ;; (search-forward-regexp "\\*+ Links" nil 1 nil)
  ;; (while (< (point) (point-max))
  ;;   ;; get current level,
  ;;   ;; get links in the current level

  ;;   (search-forward-regexp "\\*+ Links" nil 1 nil)
  ;;   )

  ;; for all links:



  ;; ensure on a newline

  ;; for all tweets
  ;; go line by line, separate out sentences

  ;;for all hashtags put on newline

  ;;apply whitelisted tags if found

  )
