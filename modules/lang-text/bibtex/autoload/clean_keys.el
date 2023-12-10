;;; +keys.el -*- lexical-binding: t; -*-
(require 'bibtex)

(defvar jg-bibtex-default-stubkey-base "stub_key_")

;;;###autoload
(defun +jg-bibtex-insert-stub-key ()
  "Insert a stub key if there isnt an actual one"
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (if (looking-at ",")
      (insert (format "%s%s" jg-bibtex-default-stubkey-base (random 5000)))
    )
  )

;;;###autoload
(defun +jg-bibtex-orcb-key-hook ()
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file.
Does not modify keys ending in an underscore
 "
  (bibtex-beginning-of-entry)
  ;; (end-of-line)
  ;; (forward-char -2)
  ;; (unless (looking-at "_")
  (search-forward "{" (line-end-position) t)
  (when (looking-at jg-bibtex-default-stubkey-base)
    (let ((key (bibtex-generate-autokey))
          handle-duplicate
          )
      ;; remove any \\ in the key
      (setq key (replace-regexp-in-string "[ \\\\'{}]" "" key))
      ;; first we delete the existing key
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (if (match-beginning bibtex-key-in-head)
	  (delete-region (match-beginning bibtex-key-in-head)
		         (match-end bibtex-key-in-head)))
      ;; check if the key is in the buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (bibtex-search-entry key)
	    (bibtex-search-entry key)
	    (bibtex-copy-entry-as-kill)
            (setq handle-duplicate t)
            )
          )
        )

      (when handle-duplicate
        (save-window-excursion
          (switch-to-buffer-other-window "*duplicate entry*")
          (bibtex-yank)
          (setq key (bibtex-read-key "Duplicate Key found, edit: " key))
          (kill-buffer "*duplicate entry*")
          )
        )
      (insert key)
      (kill-new key))
    )
  )

;;;###autoload
(defun +jg-bibtex-insert-volume-to-key ()
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (let ((vol (s-replace " " "_" (bibtex-autokey-get-field "volume"))))
    (unless (or (s-equals? vol "") (looking-at ".+?_,") (looking-at (format ".+?_%s" vol)))
      (goto-char (- (line-end-position) 1))
      (insert (format "_%s" vol))
      )
    )
  )
