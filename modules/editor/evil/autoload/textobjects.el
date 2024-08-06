;;; editor/evil/autoload/textobjects.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+evil:whole-buffer-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))

;;;###autoload (autoload '+evil:defun-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
  "Text object to select the top-level Lisp form or function definition at
point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end type)))

;;;###autoload (autoload '+evil:inner-url-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
  "Text object to select the inner url at point.

This excludes the protocol and querystring."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     (save-excursion
       (goto-char beg)
       (re-search-forward "://" end t))
     (save-excursion
       (goto-char end)
       (- (if-let (pos (re-search-backward "[?#]" beg t))
              pos
            end)
          (if (evil-visual-state-p)
              1
            0)))
     type)))

;;;###autoload (autoload '+evil:outer-url-txtobj "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
  "Text object to select the whole url at point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     beg (- end (if (evil-visual-state-p) 1 0))
     type)))

;;;###autoload (autoload '+evil:inner-any-quote "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:inner-any-quote (count &optional beg end type)
  "Select the closest inner quote."
  (require 'evil-textobj-anyblock)
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "`")
           ("‘" . "’")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count nil)))

;;;###autoload (autoload '+evil:outer-any-quote "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +evil:outer-any-quote (count &optional beg end type)
  "Select the closest outer quote."
  (require 'evil-textobj-anyblock)
  (let ((evil-textobj-anyblock-blocks
         '(("'" . "'")
           ("\"" . "\"")
           ("`" . "`")
           ("‘" . "’")
           ("“" . "”"))))
    (evil-textobj-anyblock--make-textobj beg end type count t)))

;;;###autoload (autoload '+jg-evil-whitespace "editor/evil/autoload/textobjects" nil t)
(evil-define-text-object +jg-evil-whitespace (count &optional beg end type)
  "select spaces on the same line"
  :type exclusive
  :extend-selection nil
  (let* ((start (save-excursion (re-search-backward (rx (not space)) nil t)))
         (end   (save-excursion (re-search-forward  (rx (not space)) nil t)))
         (result (list
                  (if start (+ 1 start) (point))
                  (if end   (- end 2)   (point))
                  )
                 )
         )
    (message "whitespace Result: %s  | %s %s" result start end)
    result
    )
  )

;;;###autoload (autoload '+jg-evil-gap "editor/evil/autoload/textobjects" nil nil)
(evil-define-text-object +jg-evil-gap (count &optional beg end type)
  "an evil textobject for selecting contiguous whitespace"
  :type exclusive
  :extend-selection nil
  (let* ((start (save-excursion (re-search-backward (rx (not blank) blank) nil t)))
         (end (save-excursion   (re-search-forward  (rx blank (not blank)) nil t)))
         (result (list
                  (if (and start
                           (<= (line-beginning-position) start))
                      (+ 1 start)
                    (point))
                  (if (and end
                           (<= end (line-end-position)))
                      (- end 2)
                    (point))
                  )
                 )
         )
    result
    )
  )
