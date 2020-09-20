;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(defun +jg-personal-insert-lparen ()
  """ utility to insert a (  """
  (interactive)
  (insert "(")
  )
(defun +jg-personal-insert-rparen ()
  """ utility to insert a ) """
  (interactive)
  (insert ")")
  )
(global-set-key (kbd "C-c [") '+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") '+jg-personal-insert-rparen)

(map! :after (featurep! :editor evil)
 :m "TAB" nil
 :m "TAB" #'indent-for-tab-command
 )

(map! :after evil
      :map evil-motion-state-map
      "\\" nil)

(map! :after ibuffer
      :mode ibuffer-mode
      "\\" ibuffer--filter-map
      )

(map! :n "gb" #'avy-pop-mark)
