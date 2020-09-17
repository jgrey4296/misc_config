;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(global-set-key (kbd "C-c [") 'jg-spacemacs-main-layer/insert-lparen)
(global-set-key (kbd "C-c ]") 'jg-spacemacs-main-layer/insert-rparen)

(map! :after (featurep! :editor evil)
 :m "TAB" nil
 :m "TAB" #'indent-for-tab-command
 )
