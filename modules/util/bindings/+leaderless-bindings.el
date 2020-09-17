;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(message "Loading Leaderless bindings")
(map! :after (featurep! :editor evil)
 :m "TAB" nil
 :m "TAB" #'indent-for-tab-command
 )
