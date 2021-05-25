(load! "+vars")
(after! evil
  (load! "+bindings")
)
(after! (ivy counsel)
  (load! "+ivy_actions")
  )

(after! helm-mode
  (map! :map helm-map
        "<tab>" nil
        "TAB" #'helm-select-action
        )
  )

(after! (company gtags helm-gtags)
  (set-company-backend! 'python-mode 'company-gtags)
  )
