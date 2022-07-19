;;; +file-templates.el -*- lexical-binding: t; -*-
(after! jg-file-templates
        ;; Python
        (set-file-templates!
        '(python-mode       :trigger "__")
        '("\\.py$"          :trigger "__"               :mode python-mode)
        '("setup\\.py$"     :trigger "__setup"          :mode python-mode)
        '("conf\\.py$"      :trigger "__conf"           :mode python-mode)
        '("_.+\\.py$"       :trigger "__"               :mode python-mode)
        '("_cli.+\\.py$"    :trigger "__cli"            :mode python-mode)
        '("test_.+\\.py$"   :trigger "__tests"          :mode python-mode)
        '("__init__\\.py$"  :trigger "__init"           :mode python-mode)
        '("setup\\.cfg$"    :trigger "__setup_cfg"      :mode python-mode)
        '("pyproject.toml$" :trigger "__pyproject_toml" :mode python-mode)
        '("LICENSE$"        :trigger "__license-acab"   :mode text-mode)
        )
        )
