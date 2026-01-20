;;; config.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package! elfeed
  :config
  (setq elfeed-feeds '(
                       "https://www.rockpapershotgun.com/feed"
                       "https://www.techdirt.com/feed/"
                       "https://auratriolo.com/blog/feed/"
                       "https://enikofox.com/feed.xml"
                       "https://www.ntietz.com/atom.xml"
                       "https://noctuid.github.io/atom.xml"
                       )
        )

  )

;;; config.el ends here
