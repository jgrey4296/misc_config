;;; lang/jg-cron/config.el -*- lexical-binding: t; -*-

(use-package! cron-mode
  :defer t
  :commands cron-mode
  :mode ("/crontab\\..+" . cron-mode)
  :config
  (setq jg-cron-guru-url "https://crontab.guru/")
  (map! :map cron-mode-map
        :localleader
        :desc "Crontab Guru" "1" (cmd! (+jg-misc-browse-url jg-cron-guru-url)))
  )
