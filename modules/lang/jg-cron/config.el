;;; lang/jg-cron/config.el -*- lexical-binding: t; -*-

(use-package! cron-mode
  :defer t
  :commands cron-mode
  :mode ("/crontab\\..+" . cron-mode)
  :config
  (setq jg-cron-guru-url "https://crontab.guru/")
  (map! :map cron-mode-map
        :localleader
        :desc "Crontab Guru" "1" (+jg-browse-url-cmd jg-cron-guru-url))
  )
