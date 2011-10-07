(req server)
(unless (server-running-p)
  (server-start))
(req sudo-ext)
