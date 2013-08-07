(load "@lib.l" "@ext.l" "@lib/http.l" "@lib/scrape.l")
(setq *Port (port 16789))
(loop
   (setq *Sock (listen *Port))
   (NIL (fork) (close *Port))
   (close *Sock) )
(in *Sock 
   (while (read)
      (out *Sock
         (println (eval @)) ) ) )
(bye)
