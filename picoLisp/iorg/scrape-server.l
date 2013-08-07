(allowed ()
   "!scrape" "!expect" "!click" "!press" "!value" "!enter"
   "!display" "!displayAll" )

(load "@lib.l" "@ext.l" "@lib/http.l" "@lib/scrape.l")

(setq *Port (port 6789))
(loop
   (setq *Sock (listen *Port))
   (NIL (fork) (close *Port))
   (close *Sock) )
(in *Sock 
   (while (read)
      (out *Sock
         (println (eval @)) ) ) )
(bye)
