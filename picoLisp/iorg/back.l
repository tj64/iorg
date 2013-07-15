# 04aug11abu
# (c) Software Lab. Alexander Burger

(unless (= *PW (pw))
   (quit "back.l" *PW) )

(load "@lib/too.l")

(out NIL
   # Timestamp
   (prinl *Pid " + Backup " (stamp))

   # Terminate all
   (tell 'bye 2)

   (when (lock)
      (quit "Can't lock DB" @) )

   # Check integrity
   (for (F . @) *Dbs
      (when (dbck F)
         (quit "DB Check" (cons F @)) ) )

   # Backup
   (call "sh" "-c" "tar cfz bak/iorg$(date +%a).tgz db/iorg/")

   # Garbage collection
   (when (dbgc)
      (println 'dbgc @) )

   # Exit
   (prinl *Pid " - Backup " (stamp)) )

# vi:et:ts=3:sw=3
