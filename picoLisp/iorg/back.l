## * iorg/back.l --- Backup for iOrg
## ** MetaData 
##   :PROPERTIES:
##   :copyright: Alexander_Burger Thorsten_Jolitz
##   :copyright-since: 2011
##   :version:  0.9
##   :licence:  GPL3+
##   :licence-url: http://www.gnu.org/licenses/
##   :authors: Alexander_Burger Thorsten_Jolitz
##   :contact: <abu@software-lab.de> <tjolitz@gmail.com>
##   :git-repo: https://github.com/tj64/iorg
##   :git-clone: git@github.com:tj64/iorg.git
##   :keywords: emacs org-mode picolisp wiki
##   :END:

## ** Comments

## * Code

(unless (= *PW (pw))
   (quit "back.l" *PW) )

(load "@lib/too.l")

(out NIL
   ## Timestamp
   (prinl *Pid " + Backup " (stamp))

   ## Terminate all
   (tell 'bye 2)

   (when (lock)
      (quit "Can't lock DB" @) )

   ## Check integrity
   (for (F . @) *Dbs
      (when (dbck F)
         (quit "DB Check" (cons F @)) ) )

   ## Backup
   (call "sh" "-c" "tar cfz bak/iorg$(date +%a).tgz db/iorg/")

   ## Garbage collection
   (when (dbgc)
      (println 'dbgc @) )

   ## Exit
   (prinl *Pid " - Backup " (stamp)) )

## iorg/back.l ends here
