5# 01jun13abu
# (c) Software Lab. Alexander Burger

(allowed ("iorg/" "db/iorg/blob/")
   *PW "!start" "!stop" "!iorg" "@lib.css" "!latex" "!pdf" "!psh"
   *PRG *Search "!download" )

(load
   "@lib/http.l" "@lib/xhtml.l" "@lib/form.l" "@lib/tex.l" "@lib/adm.l"
   "@lib/socialshareprivacy/lib.l"
   "@lib/boss.l"
   "@iorg/lib.l")

(setq
   *Scl 2
   *Pool "db/iorg/"
   *Blob "db/iorg/blob/"
   *Jnl (and (info "key/iorg") "fifo/iorg")
   *Salt (16 . "$6$@1$")
   *Css '("@lib.css" "iorg/css/lib.css")
   *IorgCss '("iorg/css/worg.css" "iorg/css/iorg.css")
   *IorgPort 5000 )

(load "iorg/er.l" "iorg/lib.l" "iorg/gui.l")

(permission
   Iorg        "Edit Iorg Documents"
   System      "Edit System Data"
   Upload      "File upload"
   RoleAdmin   "Role Administration"
   UserAdmin   "User Administration"
   Password    "Password"
   Delete      "Delete" )

(mime "d" NIL 120)

# Entry point
(de main ()
   (call 'mkdir "-p" *Pool *Blob)
   (pool *Pool *Dbs *Jnl)
   (unless (seq *DB)
      (load "iorg/init.l") )
   (unless (db 'nm '+Doc "home")
      (msg "No home document") )
   (if (db 'nm '+Doc "menu")
      (setq *IorgMenu (id @ T))
      (msg "No menu document") ) )

(de go ()
   (pw 12)
   (rollback)
   (server *IorgPort "!start") )

## # Start Emacs Server named "iorg-server"
## (call
##    'emacs
##    "-Q"
##    "--daemon=iorg-server"
##    (pack "--directory=" "@/iorg/el")
##    "--eval='(require 'ox)'" )

# vi:et:ts=3:sw=3
