5# 01jun13abu
# (c) Software Lab. Alexander Burger

(allowed ("wiki/" "db/wiki/blob/")
   *PW "!start" "!stop" "!wiki" "@lib.css" "!latex" "!pdf" "!psh"
   *PRG *Search "!download" )

(load
   "@lib/http.l" "@lib/xhtml.l" "@lib/form.l" "@lib/tex.l" "@lib/adm.l"
   "@lib/socialshareprivacy/lib.l"
   "@lib/boss.l" )

(setq
   *Scl 2
   *Pool "db/wiki/"
   *Blob "db/wiki/blob/"
   *Jnl (and (info "key/wiki") "fifo/wiki")
   *Salt (16 . "$6$@1$")
   *Css '("@lib.css" "wiki/lib.css")
   *WikiCss '("wiki/worg.css" "wiki/wiki.css")
   *WikiPort 5000 )

(load "wiki/er.l" "wiki/lib.l" "wiki/gui.l")

(permission
   Wiki        "Edit Wiki Documents"
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
      (load "wiki/init.l") )
   (unless (db 'nm '+Doc "home")
      (msg "No home document") )
   (if (db 'nm '+Doc "menu")
      (setq *WikiMenu (id @ T))
      (msg "No menu document") ) )

(de go ()
   (pw 12)
   (rollback)
   (server *WikiPort "!start") )

## # Start Emacs Server named "iorg-server"
## (call
##    'emacs
##    "-Q"
##    "--daemon=iorg-server"
##    (pack "--directory=" "@/iorg/el")
##    "--eval='(require 'ox)'" )

# vi:et:ts=3:sw=3
