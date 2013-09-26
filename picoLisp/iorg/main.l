## * iorg/main.l --- Main-file for iOrg
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

## ** Permissions

(allowed ("iorg/" "db/iorg/blob/")
   *PW "!start" "!stop" "!iorg" "@lib.css" "!latex" "!pdf" "!psh" "!login"
   *PRG *Search "!download" )

## ** Load and Set

(scl 2)

(load
   "@lib/http.l" "@lib/xhtml.l" "@lib/form.l" "@lib/tex.l" "@lib/adm.l"
   "@lib/socialshareprivacy/lib.l" 
   "@lib/boss.l" )

## FIXME during development, set session timeout to 4h
(patch login `(* 3600 1000) `(* 4 3600 1000))

(setq
   *Scl 2
   *Pool "db/iorg/"
   *Blob "db/iorg/blob/"
   *Jnl (and (info "key/iorg") "fifo/iorg")
   *Salt (16 . "$6$@1$")
   *Css '("@lib.css" "iorg/css/lib.css")
   *IorgCss '("iorg/css/worg.css" "iorg/css/iorg.css")
   *IorgPort 5001 )

(load "iorg/er.l" "iorg/lib.l" "iorg/gui.l")

(permission
   OrgData     "Edit Org Element Objects"
   Iorg        "Edit Iorg Wiki Documents"
   System      "Edit System Data"
   Upload      "File upload"
   RoleAdmin   "Role Administration"
   UserAdmin   "User Administration"
   Password    "Password"
   Delete      "Delete" )

(mime "d" NIL 120)

## ** Entry point

## FIXME: account for possible "../../"
## Return absolute path to local (running) PicoLisp installation
## Usage example: (absolutePath "@lib/misc.l")
(de absolutePath (P)
   (split
      (or
         (pre? "/" (path P))
         (pack (pwd) "/" (path P)) ) ) )

(de startEmacsServer ()
   (call 'emacs
      "-Q" "--daemon=iorg-server"
      "-L" (pack (absolutePath "@iorg/el/")) )
   (call 'emacsclient "-s" "iorg-server" "-e" "(require 'iorg-init)" ) )

(de killEmacsServer ()
   (call 'emacsclient "-s" "iorg-server" "-e" "(kill-emacs)") )

(de main ()
   (call 'mkdir "-p" *Pool *Blob)
   (pool *Pool *Dbs *Jnl)
   (unless (seq *DB)
      (load "iorg/init.l") )
   (unless (db 'nm '+Doc "home")
      (msg "No home document") )
   (if (db 'nm '+Doc "menu")
      (setq *IorgMenu (id @ T))
      (msg "No menu document") )
   (start-emacs-server) )

(de go ()
   (pw 12)
   (rollback)
   (server *IorgPort "!start") )

## iorg/main.l ends here
