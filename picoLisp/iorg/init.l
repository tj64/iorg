## * iorg/init.l --- Init-file for iOrg
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

## * Wiki

## ** Create the first documents in the root DB file

(new T '(+Doc) 'nm "home")
(new T '(+Doc) 'nm "menu")
(new T '(+Doc) 'nm "help")
(new T '(+Doc) 'nm "intro")
(new T '(+Doc) 'nm "assumpt")
(new T '(+Doc) 'nm "webUI")
(new T '(+Doc) 'nm "emacsUI")
(new T '(+Doc) 'nm "picoDB")
(new T '(+Doc) 'nm "dhtmlMup")
(commit)

## ** Create two roles, one for admins and one for normal members

(obj ((+Role) nm "Administrator") perm `*Perms)
(obj ((+Role) nm "Member") perm (Iorg OrgData))
(commit)

## ** Create the admin and member users

(obj
   ((+User) nm "admin")
   pw `(passwd "admin")
   role `(db 'nm '+Role "Administrator") )

(obj
   ((+User) nm "member")
   pw `(passwd "member")
   role `(db 'nm '+Role "Member") )

(commit)

## ** Create some initial markup texts

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial start page"
      txt "iorg/txt/home.org" )
   'doc
   (db 'nm '+Doc "home") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial menu"
      txt "iorg/txt/menu.org" )
   'doc
   (db 'nm '+Doc "menu") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Help text"
      txt "iorg/txt/help.org" )
   'doc
   (db 'nm '+Doc "help") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Introduction"
      txt "iorg/txt/intro.org" )
   'doc
   (db 'nm '+Doc "intro") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Assumptions"
      txt "iorg/txt/assumpt.org" )
   'doc
   (db 'nm '+Doc "assumpt") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Web UI"
      txt "iorg/txt/webUI.org" )
   'doc
   (db 'nm '+Doc "webUI") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Emacs UI"
      txt "iorg/txt/emacsUI.org" )
   'doc
   (db 'nm '+Doc "emacsUI") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "PicoLisp DB"
      txt "iorg/txt/picoDB.org" )
   'doc
   (db 'nm '+Doc "picoDB") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Markup for DHTML"
      txt "iorg/txt/dhtmlMup.org" )
   'doc
   (db 'nm '+Doc "dhtmlMup") )


(commit)


## iorg/init.l ends here

