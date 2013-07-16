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

## * Code

## Create the first three documents in the root DB file
(new T '(+Doc) 'nm "home")
(new T '(+Doc) 'nm "menu")
(new T '(+Doc) 'nm "help")
(commit)

## Create two roles, one for admins and one for normal members
(obj ((+Role) nm "Administrator") perm `*Perms)
(obj ((+Role) nm "Member") perm (Iorg))
(commit)

## Create the admin user
(obj ((+User) nm "admin") pw `(passwd "admin") role `(db 'nm '+Role "Administrator"))
(commit)

## Create some initial markup texts
(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial start page"
      txt "iorg/txt/home.txt" )
   'doc
   (db 'nm '+Doc "home") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Initial menu"
      txt "iorg/txt/menu.txt" )
   'doc
   (db 'nm '+Doc "menu") )

(put>
   (obj ((+Mup))
      u `(db 'nm '+User "admin")
      d `(date)
      t `(time)
      sum "Help text"
      txt "iorg/txt/help.txt" )
   'doc
   (db 'nm '+Doc "help") )

(commit)

## iorg/init.l ends here
