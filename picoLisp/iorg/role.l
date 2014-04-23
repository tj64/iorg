## * iorg/role.l --- Roles for iOrg
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

(must "Role Administration" RoleAdmin)

## ** Functions

(menu "Role Administration"
   (idForm "Role" '(choRole) 'nm '+Role T '(may Delete) '((: nm))
      (gui '(+E/R +Cue +TextField) '(nm : home obj) "Role" 30 "Name")
      (<table> NIL NIL NIL
         (gui '(+E/R +Fmt +Chart)
            '(perm : home obj)
            '((Val) (mapcar '((S) (list S (memq S Val))) *Perms))
            '((Lst) (extract '((L P) (and (cadr L) P)) Lst *Perms))
            2 )
         (do (length *Perms)
            (<row> NIL
               (gui 1 '(+Set +TextField) '((Sym) (val (val Sym))))
               (gui 2 '(+Checkbox)) ) ) )
      (gui '(+/R +Chart) '(usr : home obj) 1 list)
      (<table> 'chart "User" NIL
         (do 8
            (<row> (alternating)
               (gui 1 '(+Obj +TextField) '(nm +User)) ) ) )
      (<spread> (scroll 8 T) (editButton T)) ) )

## iorg/role.l ends here
