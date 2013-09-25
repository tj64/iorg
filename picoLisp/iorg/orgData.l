## * iorg/orgData.l --- iOrg GUI for OrgData
## ** MetaData 
##   :PROPERTIES:
##   :copyright: Thorsten_Jolitz
##   :copyright-since: 2013
##   :version:  0.9
##   :licence:  GPL3+
##   :licence-url: http://www.gnu.org/licenses/
##   :authors: Thorsten_Jolitz
##   :contact: <tjolitz@gmail.com>
##   :git-repo: https://github.com/tj64/iorg
##   :git-clone: git@github.com:tj64/iorg.git
##   :keywords: emacs org-mode picolisp wiki
##   :END:

## ** Comments

## * Code

## ** Permissions

(must "OrgData" OrgData)
## (must "OrgData" Iorg)

## ** Functions

(menu ,"OrgData"
   (idForm ,"OrgData" ,"OrgDatas" 'nm '+OrgData
      T '(may Delete) '((: nm))
      (<grid> 2

        ,"Raw Value" (gui '(+E/R +Cue +TextField)
                        '(raw-value : home obj) ,"Raw V" )
         ,"X" (gui '(+E/R +TextField) '(x : home obj) 40)
         ,"X" (gui '(+E/R +TextField) '(x : home obj) 40)
         ,"X" (gui '(+E/R +TextField) '(x : home obj) 40)

         ,"XXXX" (gui '(+E/R +NumField) '(xx : home obj) 40) )
      (<spread> NIL (editButton T)) ) )

## (menu "Headline"
##    (idForm "User" '(choUser) 'nm '+User
##       '(or (may UserAdmin) (== *Login (: home obj)))
##       '(or (may Delete) (== *Login (: home obj)))
##       '((: nm))
##       (<grid> 2
##          "Login Name" (gui '(+E/R +Cue +TextField) '(nm : home obj) "User" 30)
##          "Password"
##          (gui '(+Able +E/R +Fmt +TextField)
##             '(or (may Password) (== *Login (: home obj)))
##             '(pw : home obj)
##             '((V) (and V "****"))
##             '((V)
##                (if (= V "****")
##                   (: home obj pw 0)
##                   (passwd V (: home obj pw 0)) ) )
##             30 )
##          "Role"
##          (gui '(+Able +E/R +Obj +TextField)
##             '(may RoleAdmin)
##             '(role : home obj)
##             '(nm +Role)
##             T )
##          "Full Name" (gui '(+E/R +TextField) '(name : home obj) 40)
##          "EMail" (gui '(+E/R +MailField) '(em : home obj) 40)
##          "Picture"
##          (prog
##             (gui '(+Able +UpField) '(not (: home obj jpg)) 30)
##             (gui '(+Drop +Button) '(field -1)
##                '(if (: home obj jpg) "Uninstall" "Install")
##                '(cond
##                   ((: home obj jpg)
##                      (ask "Uninstall Picture?"
##                         (put!> (: home top 1 obj) 'jpg NIL) ) )
##                   ((: drop) (blob! (: home obj) 'jpg @)) ) ) )
##          NIL
##          (gui '(+Upd +Img) 
##             '(and (: home obj jpg) (allow (blob (: home obj) 'jpg)))
##             "Picture" )
##          "Created" (gui '(+Able +E/R +DateField) '(may System) '(ca : home obj) 10)
##          "New" (gui '(+Able +E/R +Checkbox) '(may System) '(new : home obj)) )
##       (gui '(+QueryChart) 12
##          '(goal
##             (quote
##                @User (list (cons (: home obj) T) (: home obj))
##                (db u +Mup @User @@) ) )
##          3
##          '((This) (list This (: t) (: sum))) )
##       (<table> 'chart "Postings"
##          '((em7 "Date") (em7 "Time") (NIL "Summary"))
##          (do 12
##             (<row> (alternating)
##                (gui 1 '(+ObjView +DateField) '(: d))
##                (gui 2 '(+TimeField))
##                (gui 3 '(+TextField)) ) ) )
##       (scroll 12)
##       (<spread> NIL
##          (editButton '(or (may UserAdmin) (== *Login (: home obj)))) ) ) )

## iorg/orgData.l ends here
