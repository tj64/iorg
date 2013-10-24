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

## * Permissions

(must "OrgData" OrgData)

## ** GUI Function

## (rel :descendants (+List +Joint) :ancestor (+orgElement))
## (rel :label (+Ref +Symbol))
## (rel :contents (+Any))

## (rel :parse-tree-id (+Need +Sn +Idx +String))
## (rel :input-file (+Need +Ref +String))
## (rel :author (+Ref +String))
## (rel :email (+Ref +String))
## (rel :creator (+Sn +Idx +String))
## (rel :description (+Sn +Idx +String))


(menu ,"OrgData"
   (idForm ,"OrgData" '(choOrgData) ':level '+OrgData T '(may Delete)
      '((: :label) " "
        (let Split (split (chop (: :parse-tree-id)) "_")
           (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      (<grid> 3
         ,"Parse Tree ID" NIL
         (gui '(+View +TextField) '(:parse-tree-id : home obj) 40)
         ,"Input File" NIL
         (gui '(+View +FileField) '(:input-file : home obj) 60)
         ,"Author" NIL
         (gui '(+View +TextField) '(:author : home obj) 40)
         ,"Email" NIL
         (gui '(+View +MailField) '(:email : home obj) 40)
         ,"Creator" NIL
         (gui '(+View +TextField) '(:creator : home obj) 60)
         (--)
         ,"Description" NIL
         (gui '(+View +TextField) '(:description : home obj) 60 20) )
         (----) ) )

## iorg/orgData.l ends here
