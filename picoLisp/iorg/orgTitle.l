## * iorg/orgTitle.l --- iOrg GUI for OrgHeadline
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

(must "OrgTitle" OrgData)

## ** GUI Function

## (rel :title (+Ref +String))
## (rel :timestamp (+Ref +Link) NIL (+OrgTimestamp))

(menu ,"OrgTitle"
   (idForm ,"OrgTitle" '(choOrgHeadline) ':level '+OrgTitle T '(may Delete)
      NIL
      ## '((: :label) " "
      ##   (let Split (split (chop (: :ptree :parse-tree-id)) "_")
      ##      (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      (<grid> 3
         ,"Raw Value" NIL
         (gui '(+View +TextField) '(rawValue> (: home obj)) 80)
         ,"Title" NIL
         (gui '(+E/R +TextField) '(:title : home obj) 60)
         ,"Date" NIL
         (gui '(+E/R +ObjView +TimeField)
            '(:timestamp : home obj)
            '(: :start-dat) 40 )
         ,"Time" NIL
         (gui '(+E/R +ObjView +TimeField)
            '(:timestamp : home obj)
            '(: :start-tim) 40 ) ) ) )

## iorg/orgTitle.l ends here
