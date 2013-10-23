## * iorg/orgHeadline.l --- iOrg GUI for OrgHeadline
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

(must "OrgHeadline" OrgData)

## ** GUI Function

## (class +OrgHeadline +orgElement)
## (rel ptree (+Ref +Link) NIL (+OrgData))
## (rel title (+Any))
## (rel alt-title (+Any))
## (rel CATEGORY (+Any))
## (rel pre-blank (+Number))
## (rel hiddenp (+Symbol))
## (rel level (+Need +Ref +Number))
## (rel priority (+Ref +Number))
## (rel tags (+List +String))
## (rel todo-keyword (+Ref +String))
## (rel scheduled (+Any))
## (rel deadline (+Any))
## (rel closed (+Date))
## (rel quotedp (+Bool))
## (rel archivedp (+Bool))
## (rel commentedp (+Bool))
## (rel footnote-section-p (+Bool))

(menu ,"OrgHeadline"
   (idForm ,"OrgHeadline" '(choOrgHeadline) ':level '+OrgHeadline T '(may Delete)
      '((: label) " "
        (let Split (split (chop (: ptree parse-tree-id)) "_")
           (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      (<grid> 3
         ,"Level" NIL
         (gui '(+E/R +NumField) '(:level : home obj) 1)
         ,"Priority" NIL
         (gui '(+E/R +Fmt +TextField)
            '(:priority : home obj)
            char char
            '("A" "B" "C") )
         ,"Todo" NIL
         (gui '(+E/R +TextField)
            '(:todo-keyword : home obj)
            '("TODO" "DONE" "NEXT" "WAITING") )
         ,"Title" (choTitle 0)
         (gui '(+E/R +Obj +TextField)
            '(title-object : home obj)
            '(title +OrgTitle)
            30 )
         NIL NIL
         (gui '(+View +TextField)
            '(and (; (val> (field -1)) timestamp) (rawValue> @))
            30 )
         ,"Tags" NIL
         (gui '(+View +TextField)
            '(pack ":" (glue ":" (: home obj :tags)) ":")
            30 ) )
      (----)
      (gui '(+E/R +Chart) '(properties : home obj) 4)
      (<table> NIL ,"Properties" '((NIL ,"Key") (NIL ,"Value"))
         (do 6
            (<row> NIL
               (gui 1 '(+TextField) 30)
               (gui 2 '(+TextField) 50)
               (gui 3 '(+DelRowButton))
               (gui 4 '(+BubbleButton)) ) ) )
      (<spread> (scroll 6) (editButton T))
      (----)
      (gui '(+E/R +Cue +TextField)
         '(:contents : home obj)
         "Lorem ipsum dolor sit amet"
         80 40 ,"Contents" ) ) )

## iorg/orgHeadline.l ends here
