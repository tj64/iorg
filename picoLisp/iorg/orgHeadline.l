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
   (idForm ,"OrgHeadline" '(choOrgHeadline) 'level '+OrgHeadline T '(may Delete)
      '((: ptree input-buffer) "<" (stamp (: ptree date) (: ptree time)) ">")
        ## (let Split (split (chop (: :ptree :parse-tree-id)) "_")
        ##    (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      ## (msg "before grid 3")
      (<grid> 9
         ## (msg "before Level")
         ,"Level" NIL
         (gui '(+E/R +NumField) '(level : home obj) 1)
         ## (msg "before Priority")
         ,"Priority" NIL
         (gui '(+E/R +Fmt +TextField)
            '(priority : home obj)
            char char
            '("A" "B" "C") )
         ## (msg "before Todo")
         ,"Todo" NIL
         (gui '(+E/R +TextField)
            '(todo-keyword : home obj)
            '("TODO" "DONE" "NEXT" "WAITING") ) )
      (<grid> 3
         ## (msg "before Category")
         ,"Category" (choCategory 0)
         (gui '(+E/R +TextField)
            '(category : home obj) 15)
         ## (msg "before Title")
         ,"Title" (choOrgTitle 0)
         (gui '(+E/R +TextField)
            '(title-string : home obj) 50)
         ## (msg "before Tags")
         ,"Tags" NIL
         (gui '(+E/R +Fmt +TextField)
            '(tags : home obj)
            '((Tags) (and Tags (pack ":" (glue ":" Tags) ":")))
            '((Strg) (and Strg
                        (filter bool
                           (mapcar pack
                              (split (chop Strg) ":") ) ) ) )
            25 ) )
      ## (msg "after grid")

        ## (----)
        ##  (gui '(+E/R +Chart) '(title-stamp : home obj) 9)
        ##  (<table> NIL ,"Timestamp" '((NIL ,"Key") (NIL ,"Value"))
        ##     (do 2
        ##        (<row> NIL
        ##           (gui 1 '(+TextField) 30)
        ##           (gui 2 '(+TextField) 40)
        ##           (gui 3 '(+DelRowButton))
        ##           (gui 4 '(+BubbleButton)) ) ) )
        ##  (<spread> (scroll 6) (editButton T))
        ##  (----)

         ## NIL NIL
         ## (gui '(+E/R +Fmt +Field)
         ##    '(
         ##    40 )

     ##  (--)
     ##  (<grid> 4
     ##     ,"Scheduled (Date/Time)" (choOrgTimestamp 0) NIL NIL
     ##     ,"Start" NIL
     ##     (gui '(+E/R +ObjView +DateField)
     ##               '(:scheduled-stamp : home obj)
     ##               '(: :start-dat) 15 )
     ##     (gui '(+E/R +ObjView +TimeField)
     ##                '(:scheduled-stamp : home obj)
     ##                '(: :start-tim) 15 )
     ##     ,"End" NIL
     ##     (gui '(+E/R +ObjView +DateField)
     ##                '(:scheduled-stamp : home obj)
     ##                '(: :end-dat) 15 )
     ##     (gui '(+E/R +ObjView +TimeField)
     ##                '(:scheduled-stamp : home obj)
     ##                '(: :end-tim) 15 )
     ##     ,"Deadline (Date/Time)" (choOrgTimestamp 0)
     ##     (gui '(+E/R +ObjView +DateField)
     ##        '(:deadline-stamp : home obj)
     ##        '(: :start-dat) 10 )
     ##     (gui '(+E/R +ObjView +TimeField)
     ##        '(:deadline-stamp : home obj)
     ##        '(: :start-tim) 15 )
     ##     ,"Closed (Date/Time)" (choOrgTimestamp 0)
     ##     (gui '(+E/R +ObjView +DateField)
3     ##        '(:closed-stamp : home obj)
     ##        '(: :start-dat) 10 )
     ##     (gui '(+E/R +ObjView +TimeField)
     ##        '(:closed-stamp : home obj)
     ##        '(: :start-tim) 15 ) )
     ##   (----)
     ##  (gui '(+E/R +Chart) '(:properties : home obj) 4)
     ##  (<table> NIL ,"Properties" '((NIL ,"Key") (NIL ,"Value"))
     ##     (do 6
     ##        (<row> NIL
     ##           (gui 1 '(+TextField) 30)
     ##           (gui 2 '(+TextField) 40)
     ##           (gui 3 '(+DelRowButton))
     ##           (gui 4 '(+BubbleButton)) ) ) )
     ##  (<spread> (scroll 6) (editButton T))
      (----)
      (gui '(+E/R +Cue +TextField)
         '(section : home obj)
         "Lorem ipsum dolor sit amet"
         80 40 ,"Contents" )
     ##  (----)
     ##  (<grid> 12
     ## ,"Quoted" NIL
     ##  (gui  '(+E/R +Field +Checkbox)
     ##     '(:quotedp : home obj) )
     ## ,"Archived" NIL
     ##  (gui  '(+E/R +Field +Checkbox)
     ##     '(:archivedp : home obj) )
     ## ,"Commented" NIL
     ##  (gui  '(+E/R +Field +Checkbox)
     ##     '(:commentedp : home obj) )
     ## ,"Footnote Section" NIL
     ##  (gui  '(+E/R +Field +Checkbox)
     ##     '(:footnote-section-p : home obj) )
 ) ) #)

## iorg/orgHeadline.l ends here
