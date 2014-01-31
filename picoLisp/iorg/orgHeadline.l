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
   (idForm ,"OrgHeadline" '(choOrgHeadline) 'level
      '+OrgHeadline T '(may Delete)
      '("ID " (: elem-id)
          " Obj" This
          " [" (: ptree input-buffer)
          "<" (stamp
                 (: ptree creation-date)
                 (: ptree creation-time) ) ">]" )
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
        ##           (gui 1 '(+TextField) 25)
        ##           (gui 2 '(+TextField) 35)
        ##           (gui 3 '(+DelRowButton))
        ##           (gui 4 '(+BubbleButton)) ) ) )
        ##  (<spread> (scroll 6) (editButton T))
        ##  (----)

        (----)
        (gui '(+E/R +Fmt +Chart) '(title-stamp : home obj)
           list car 8 ) 
           (<grid> 5
              ,"Timestamp" (choOrgTimestamp 0) NIL NIL NIL
              ,"Type" NIL
              (gui 1 '(+SymField) 10) NIL NIL
              ,"Start (Date/Time)" NIL
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 8) NIL
              ,"End (Date/Time)" NIL
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 8) NIL
              ,"Repeater (Type/Value/Unit)" NIL
              (gui 6 '(+SymField) 10)
              (gui 7 '(+NumField) 8)
              (gui 8 '(+SymField) 8) )

        (----)

        (gui '(+E/R +Fmt +Chart) '(scheduled : home obj)
           list car 8 ) 
           (<grid> 5
              ,"Scheduled" (choOrgTimestamp 0) NIL NIL NIL
              ,"Type" NIL
              (gui 1 '(+SymField) 10) NIL NIL
              ,"Start (Date/Time)" NIL
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 8) NIL
              ,"End (Date/Time)" NIL
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 8) NIL
              ,"Repeater (Type/Value/Unit)" NIL
              (gui 6 '(+SymField) 10)
              (gui 7 '(+NumField) 8)
              (gui 8 '(+SymField) 8) )

        (----)

        (gui '(+E/R +Fmt +Chart) '(deadline : home obj)
           list car 8 ) 
           (<grid> 5
              ,"Deadline" (choOrgTimestamp 0) NIL NIL NIL
              ,"Type" NIL
              (gui 1 '(+SymField) 10) NIL NIL
              ,"Start (Date/Time)" NIL
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 8) NIL
              ,"End (Date/Time)" NIL
              (gui 4 '(+Lock +DateField) 10)
              (gui 5 '(+Lock +TimeField) 8) NIL
              ,"Repeater (Type/Value/Unit)" NIL
              (gui 6 '(+Lock +SymField) 10)
              (gui 7 '(+Lock +NumField) 8)
              (gui 8 '(+Lock +SymField) 8) )

        (----)

        (gui '(+E/R +Fmt +Chart) '(closed : home obj)
           list car 8 ) 
           (<grid> 5
              ,"Closed" (choOrgTimestamp 0) NIL NIL NIL
              ,"Type" NIL
              (gui 1 '(+SymField) 10) NIL NIL
              ,"Start (Date/Time)" NIL
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 8) NIL
              ,"End (Date/Time)" NIL
              (gui 4 '(+Lock +DateField) 10)
              (gui 5 '(+Lock +TimeField) 8) NIL
              ,"Repeater (Type/Value/Unit)" NIL
              (gui 6 '(+Lock +SymField) 10)
              (gui 7 '(+Lock +NumField) 8)
              (gui 8 '(+Lock +SymField) 8) )

      ## (--)
      ## (<grid> 4
      ##    ,"Scheduled (Date/Time)" (choOrgTimestamp 0) NIL NIL
      ##    ,"Start" NIL
      ##    (gui '(+E/R +ObjView +DateField)
      ##              '(scheduled-stamp : home obj)
      ##              '(: start-dat) 15 )
      ##    (gui '(+E/R +ObjView +TimeField)
      ##               '(scheduled-stamp : home obj)
      ##               '(: start-tim) 15 )
      ##    ,"End" NIL
      ##    (gui '(+E/R +ObjView +DateField)
      ##               '(scheduled-stamp : home obj)
      ##               '(: end-dat) 15 )
      ##    (gui '(+E/R +ObjView +TimeField)
      ##               '(scheduled-stamp : home obj)
      ##               '(: end-tim) 15 )
      ##    ,"Deadline (Date/Time)" (choOrgTimestamp 0)
      ##    (gui '(+E/R +ObjView +DateField)
      ##       '(deadline-stamp : home obj)
      ##       '(: start-dat) 15 )
      ##    (gui '(+E/R +ObjView +TimeField)
      ##       '(deadline-stamp : home obj)
      ##       '(: start-tim) 15 )
      ##    ,"Closed (Date/Time)" (choOrgTimestamp 0)
      ##    (gui '(+E/R +ObjView +DateField)
      ##       '(closed-stamp : home obj)
      ##       '(: start-dat) 15 )
      ##    (gui '(+E/R +ObjView +TimeField)
      ##       '(closed-stamp : home obj)
      ##       '(: start-tim) 15 ) )

       (----)
      (gui '(+E/R +Chart) '(properties : home obj) 4)
      (<table> NIL ,"Properties" '((NIL ,"Key") (NIL ,"Value"))
         (do 6
            (<row> NIL
               (gui 1 '(+TextField) 20)
               (gui 2 '(+TextField) 30)
               (gui 3 '(+DelRowButton))
               (gui 4 '(+BubbleButton)) ) ) )
      (<spread> (scroll 6) (editButton T))
      (----)

      (gui '(+E/R +Cue +TextField)
         '(section : home obj)
         "Lorem ipsum dolor sit amet"
         60 30 ,"Contents" )
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
