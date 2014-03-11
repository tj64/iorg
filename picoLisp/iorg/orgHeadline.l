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
## (rel archived-p (+Bool))
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
      (--)
      (<grid> 6
         ## ,"OrgData-ID" NIL
         ## (gui '(+Chk +E/R +Obj +NumField)
         ##    '(unless (val> This)
         ##        "Please provide an OrgData-ID" )
         ##    '(ptree : home obj ) '(: elem-id) 6 )
         ## ,"Ancestor-ID" NIL
         ## (gui '(+Chk +E/R +Obj +NumField)
         ##    '(unless (val> This)
         ##        "Please provide an Ancestor-ID" )
         ##    '(ancestor : home obj) '(: elem-id) 6 ) )
         ,"OrgData-ID" NIL
         (gui '(+E/R +ObjView +NumField)
            '(ptree : home obj ) '(: elem-id) )
         ,"Ancestor-ID" NIL
         (gui '(+E/R +ObjView +NumField)
            '(ancestor : home obj) '(: elem-id) ) )
      (--)
      (<grid> 9
         ,"Level" NIL
         (gui '(+Chk +E/R +Map +NumField)
            '(unless (val> This)
                "Please provide a headline level")
            '(level : home obj)
            '(("-" . NIL) ("1" . 1) ("2" . 2) ("3" . 3) ("4" . 4)
              ("5" . 5)( "6" . 6) ("7" . 7) ("8" . 8))
            '("-" "1" "2" "3" "4"  "5" "6" "7" "8") )
         ,"Priority" NIL
         (gui '(+E/R +Fmt +Map +TextField)
            '(priority : home obj)
            '((C) (and C (char C)))
            '((C) (and C (char C)))
            '(("-" . NIL))
            '("A" "B" "C") )
         ,"Todo" NIL
         (gui '(+E/R +Map +TextField)
            '(todo-keyword : home obj)
            '(("---" . NIL))
            '("TODO" "DONE" "NEXT" "WAITING") ) )
      (<grid> 7
         ,"Category" NIL
         (gui '(+E/R +TextField)
            '(category : home obj) 15) NIL
         ,"Author" NIL
         (gui 'author '(+E/R +TextField)
            '(author : home obj) 23 )
         ,"Title" NIL
         (gui '(+E/R +TextField)
            '(title-string : home obj) 50) - - - -
         ,"Alt-Title" NIL
         (gui '(+E/R +TextField)
            '(alt-title-string : home obj) 50) - - - -
         ,"Tags" NIL
         (gui '(+E/R +Fmt +TextField)
            '(tags : home obj)
            '((Tags) (and Tags (glue " " Tags)))
            '((Strg) (and Strg
                        (filter bool
                           (mapcar pack
                              (split (chop Strg) " ") ) ) ) )
            50 ) - - - -)
        (----)
      (<table> NIL ,"Planning" NIL
         (<row> NIL ,"Titlestamp")
         (<row> NIL
           (prog
              (gui '(+E/R +Fmt +Chart) '(title-stamp : home obj)
                 list car 8 ) 
              (gui 1 '(+Map +TextField)
                 *TimestampType
                 (mapcar car *TimestampType) ) 
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 5)
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 5)
              (gui 6 '(+Map +TextField)
                 *RepeaterType
                 (mapcar car *RepeaterType) )
              (gui 7 '(+NumField) 3)
              (gui 8 '(+Map +TextField)
                 *RepeaterUnit
                 (mapcar car *RepeaterUnit) ) ) )
         (<row> NIL ,"Scheduled")
         (<row> NIL
           (prog
              (gui '(+E/R +Fmt +Chart) '(scheduled : home obj)
                 list car 8 )
              (gui 1 '(+Map +TextField)
                 *TimestampType
                 (mapcar car *TimestampType) )
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 5)
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 5)
              (gui 6 '(+Map +TextField)
                 *RepeaterType
                 (mapcar car *RepeaterType) )
              (gui 7 '(+NumField) 3)
              (gui 8 '(+Map +TextField)
                 *RepeaterUnit
                 (mapcar car *RepeaterUnit) ) ) )
         (<row> NIL ,"Deadline")
         (<row> NIL
           (prog
              (gui '(+E/R +Fmt +Chart) '(deadline : home obj)
                 list car 8 ) 
              (gui 1 '(+Map +TextField)
                 *TimestampType
                 (mapcar car *TimestampType) )
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 5)
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 5)
              (gui 6 '(+Map +TextField)
                 *RepeaterType
                 (mapcar car *RepeaterType) )
              (gui 7 '(+NumField) 3)
              (gui 8 '(+Map +TextField)
                 *RepeaterUnit
                 (mapcar car *RepeaterUnit) ) ) )
         (<row> NIL ,"Closed")
         (<row> NIL
           (prog
              (gui '(+E/R +Fmt +Chart) '(closed : home obj)
                 list car 8 ) 
              (gui 1 '(+Map +TextField)
                 *TimestampType
                 (mapcar car *TimestampType) )
              (gui 2 '(+DateField) 10)
              (gui 3 '(+TimeField) 5)
              (gui 4 '(+DateField) 10)
              (gui 5 '(+TimeField) 5)
              (gui 6 '(+Map +TextField)
                 *RepeaterType
                 (mapcar car *RepeaterType) )
              (gui 7 '(+NumField) 3)
              (gui 8 '(+Map +TextField)
                 *RepeaterUnit
                 (mapcar car *RepeaterUnit) ) ) ) )
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
      (--)
      (<grid> 3
        ,"Archived?" NIL
         (gui 'archived-p '(+E/R +Checkbox)
               '(archived-p : home obj) ) 
        ,"Footnote-Section?" NIL
         (gui 'footnote-section-p '(+E/R +Checkbox)
               '(footnote-section-p : home obj) ) )
      (--)
      (<grid> 5
        ,"Creation-Date" NIL
        (gui 'creation-date '(+E/R +ObjView +DateField)
               '(creation-date : home obj) ) NIL 
         (gui 'creation-time '(+E/R +ObjView +TimeField)
               '(creation-time : home obj) ) )
 ) )

## iorg/orgHeadline.l ends here