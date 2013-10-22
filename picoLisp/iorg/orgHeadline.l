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

## * Functions
## ** Choose Function

# Eventuell nach "gui.l" schieben
(de choOrgHeadline (Dst)
   (diaform '(Dst)
      (<grid> "--."
         ,"Number" (gui ':level '(+Focus +Var +NumField) '*OrgHeadline:Level 3)
         (searchButton '(init> (: home query)))
         ,"Todo"
         (gui 'todo '(+Var +TextField) '*OrgHeadlineTodo
            '("TODO" "DONE" "NEXT" "WAITING") )
         (resetButton '(:level todo query)) )
      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @Level (and *OrgHeadlineLevel (cons @ T))
               @Todo *OrgHeadlineTodo
               (select (@@)
                  ((:level +OrgHeadline @Level) (todo +OrgHeadline @Todo))
                  (range @Level @@ level)
                  (head @Todo @@ todo) ) ) )
         4
         '((This)
            (list
               This
               (pack (car (split (chop (: ptree parse-tree-id)) "_")))
               ## (last (getCategory> This))
               (car (: :title 2))
               (pack
                  (and :tags ":")
                  (glue ":" (: :tags))
                  (and :tags ":") ) ) ) )
      (<table> 'chart (choTtl ,"Headlines" 'nr '+OrgHeadline)
         '((btn) (NIL ,"Category") (NIL ,"Title") (NIL ,"Tags"))
         (do (cho)
            (<row> (alternating)
               (gui 1 '(+DstButton) Dst)
               (gui 2 '(+TextField))
               (gui 3 '(+TextField))
               (gui 4 '(+TextField)) ) ) )
      (<spread>
         (scroll (cho))
         (newButton T Dst '(+OrgHeadline))
         (cancelButton) ) ) )


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
        (let (Split (split (chop (: ptree parse-tree-id)) "_"))
           (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      (<grid> 2
        ,"Level" (gui '(+E/R +NumField)
                    '(:level : home obj) 1 )
        ,"Priority" (gui '(+E/R +Fmt +TextField)
                       '(:priority : home obj)
                        char char
                       '("A" "B" "C") )
        ,"Todo" (gui '(+E/R +TextField)
                   '(:todo-keyword : home obj)
                   '("TODO" "DONE" "NEXT" "WAITING") )
        ,"Title" (gui '(+View +TextField)
                    '(car (: home obj :title 2)) 30 )
        ,"Tags" (gui '(+View +TextField)
                   '(pack ":" (glue ":" (: home obj :tags)) ":") 30 )
        ,"Contents" (gui '(+View +Cue +TextField)
                       NIL "Lorem ipsum dolor sit amet" 50 20 ) )
      (<spread> NIL (editButton T)) ) )

## iorg/orgHeadline.l ends here
