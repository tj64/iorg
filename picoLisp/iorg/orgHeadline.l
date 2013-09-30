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
         ,"Number" (gui 'level '(+Focus +Var +NumField) '*OrgHeadlineLevel 3)
         (searchButton '(init> (: home query)))
         ,"Todo"
         (gui 'todo '(+Var +Hint1 +Uppc +TextField) '*OrgHeadlineTodo
            '("TODO" "DONE" "NEXT" "WAITING")
            30 )
         (resetButton '(level todo query)) )
      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @Level (and *OrgHeadlineLevel (cons @ T))
               @Todo *OrgHeadlineTodo
               (select (@@)
                  ((level +OrgHeadline @Level) (todo +OrgHeadline @Todo))
                  (range @Level @@ level)
                  (head @Todo @@ todo) ) ) )
         4
         '((This)
            (list
               This
               (last (: CATEGORY))
               (: title 1)
               (pack ":" (glue ":" (: tags)) ":") ) ) )
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

(menu ,"OrgHeadline"
   (idForm ,"OrgHeadline" '(choOrgHeadline) 'level '+OrgHeadline T '(may Delete)
      '((glue ", " (: title)))
      (<grid> 2
        ,"Level" (gui '(+E/R +NumField) '(level : home obj) 1)
        ,"Title" (gui '(+E/R +Cue +TextField) '(title : home obj) ,"Title" 40) )
      (<spread> NIL (editButton T)) ) )

## iorg/orgHeadline.l ends here
