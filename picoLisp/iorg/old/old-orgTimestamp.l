## * iorg/orgTimestamp.l --- GUI for iOrg timestamp objects
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

(must "OrgTimestamp" OrgData)

## ** GUI Function

## (class +OrgTimestamp +orgElement)
## (rel :type (+Ref +Symbol))
## (rel :year-start (+Any))
## (rel :month-start (+Any))
## (rel :day-start (+Any))
## (rel :hour-start (+Any))
## (rel :minute-start (+Any))
## (rel :year-end (+Any))
## (rel :month-end (+Any))
## (rel :day-end (+Any))
## (rel :hour-end (+Any))
## (rel :minute-end (+Any))
## (rel :start-dat (+Aux +Ref +Date) (:start-tim))
## (rel :start-tim (+Time))
## (rel :end-dat (+Aux +Ref +Date) (:end-tim))
## (rel :end-tim (+Time))
## (rel :repeater-type (+Symbol))
## (rel :repeater-value (+Number))
## (rel :repeater-unit (+Symbol))

(menu ,"OrgTimestamp"
   (idForm ,"OrgTimestamp" '(choOrgTimestamp)
      ':level '+OrgTimestamp T '(may Delete) NIL
      ## '((: :start-dat) " "
      ##   (let Split (split (chop (: :ptree :parse-tree-id)) "_")
      ##      (pack (car Split) ".org [" (pack (cdr Split)) "] ") ) )
      (<table> NIL ,"Overview"
         '((NIL ,"Stamp") NIL (NIL ,"Year") (NIL ,"Month") (NIL ,"Day")
           (NIL ,"Hour") (NIL ,"Minute") )
            (<row> NIL
               ,"Start" NIL
               (gui '(+View +NumField)
                  '(eval (: :year-start)) 8 )
               (gui '(+View +NumField)
                  '(eval (: :month-start)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :day-start)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :hour-start)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :minute-start)) 4 ) )
            (<row> NIL
               ,"End" NIL
               (gui '(+View +NumField)
                  '(eval (: :year-end)) 8 )
               (gui '(+View +NumField)
                  '(eval (: :month-end)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :day-end)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :hour-end)) 4 )
               (gui '(+View +NumField)
                  '(eval (: :minute-end)) 4 ) ) )
       (----)
      (<grid> 6
         NIL NIL 
         ,"Start Date" NIL
         (gui '(+E/R +DateField)
            '(:start-dat : home obj) 10 )
         ,"Start Time" NIL
         (gui '(+E/R +TimeField)
            '(:start-tim : home obj) 10)
         ,"End Date" NIL
         (gui '(+E/R +DateField)
            '(:end-dat : home obj) 10)
         ,"End Time" NIL
         (gui '(+E/R +TimeField)
            '(:end-tim : home obj) 10 ) ) ) )

         ## ,"Start Date" NIL
         ## (gui '(+E/R +Fmt +DateField)
         ##    '(:start-dat : home obj)
         ##    date stamp 30 )
         ## ,"Start Time" NIL
         ## (gui '(+E/R +Fmt +TimeField)
         ##    '(:start-tim : home obj)
         ##    date stamp 30 )
         ## ,"End Date" NIL
         ## (gui '(+E/R +Fmt +DateField)
         ##    '(:end-dat : home obj)
         ##    date stamp 30 )
         ## ,"End Time" NIL
         ## (gui '(+E/R +Fmt +TimeField)
         ##    '(:end-tim : home obj)
         ##    date stamp 30 ) ) ) )

## iorg/orgTimestamp.l ends here
