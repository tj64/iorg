## * iorg/orgData.l --- iOrg GUI for OrgHeadline
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

## (msg "enter orgData")
(must "OrgData" OrgData)

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

(menu ,"OrgData"
   (idForm ,"OrgData" '(choOrgData) 'input-buffer
      '+OrgData T '(may Delete)
      '("ID " (: elem-id)
          " Obj" This )
      (<grid> 3
         ,"Input Buffer" NIL
         (gui '(+Chk +E/R +TextField)
            '(unless (val> This)
                ,"Please provide a name" )
            '(input-buffer : home obj) 20 )
         ,"Input File" NIL
         ## (gui '(+Chk +E/R +TextField)
            ## '(or
            ##     (extra)
            ##     (unless (info (val> This))
            ##        "File does not exist" ) )
         (gui '(+E/R +TextField)
            '(input-file : home obj) 50 )
         ,"Creation-Date" NIL
         (gui '(+E/R +DateField)
            '(creation-date : home obj) 10 )
         ,"Creation-Time" NIL
         (gui '(+E/R +TimeField)
            '(creation-time : home obj) 5 ) )
      (--)
      (<grid> 3
         ,"Author" NIL
         (gui '(+E/R +TextField)
            '(author : home obj) 40 )
         ,"Email" NIL
         (gui '(+E/R +MailField)
            '(email : home obj) 40 )
         ,"Creator" NIL
         (gui '(+E/R +TextField)
            '(creator : home obj) 40 )
         ,"Description" NIL
         (gui '(+E/R +TextField)
            '(description : home obj) 40 3 ) )
      (----)
      (gui '(+E/R +TextField)
         '(section : home obj)
        60 10 ,"Contents" )
      (--)
      (gui '(+E/R +Chart) '(descendants : home obj) 8
         '((This)
            (list
               This
               (and (: priority) (char @))
               (: todo-keyword)
               (: category)
               (: title-string)
               (and (: tags) (pack ":" (glue ":" @) ":")) ) )
         car )
      (<table> NIL ,"Direct Descendants"
         '((NIL ,"*") (NIL ,"[#]") (NIL ,"Todo") (NIL ,"Category")
           (NIL ,"Title") (NIL ,"Tags"))
         (do 6
            (<row> (alternating)
               (gui 1 '(+ObjView +NumField) '(: level))
               (gui 2 '(+TextField))
               (gui 3 '(+TextField))
               (gui 4 '(+TextField))
               (gui 5 '(+TextField))
               (gui 6 '(+TextField))
               (gui 7 '(+DelRowButton))
               (gui 8 '(+BubbleButton)) ) ) )
      (scroll 6)
      (--)
      (gui '(+QueryChart) 12
         '(goal '(@OrgDat (: home obj)
                    (db ptree +OrgHeadline @OrgDat @@) ) )
         6
         '((This)
           (list
              This
              (and (: priority) (char @))
              (: todo-keyword)
              (: category)
              (: title-string)
              (and (: tags) (pack ":" (glue ":" @) ":")) ) ) )
      (<table> NIL ,"All Children"
         '((NIL ,"*") (NIL ,"[#]") (NIL ,"Todo") (NIL ,"Category")
           (NIL ,"Title") (NIL ,"Tags") )
         (do 12
            (<row> (alternating)
               (gui 1 '(+ObjView +NumField) '(: level))
               (gui 2 '(+TextField))
               (gui 3 '(+TextField))
               (gui 4 '(+TextField))
               (gui 5 '(+TextField))
               (gui 6 '(+TextField)) ) ) )
      (scroll 6)
      (--)
 ) )

## iorg/orgData.l ends here
