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
         (gui '(+E/R +TextField)
            '(input-buffer : home obj) 20 )
         ,"Input File" NIL
         (gui '(+Chk +E/R +TextField)
            '(or
                (extra)
                (and (info (val> This))
                   "File does not exist" ) )
            '(input-file : home obj) 40 )
         ,"Creation-Date" NIL
         (gui '(+E/R +DateField)
            '(creation-date : home obj) 10 )
         ,"Creation-Time" NIL
         (gui '(+E/R +TimeField)
            '(creation-time : home obj) 8 ) )
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
         60 30 ,"Contents" ) ) )

## iorg/orgData.l ends here
