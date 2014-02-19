## * iorg/gui.l --- Start-page for iOrg
## ** MetaData 
##   :PROPERTIES:
##   :copyright: Alexander_Burger Thorsten_Jolitz
##   :copyright-since: 2011
##   :version:  0.9
##   :licence:  GPL3+
##   :licence-url: http://www.gnu.org/licenses/
##   :authors: Alexander_Burger Thorsten_Jolitz
##   :contact: <abu@software-lab.de> <tjolitz@gmail.com>
##   :git-repo: https://github.com/tj64/iorg
##   :git-clone: git@github.com:tj64/iorg.git
##   :keywords: emacs org-mode picolisp wiki
##   :END:

## ** Comments

## * Code

## ** Menu 

(de menu (Ttl . Prg)
   (action
      ## (html 0 Ttl *Css NIL
      (html 0 Ttl *IorgCss NIL
         (<layout>
            ((180 0 'menu)
               (<div> @
                  (expires)
                  (<menu>
                     ("iOrg"
                        (if *Login
                           (baseHRef NIL *SesId "!iorg?home")
                           (baseHRef *Port1 "!iorg?home") ) )
                     ("Home" "!start")
                     ("logout" (and *Login "!stop"))
                     (NIL (<hr>))
                     ("Documents" (and (may Iorg) "iorg/doc.l"))
                     ("Org Data" (and (may OrgData) "iorg/orgData.l"))
                     ("Org Headlines" (and (may OrgData) "iorg/orgHeadline.l"))
                     ("Role Administration" (and (may RoleAdmin) "iorg/role.l"))
                     ("User Administration" (and (may Iorg) "iorg/user.l"))
                     ("Upload" (and (may Upload) "iorg/upload.l"))
                     ("Help" (and (may Iorg) "iorg/help.l")) ) )
               ((NIL NIL 'main)
                  (<div> @ (run Prg 1)) ) ) ) ) ) )


## ** Entry Point

(de start ()
   (setq *Url "!start")
   (and (app) (setq *Menu 3))
   (menu "iOrg"
      ## (<h2> NIL "iOrg")
      (<img> "iorg/img/iorg.png" "iOrg Logo")
      (----)
      (loginForm) ) )

(de stop ()
   (logout)
   (start) )

(de iorg (Nm)
   (if (db 'nm '+Doc (fold Nm))
      (html> @)
      (http404) ) )


## ** Choose Dialogs

(de choUser (Dst)
   (diaform '(Dst)
      (<grid> "--.-."
         "Login Name" (gui 'nm '(+Focus +Var +TextField) '*UserNm 10)
         "Full Name" (gui 'name '(+Var +TextField) '*UserName 30)
         (searchButton '(init> (: home query)))
         "Created <=" (gui 'ca '(+Var +DateField) '*UserCa 10)
         "EMail" (gui 'em '(+Var +TextField) '*UserEm 30)
         (resetButton '(nm name em ca new query))
         "New" (gui 'new '(+Var +Checkbox) '*UserNew) )
      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @Nm *UserNm
               @Name *UserName
               @Em *UserEm
               @Ca (and *UserCa (cons @))
               @New *UserNew
               (select (@@)
                  ((nm +User @Nm) (name +User @Name)
                     (em +User @Em) (ca +User @Ca) (new +User @New) )
                  (head @Nm @@ nm)
                  (tolr @Name @@ name)
                  (fold @Em @@ em)
                  (range @Ca @@ ca)
                  (bool @New @@ new) ) ) )
         6
         '((This) (list This (: nm) This (: em) (: ca) (and (: new) "NEW"))) )
      (<table> 'chart (choTtl "Users" 'nm '+User)
         (quote
            (btn)
            (NIL "Login")
            (NIL "Full Name")
            (NIL "EMail")
            (NIL "Created") )
         (do (cho)
            (<row> (alternating)
               (gui 1 '(+DstButton) Dst)
               (gui 2 '(+TextField))
               (gui 3 '(+ObjView +TextField) '(: name))
               (gui 4 '(+MailField))
               (gui 5 '(+DateField))
               (gui 6 '(+TextField)) ) ) )
      (<spread>
         (scroll (cho))
         (newButton T Dst '(+User) 'nm *UserNm 'name *UserName)
         (cancelButton) ) ) )

(de choRole (Dst)
   (choDlg Dst "Roles" '(nm +Role)) )

(de choDoc (Dst)
   (diaform '(Dst)
      (gui 'nm '(+Focus +Var +TextField) '*DocNm 30 "Name")
      (searchButton '(init> (: home query)))
      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @D (cons T)
               @Nm *DocNm
               (select (@@)
                  ((d +Mup @D doc) (nm +Doc @Nm))
                  (range @D @@ mup d)
                  (head @Nm @@ nm) ) ) )
         7
         '((This)
            (list This This
               (: mup d) (: mup t) (: mup u)
               (: mup sum) (and (: mup min) "Minor edit") ) ) )
      (<table> 'chart (choTtl "Documents" 'nm '+Doc)
         '((btn) (NIL "Name") (NIL "Changed") NIL (NIL "by") (NIL "Summary"))
         (do (cho)
            (<row> (alternating)
               (gui 1 '(+DstButton) Dst)
               (gui 2 '(+ObjView +TextField) '(: nm))
               (gui 3 '(+DateField))
               (gui 4 '(+TimeField))
               (gui 5 '(+ObjView +TextField) '(: nm))
               (gui 6 '(+TextField))
               (gui 7 '(+TextField)) ) ) )
      (<spread>
         (scroll (cho))
         (newButton T Dst '(+Doc))
         (cancelButton) ) ) )

(de choOrgData (Dst)
   (diaform '(Dst)
      (<grid> 5
         ## "--."
         (searchButton '(init> (: home query)))NIL
         (resetButton '(input-buffer author query)) NIL NIL
         ,"Input Buffer" NIL
         (gui 'input-buffer '(+Focus +Var +TextField) '*OrgDataInputBuffer 20) NIL NIL
         ,"Author" NIL
         (gui 'author '(+Var +TextField) '*OrgDataAuthor 20) NIL NIL
         ,"Creation-Range" NIL
         (gui 'creation-date-start '(+Var +DateField) '*OrgDataCreationDateStart 10) NIL
         (gui 'creation-date-end '(+Var +DateField) '*OrgDataCreationDateEnd 10) )
      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @InputBuffer *OrgDataInputBuffer
               @Author *OrgDataAuthor
               @OrgDatRng (cons *OrgDataCreationDateStart
                             (or *OrgDataCreationDateEnd T) )
               ## @CreationDateStart *OrgDataCreationDateStart
               ## @CreationDateEnd *OrgDataCreationDateEnd
               (select (@@)
                  ((input-buffer +OrgData @InputBuffer)
                   (author +OrgData @Author)
                   (creation-date +Cls @OrgDatRng) )
                   ## (creation-date +OrgData @CreationDateStart)
                   ## (creation-date +OrgData @CreationDateEnd)
                  (part @InputBuffer @@ input-buffer)
                  (tolr @Author @@ author)
                  (range @OrgDatRng @@ creation-date) ) ) )
         6
         '((This)
            (list
               This
               (: input-buffer)
               (: author)
               (: creation-date)
               (: creation-time) ) ) )
               ## (: description) ) ) )
               ## (and tags (pack ":" (glue ":" (: tags)) ":")) ) ) )
      (<table> 'chart (choTtl ,"OrgData" 'nr '+OrgData)
         '((btn) (NIL ,"Input Buffer") (NIL ,"Author") (NIL ,"Date")
           (NIL ,"Time") )
           ## (NIL ,"Time") (NIL ,"Description") )
         (do (cho)
            (<row> (alternating)
               (gui 1 '(+DstButton) Dst)
               (gui 2 '(+TextField))
               (gui 3 '(+TextField))
               (gui 4 '(+DateField))
               (gui 5 '(+TimeField))
               (gui 6 '(+TextField)) ) ) )
      (<spread>
         (scroll (cho))
         (newButton T Dst '(+OrgData))
         (cancelButton) ) ) )

(de choOrgHeadline (Dst)
   (diaform '(Dst)
      (<grid> 3
         ## "--."
         (searchButton '(init> (: home query))) NIL
         (resetButton '(level todo-keyword query)) )
      (--)
      (<grid> 3
        ,"Category" NIL
         (gui 'category '(+Var +TextField)
            '*OrgHeadlineCategory 20 ) )
      (--)
      (<grid> 11
         ,"Level (L)" NIL
         ## (gui 'level '(+Focus +Var +Init +Map +TextField)
         ##    '*OrgHeadlineLevel
         ##    "1"
         (gui 'level '(+Focus +Var +Map +TextField)
            '*OrgHeadlineLevel
            '(("1" . 1) ("2" . 2) ("3" . 3) ("4" . 4)
              ("5" . 5)( "6" . 6) ("7" . 7) ("8" . 8))
            '("1" "2" "3" "4"  "5" "6" "7" "8") ) NIL
         (gui 'level= '(+Var +Init +TextField)
            '*OrgHeadlineLevel=
            ">="
            '("==" ">=" "<=") ) NIL
         ,"Priority (P)" NIL
         (gui 'priority '(+Var +Map +TextField)
            '*OrgHeadlinePriority
            '(NIL
                ("A" . `(char "A"))
                ("B" . `(char "B"))
                ("C" . `(char "C")) )
            '("A" "B" "C") ) NIL
         (gui 'priority= '(+Var +Init +TextField)
            '*OrgHeadlinePriority=
            ">="
            '("==" ">=" "<=") ) )
      (<grid> 5
         ,"Todo" NIL
         (gui 'todo-keyword '(+Var +TextField)
            '*OrgHeadlineTodo
            '("TODO" "DONE" "NEXT" "WAITING") ) NIL
         (gui 'todo-keyword= '(+Var +Init +TextField)
            '*OrgHeadlineTodo=
            "=="
            '("==" ">=" "<=") ) )
      (--)
      (<grid> 3
        ,"Tags" NIL
         (gui 'tags '(+Var +TextField)
            '*OrgHeadlineTags 40 ) )
      (--)
      (<table>
         NIL ,"Planning" '(NIL (NIL ,"Start") (NIL ,"End") NIL)
         (<row> NIL
            ## (<grid> 7
            ## NIL NIL ,"Start" NIL ,"End" NIL NIL
            ,"Timestamp" 
            (gui 'title-stamp-start '(+Var +Able +DateField)
               '*OrgHeadlineTitleStampStart
               '(not (= *OrgHeadlineTitleStamp= "<="))
               10 ) 
            (gui 'title-stamp-end '(+Var +Able +DateField)
               '*OrgHeadlineTitleStampEnd 
               '(not (= *OrgHeadlineTitleStamp= ">="))
               10 ) 
            ## (gui 'title-stamp= '(+Upd +Var +Init +TextField)
               ## '(prog
               ##     (upd> (: gui 'title-stamp-start))
               ##     (upd> (: gui 'title-stamp-end)) )
            (gui 'title-stamp= '(+Var +Init +TextField)
               '*OrgHeadlineTitleStamp=
               "=="
               '("==" ">=" "<=") ) )
         (<row> NIL
            ,"Scheduled" 
            (gui 'scheduled-start '(+Var +Able +DateField)
               '*OrgHeadlineScheduledStart 
               '(not (= *OrgHeadlineTitleStamp= "<="))
               10 )
            (gui 'scheduled-end '(+Var +Able +DateField)
               '*OrgHeadlineScheduledEnd 
               '(not (= *OrgHeadlineTitleStamp= ">="))
               10 )
            (gui 'scheduled= '(+Var +Init +TextField)
               '*OrgHeadlineScheduled=
               "=="
               '("==" ">=" "<=") ) )
         (<row> NIL
            ,"Deadline"
            (gui 'deadline-start '(+Var +Able +DateField)
               '*OrgHeadlineDeadlineStart
               '(not (= *OrgHeadlineTitleStamp= "<="))
               10 )
            (gui 'deadline-end '(+Var +Able +DateField)
               '*OrgHeadlineDeadlineEnd 
               '(not (= *OrgHeadlineTitleStamp= ">="))
               10 )
            (gui 'deadline= '(+Var +Init +TextField)
               '*OrgHeadlineDeadline=
               "=="
               '("==" ">=" "<=") ) )
         (<row> NIL
            ,"Closed"
            (gui 'closed-start '(+Var +Able +DateField)
               '*OrgHeadlineClosedStart 
               '(not (= *OrgHeadlineTitleStamp= "<="))
               10 )
            (gui 'closed-end '(+Var +Able +DateField)
               '*OrgHeadlineClosedEnd 
               '(not (= *OrgHeadlineTitleStamp= ">="))
               10 )
            (gui 'closed= '(+Var +Init +TextField)
               '*OrgHeadlineClosed=
               "=="
               '("==" ">=" "<=") ) ) )
      (--)
      (gui 'properties '(+Var +Chart) '*OrgHeadlineProperties 4)
      (<table> NIL ,"Properties" '((NIL ,"Key") (NIL ,"Value"))
         (do 6
            (<row> NIL
               (gui 1 '(+TextField) 20)
               (gui 2 '(+TextField) 20)
               (gui 3 '(+DelRowButton))
               (gui 4 '(+BubbleButton)) ) ) )
      (scroll 6)
      ## (<spread> (scroll 6) (editButton T))

        ## ,"Properties Key:" NIL
        ##  (gui 'property-key '(+Var +TextField)
        ##     '*OrgHeadlinePropertyKey 20 )
        ## ,"Val:" NIL
        ##  (gui 'property-val '(+Var +TextField)
        ##     '*OrgHeadlinePropertyVal 20 ) )
      (--)
      (<grid> 3
         ,"Author" NIL
         (gui `(: ptree author) '(+Var +TextField)
            '*OrgHeadlineAuthor 30 ) )
      (--)
      (<grid> 7
        ,"Creation-Date" NIL
            (gui 'creation-date-start '(+Var +Able +DateField)
               '*OrgHeadlineCreationDateStart 
               '(not (= *OrgHeadlineTitleStamp= "<="))
               10 ) NIL
            (gui 'creation-date-end '(+Var +Able +DateField)
               '*OrgHeadlineCreationDateEnd 
               '(not (= *OrgHeadlineTitleStamp= ">="))
               10 ) NIL
            (gui 'creation-date= '(+Var +Init +TextField)
               '*OrgHeadlineCreationDate=
               "=="
               '("==" ">=" "<=") ) )
         ## (gui 'creation-date '(+Var +DateField)
         ##    '*OrgHeadlineCreationDate 10 ) NIL
         ## (gui 'creation-date= '(+Var +Init +TextField)
         ##    '*OrgHeadlineCreationDate=
         ##    "=="
         ##    '("==" ">=" "<=") ) )
      (--)
      (<grid> 7
        ,"Archived?" NIL
         (gui 'archivedp '(+Var +Checkbox)
            '*OrgHeadlineArchivedP ) NIL
        ,"Footnote-Section?" NIL
         (gui 'footnote-section-p '(+Var +Checkbox)
             '*OrgHeadlineFootnoteSectionP ) )            
      (<grid> 3
         (searchButton '(init> (: home query))) NIL
         (resetButton '(level todo-keyword query)) )

      (gui 'query '(+QueryChart) (cho)
         '(goal
            (quote
               @Level
               (case *OrgHeadlineLevel=
                  ("=="
                   (and
                      *OrgHeadlineLevel
                      (cons
                         *OrgHeadlineLevel
                         *OrgHeadlineLevel ) ) )
                  ("<="
                   (and
                      *OrgHeadlineLevel
                      (cons NIL *OrgHeadlineLevel ) ) )
                  (">="
                   (and
                      *OrgHeadlineLevel
                      (cons *OrgHeadlineLevel T) ) ) )
               @Todo *OrgHeadlineTodo
               ## (case *OrgHeadlineTodo=
               ##    ("=="
               ##     (and
               ##        *OrgHeadlineTodo
               ##        (cons
               ##           *OrgHeadlineTodo
               ##           *OrgHeadlineTodo ) ) )
               ##    ("<="
               ##     (and
               ##        *OrgHeadlineTodo
               ##        (cons NIL *OrgHeadlineTodo ) ) )
               ##    (">="
               ##     (and
               ##        *OrgHeadlineTodo
               ##        (cons *OrgHeadlineTodo T) ) ) )
               @Priority 
               (case *OrgHeadlinePriority=
                  ("=="
                   (and
                      *OrgHeadlinePriority
                      (cons
                         *OrgHeadlinePriority
                         *OrgHeadlinePriority ) ) )
                  ("<="
                   (and
                      *OrgHeadlinePriority
                      (cons `(char "A") *OrgHeadlinePriority ) ) )
                  (">="
                   (and
                      *OrgHeadlinePriority
                      (cons *OrgHeadlinePriority `(char "C")) ) ) )
               @Tags *OrgHeadlineTags
               @TitleStampRng
               (case *OrgHeadlineTitleStamp=
                  ("=="
                   (cond
                      ((and
                         *OrgHeadlineTitleStampStart
                         *OrgHeadlineTitleStampEnd)
                         (cons
                            *OrgHeadlineTitleStampStart
                            *OrgHeadlineTitleStampEnd ) )
                      (*OrgHeadlineTitleStampStart
                         (cons
                            *OrgHeadlineTitleStampStart
                            *OrgHeadlineTitleStampStart ) )
                      (*OrgHeadlineTitleStampEnd
                         (cons
                            *OrgHeadlineTitleStampEnd
                            *OrgHeadlineTitleStampEnd ) )
                      (T (cons NIL T)) ) )
                  ("<="
                   (cond
                      ((and
                         *OrgHeadlineTitleStampStart
                         *OrgHeadlineTitleStampEnd)
                         (cons 0 *OrgHeadlineTitleStampEnd ) )
                      (*OrgHeadlineTitleStampStart
                         (cons 0 *OrgHeadlineTitleStampStart ) )
                      (*OrgHeadlineTitleStampEnd
                         (cons 0 *OrgHeadlineTitleStampEnd ) )
                      (T (cons NIL T)) ) )
                  (">="
                   (cond
                      ((and
                         *OrgHeadlineTitleStampStart
                         *OrgHeadlineTitleStampEnd)
                         (cons *OrgHeadlineTitleStampEnd T) )
                      (*OrgHeadlineTitleStampStart
                         (cons *OrgHeadlineTitleStampStart T) )
                      (*OrgHeadlineTitleStampEnd
                         (cons *OrgHeadlineTitleStampEnd T) )
                      (T (cons NIL T)) ) ) )
               @ScheduledRng 
               (case *OrgHeadlineScheduled=
                  ("=="
                   (cond
                      ((and
                         *OrgHeadlineScheduledStart
                         *OrgHeadlineScheduledEnd)
                         (cons
                            *OrgHeadlineScheduledStart
                            *OrgHeadlineScheduledEnd ) )
                      (*OrgHeadlineScheduledStart
                         (cons
                            *OrgHeadlineScheduledStart
                            *OrgHeadlineScheduledStart ) )
                      (*OrgHeadlineScheduledEnd
                         (cons
                            *OrgHeadlineScheduledEnd
                            *OrgHeadlineScheduledEnd ) )
                      (T (cons NIL T)) ) )
                  ("<="
                   (cond
                      ((and
                         *OrgHeadlineScheduledStart
                         *OrgHeadlineScheduledEnd)
                         (cons 0 *OrgHeadlineScheduledEnd ) )
                      (*OrgHeadlineScheduledStart
                         (cons 0 *OrgHeadlineScheduledStart ) )
                      (*OrgHeadlineScheduledEnd
                         (cons 0 *OrgHeadlineScheduledEnd ) )
                      (T (cons NIL T)) ) )
                  (">="
                   (cond
                      ((and
                         *OrgHeadlineScheduledStart
                         *OrgHeadlineScheduledEnd)
                         (cons *OrgHeadlineScheduledEnd T) )
                      (*OrgHeadlineScheduledStart
                         (cons *OrgHeadlineScheduledStart T) )
                      (*OrgHeadlineScheduledEnd
                         (cons *OrgHeadlineScheduledEnd T) )
                      (T (cons NIL T)) ) ) )
               @Deadline 
               (case *OrgHeadlineDeadline=
                  ("=="
                   (cond
                      ((and
                         *OrgHeadlineDeadlineStart
                         *OrgHeadlineDeadlineEnd)
                         (cons
                            *OrgHeadlineDeadlineStart
                            *OrgHeadlineDeadlineEnd ) )
                      (*OrgHeadlineDeadlineStart
                         (cons
                            *OrgHeadlineDeadlineStart
                            *OrgHeadlineDeadlineStart ) )
                      (*OrgHeadlineDeadlineEnd
                         (cons
                            *OrgHeadlineDeadlineEnd
                            *OrgHeadlineDeadlineEnd ) )
                      (T (cons NIL T)) ) )
                  ("<="
                   (cond
                      ((and
                         *OrgHeadlineDeadlineStart
                         *OrgHeadlineDeadlineEnd)
                         (cons 0 *OrgHeadlineDeadlineEnd ) )
                      (*OrgHeadlineDeadlineStart
                         (cons 0 *OrgHeadlineDeadlineStart ) )
                      (*OrgHeadlineDeadlineEnd
                         (cons 0 *OrgHeadlineDeadlineEnd ) )
                      (T (cons NIL T)) ) )
                  (">="
                   (cond
                      ((and
                         *OrgHeadlineDeadlineStart
                         *OrgHeadlineDeadlineEnd)
                         (cons *OrgHeadlineDeadlineEnd T) )
                      (*OrgHeadlineDeadlineStart
                         (cons *OrgHeadlineDeadlineStart T) )
                      (*OrgHeadlineDeadlineEnd
                         (cons *OrgHeadlineDeadlineEnd T) )
                      (T (cons NIL T)) ) ) )
               @Closed
               (case *OrgHeadlineClosed=
                  ("=="
                   (cond
                      ((and
                         *OrgHeadlineClosedStart
                         *OrgHeadlineClosedEnd)
                         (cons
                            *OrgHeadlineClosedStart
                            *OrgHeadlineClosedEnd ) )
                      (*OrgHeadlineClosedStart
                         (cons
                            *OrgHeadlineClosedStart
                            *OrgHeadlineClosedStart ) )
                      (*OrgHeadlineClosedEnd
                         (cons
                            *OrgHeadlineClosedEnd
                            *OrgHeadlineClosedEnd ) )
                      (T (cons NIL T)) ) )
                  ("<="
                   (cond
                      ((and
                         *OrgHeadlineClosedStart
                         *OrgHeadlineClosedEnd)
                         (cons 0 *OrgHeadlineClosedEnd ) )
                      (*OrgHeadlineClosedStart
                         (cons 0 *OrgHeadlineClosedStart ) )
                      (*OrgHeadlineClosedEnd
                         (cons 0 *OrgHeadlineClosedEnd ) )
                      (T (cons NIL T)) ) )
                  (">="
                   (cond
                      ((and
                         *OrgHeadlineClosedStart
                         *OrgHeadlineClosedEnd)
                         (cons *OrgHeadlineClosedEnd T) )
                      (*OrgHeadlineClosedStart
                         (cons *OrgHeadlineClosedStart T) )
                      (*OrgHeadlineClosedEnd
                         (cons *OrgHeadlineClosedEnd T) )
                      (T (cons NIL T)) ) ) )
               (select (@@)
                  ((level +OrgHeadline @Level)
                   (todo-keyword +OrgHeadline @Todo)
                   (priority +OrgHeadline @Priority)
                   (title-stamp +Cls @TimestmpRng)
                   (scheduled +Cls @ScheduledRng)
                   )
                  (range @Level @@ level)
                  (head @Todo @@ todo-keyword)
                  (range @Priority @@ priority)
                  (range @TitleStampRng @@ title-stamp)
                  (range @ScheduledRng @@ scheduled)
                  (range @Deadline @@ deadline)
                  (range @Closed @@ closed) ) ) )
                  ## (head @Todo @@ todo) ) ) )
         6
         '((This)
            (list
               This
               (: level)
               (let P (: priority)
                  (and P (char P)) )
               (: todo-keyword)
               (: category)
               (: title-string)
               (let Tgs (: tags)
                  (and Tgs (pack ":" (glue ":" Tgs) ":")) ) ) ) )
      (<table> 'chart (choTtl ,"Headlines" 'nr '+OrgHeadline)
         '((btn) (NIL ,"L") (NIL ,"P") (NIL ,"Todo") (NIL ,"Category") (NIL ,"Title" ) (NIL ,"Tags") )
         (do (cho)
            (<row> (alternating)
               (gui 1 '(+DstButton) Dst)
               (gui 2 '(+NumField))
               (gui 3 '(+TextField))
               (gui 4 '(+TextField))
               (gui 5 '(+TextField))
               (gui 6 '(+TextField)) ) ) )
      (<spread>
         (scroll (cho))
         (newButton T Dst '(+OrgHeadline))
         (cancelButton) ) ) )

(de choAgenda ()
   )


## (de choOrgTitle ()
##    )

## (de choOrgTimestamp ()
##    )

## (de choCategory ()
##    )


## ** Show document HTML page

(dm (html> . +Doc) ()
   (and (== 'login *ID) (app))
   (setq *Url (ext.html This))
   (if *Post
      (cookie 'search *Search)
      (setq *Search (get *Cookies 'search)) )
   (unless *SesId (socialshareprivacy.js))
   (action
      (html NIL (pack "iOrg: " (: nm)) *IorgCss NIL
         (<div> '(id . menu)
            (and (num? *ID) (tell @ 'logout))
            (and *SesId (expires))
            (<img> "iorg/img/wiki.png" "iOrg-Wiki Logo")
            (<div> '(id . searchGrp)  # jk
               (<post> 'search *Url
                  (<js>
                     (quote  # Emulate '+hint' field
                        ("autocomplete" . "off")
                        ("onfocus" . "doHint(this)")
                        ("onkeyup" . "return hintKey(this,event,true)") )
                     (<style> '(id . "search")
                        (<field> 24 '*Search) ) )
                  (<hidden> '*PRG T)
                  ## (<submit> "Search") )
                  (<submit> "Search" "*Gui:1:") )
               (and *Search (searchIorg (splitWords (chop @)) 24)) )
            (--)
            (ifn *Login
               (<this> '*ID 'login "Log in")
               (<href> "Log out"
                  (mkUrl (list (srcUrl *Url) '*ID *Pid)) )
               (--)
               (<$> "My account" *Login) )
            (----)
            ## render menu file
            (when *IorgMenu
               (let F (blob (; (id (car @) (cdr @)) mup) 'txt)
                  (renderTxtFile F) ) ) )
         (<div> '(id . main)
            (cond
               ((== 'login *ID)
                (form 'login
                   (<grid> 2
                      "Name" (gui 'nm '(+Focus +TextField) 20)
                      "Password" (gui 'pw '(+PwField) 20) )
                   (--)
                   (gui '(+Button) "OK"
                      '(ifn (login (val> (: home nm)) (val> (: home pw)))
                          (error "Permission denied")
                          (put!> *Login 'new NIL)
                          (clr> (: home pw))
                          (off *ID) ) )
                   (gui '(+Button) "Cancel" '(off *ID))
                   (<nbsp> 7)
                   (gui '(+Tiny +Button) "Create account"
                      '(dialog NIL
                          (<h4> NIL "Create account")
                          (<br> "Please supply at least Login Name and EMail Address.")
                          (<grid> 2
                             "Login Name" (gui '(+Focus +Chk +TextField)
                                             '(let Nm (val> This)
                                                 (cond
                                                    ((not Nm) "Please supply a login name")
                                                    ((db 'nm '+User Nm) "This name is already in use") ) )
                                             10 )
                             "EMail Address" (gui '(+Chk +TextField)
                                                '(and
                                                    *Btn
                                                    (not (val> This))
                                                    "Please supply an email address" )
                                                30 )
                             "Full Name (optional)" (gui '(+TextField) 30) )
                          (<br>)
                          (<br> "Clicking \"OK\" will create a new account,")
                          (<br> "and send a password to you by email.")
                          (<br>)
                          (okButton
                             ## '(with
                            '(let Pw (fmt64 (in "/dev/urandom" (rd 6)))
                               (with
                                 (new! '(+User)
                                    'nm (val> (field -3))
                                    ## 'pw (fmt64 (in "/dev/urandom" (rd 6)))
                                    'pw (passwd Pw)
                                    'role (db 'nm '+Role "Member")
                                    'em (val> (field -2))
                                    'name (val> (field -1)) )
                                 (mail "localhost" 25
                                    "noreply@picolisp.com" (: em)
                                    "New Account" NIL
                                    "A new account on picolisp.com was created for you."
                                    NIL
                                    (prinl "You can now log in as: " (: nm))
                                 ##    (prinl "With the password: " (: pw)) )
                                 ## (off *ID) ) )
                                    (prinl "With the password: " Pw) )
                                  (off *ID) ) ) )
                          (cancelButton) ) ) ) )
               ((== 'edit *ID)
                (let Obj This
                   (ifn (lock Obj)
                      (form 'edit
                         (=: obj Obj)
                         (<h3> NIL (ht:Prin (: obj nm)))
                         (docFields T) )
                      (<h2> 'red
                         (prinl
                            "This document is currently edited by user '"
                            (cdr (lup *Users @))
                            "'" ) ) ) ) )
               ((== 'rpl *ID)
                (let Org This
                   (form 'edit
                      (=: org Org)
                      (<h3> NIL (ht:Prin "Reply"))
                      (docFields T) ) ) )
               ((str? *ID)
                (if (fetch '(nm . +Doc) @)
                   (setq This @)  # Created meanwhile
                   (form 'edit
                      (=: docNm *ID)
                      (<h2> NIL "New document '" *ID "'")
                      (<p> NIL "This document doesn't exist yet")
                      (gui '(+Button) "Create document"
                         '(let Doc (new! '(+Doc) 'nm (fold (: home docNm) 0))
                             (setq *ID 'edit)
                             (url (ext.html Doc)) ) )
                      (gui '(+Button) "Cancel" '(off *ID)) ) ) )
               ((may Iorg)
                (<spread>
                   (<this> '*ID 'edit "Edit")
                   (and (: mup) (<this> '*ID 'rpl "Reply")) )
                (--) ) )
            (cond
               ## process uploaded testfiles (??)
               (*MupTxt
                  (let TextMup (tmp "TxtMup")
                     (out TextMup (prinl @))
                     (pipe
                        (oxRender TextMup)
                        (processInternalLinks) )
                     (off *MupTxt *MupImg) ) )
               ## process DB files
               ((: mup)
                (let F (blob @ 'txt)
                   (renderTxtFile F) )
                ## privacy
                (if *SesId
                   (----)
                   (socialshareprivacy) )
                ## export
                (<small>
                   (with (: mup)
                      (<spread>
                         (<tip> "Public URL of this page"
                            (let U (baseHRef *Port1 "!iorg?" (: doc nm))
                               (<href> U U) ) )
                         (<tip> "Download TeX source"
                            (<href> "TeX" (pack "!latex?" (ht:Fmt This))) )
                         (<nbsp> 3)
                         (<tip> "Download PDF document"
                            (<href> "PDF" (pack "!pdf?" (ht:Fmt This))) )
                         (<nbsp> 3)
                         (prin (datSym (: d)))
                         (<nbsp> 3)
                         (prog
                            (while (: min)
                               (setq This (: <)) )
                            (<$> (: u nm) (: u)) ) ) ) )
                ## history
                (and *SesId (<$> "Revision History" This))
                ## replies
                (when (: mup rpl)
                   (<hr>)
                   (<h4> NIL "Replies")
                   (recur ()
                      (<ul> NIL
                         (for This (: mup rpl)
                            (<li> NIL
                               (<href> (nm> (: mup)) (ext.html This))
                               (<nbsp>)
                               (ht:Prin (: mup sum))
                               (and (: mup rpl) (recurse)) ) ) ) ) ) ) ) ) ) )
   (off *ID) )

## render Mup (text) file
(de renderTxtFile (TxtFile)
   (let (FileNameSansExt
         (text
            (car
               (tail 2
                  (split
                     (chop TxtFile) '"/" '"." ) ) ) )
         AbsPath (pack (pwd) "/" TxtFile)
         HtmlFile
         (pack "iorg/html/" FileNameSansExt ".html")
         Render?
         (or
            (not (info HtmlFile))
            (<
               (stamp
                  (cadr (info HtmlFile))
                  (cddr (info HtmlFile)) )
               (stamp
                  (cadr (info TxtFile))
                  (cddr (info TxtFile)) ) ) ) )
      ## only render again if .txt is newer than cached .html
      (and
         Render?
         (out HtmlFile (oxRender AbsPath)) )
      (pipe
         (! in HtmlFile (echo))
         (processInternalLinks) ) ) )



## ** Full-text search

(de searchIorg (Words Cnt)
   (<strong> NIL (<br> "Search results:"))
   (let Q
      (goal
         (cons
            (cons
               'select
               '(@@)
               (mapcar
                  '((W) (list 'mup '+Doc W))
                  Words )
               (mapcar
                  '((W)
                     (cons '^
                        (fill
                           '(@ (fetch '(mup . +Doc) (cons W (-> @@))))
                           'W ) ) )
                  Words ) ) ) )
               ## (mapcar
               ##    '((W) (fill '(@ fetch '(mup . +Doc) (cons W (-> @@))) 'W))
               ##    Words ) ) ) )
      (loop
         (NIL (prove Q))
         (T (lt0 (dec 'Cnt)) (<br> "(More results)"))
         (with (; @ @@)
            (<br>
               (<href>
                  (or (title> (: mup)) (: nm))
                  (pack "!iorg?" (: nm)) ) ) ) ) ) )

(de docFields (Flg)
   (unless *PRG
      (=: mup
         (list
            (and (: obj mup) (in (blob @ 'txt) (till NIL T)))  # txt
            NIL  # sum
            NIL  # min
            (: obj mup bin) ) ) )  # bin
   (gui '(+Var +TextField) (: mup) 80 25)
   (--)
   (gui '(+Var +TextField) (: mup -1) 40 "Summary")
   (<nbsp> 3)
   (gui '(+Var +Checkbox) (: mup -2) "This is a minor edit")
   (<hr>)
   (gui '(+Var +Chart1) (: mup -3) 3)
   (<grid> 2
      (<table> 'chart NIL '((NIL "Media"))
         (do 3
            (<row> (alternating)
               (gui 1 '(+ObjView +TextField) '(: nm))  # Binary name
               (gui 2 '(+DelRowButton))
               (gui 3 '(+BubbleButton)) ) )
         (<row> NIL (scroll 3 T)) )
      (<grid> ".-"
         (<strong> "Specify a name") (gui 'nm '(+TextField) 30)
         (gui '(+Button) "Fetch from page"
            '(let Nm (val> (: home nm))
               (nond
                  (Nm (error "Please specify a name"))
                  ((db 'nm '+Doc (val> (field 1))) (error "Page not found"))
                  ((findBin Nm (; @ mup bin)) (error "Name not found in page"))
                  (NIL (push (: home mup -3) @)) ) ) )
         (gui '(+Val +TextField) fold 30)
         (gui '(+Drop +Button) '(field 1) "Upload directly"
            '(nond
               ((: drop) (error "Please select or drag&drop a file"))
               ((val> (: home nm)) (error "Please specify a name"))
               (NIL
                  (blob!
                     (push (: home mup -3) (new! '(+Bin) 'nm @))
                     'd
                     (: drop) ) ) ) )
         (gui '(+UpField) 30) ) )
   (<hr>)
   (<spread>
      (gui '(+Rid +Button) "Preview"
         '(setq *MupTxt (: home mup 1)  *MupImg (: home mup 4)) )
      (when Flg
         (gui '(+Button) "Cancel"
            '(prog (rollback) (off *ID)) )
         (saveButton
            '(let Obj (: home obj)
               (when (: home org)  # Reply
                  (dbSync)
                  (put> (: home org mup) 'rpl
                     (append
                        (: home org mup rpl)
                        (cons (setq Obj (new (db: +Doc) '(+Doc)))) ) )
                  (commit 'upd) )
               (apply 'newMup> (: home mup) Obj)
               (off *ID)
               (url (ext.html (or (: home org) Obj))) ) ) ) ) )

(de findBin (Nm Lst)
   (setq Nm (fold Nm))
   (find
      '((This) (= Nm (fold (: nm))))
      Lst ) )

## ** Process Internal Links

(de iorgLink (Lst)
   (let (Nm (pack (car Lst))  Str (or (glue " " (cdr Lst)) (pack (car Lst))))
      (cond
         ((fetch '(nm . +Doc) (fold Nm 0))
            (<href> Str (pack "!iorg?" Nm)) )
         (*Login (<this1> '*ID Nm Str))
         (T (prin Str)) ) ) )

## iorg/gui.l ends here
