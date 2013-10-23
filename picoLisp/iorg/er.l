## * iorg/er.l --- Entity/Relations for iOrg
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

## *** Wiki class hierarchy

## #+begin_ascii
##   nm                      nm   pw  name
##    |                       |   |   |
## +--*--------+           +--*---*---*--+
## |           |           |             |
## |           | usr       |             *-- em
## |   Role    @-----------*    User     |
## |           |      role |             *-- jpg
## |           |           |             |
## +--------@--+           +--*---+---*--+
##          |                 |   |   |
##          perm             ca   |   new
##                                |
##                                |
##       nm                     u |   d   t                  nm
##       |                        |   |   |                  |
##    +--*------+              +--*---*---*--------+        +--*------+
##    |         | mup      doc |                   | bin    |         |
##    |         *--------------*                   @--------+   Bin   |
##    |   Doc   |              |        Mup        |        |         |
##    |         +--------------@                   *--+ >   +--*---*--+
##    |         |          rpl |                   |  |        |   |
##    +---------+              +--*---*---*-----*--+  |        d   mt
##                                |   |   |     |     |
##                              txt  sum  min   +-----+
##                                              <
## #+end_ascii

## * PicoLisp Wiki
## ** Markup index
(class +MupIdx +index)

(dm has> (Val Mup)
   (if (isa '+Mup Val)
      (extra Val Mup)
      (and (member Val (foldedWords Mup)) Mup) ) )

(dm rel> (Obj Old New Hook)
   (let
      (Tree (tree (: var) (: cls) (or Hook (get Obj (: hook))))
         A (foldedWords Old)
         B (foldedWords New) )
      (for W (diff A B)
         (store Tree (cons W Obj) NIL (: dbf)) )
      (for W (diff B A)
         (store Tree (cons W Obj) Obj (: dbf)) ) )
   (extra Obj Old New Hook) )

(dm lose> (Obj Val Hook)
   (let Tree (tree (: var) (: cls) (or Hook (get Obj (: hook))))
      (for W (foldedWords Val)
         (store Tree (cons W Obj) NIL (: dbf)) ) )
   (extra Obj Val Hook) )

(dm keep> (Obj Val Hook)
   (let Tree (tree (: var) (: cls) (or Hook (get Obj (: hook))))
      (for W (foldedWords Val)
         (store Tree (cons W Obj) Obj (: dbf)) ) )
   (extra Obj Val Hook) )

(de foldedWords (Mup)
   (when Mup
      (uniq
         (filter '((W) (>= (length W) 4))
            (splitWords (in (blob Mup 'txt) (till))) ) ) ) )


## ** Role/User
(extend +Role)

(dm url> (Tab)
   (and (may RoleAdmin) (list "iorg/role.l" '*ID This)) )


(extend +User)
(rel name (+Sn +Idx +String))          # Full Name
(rel em (+Fold +Ref +String))          # EMail
(rel jpg (+Blob))                      # Picture
(rel ca (+Ref +Date))                  # Created
(rel new (+Ref +Bool))                 # New account

(dm T @
   (pass super 'ca (date) 'new T) )

(dm url> (Tab)
   (and (may Iorg) (list "iorg/user.l" '*ID This)) )


## ** Document
(class +Doc +Entity)
(rel nm (+Key +String))                # Document title
(rel mup (+MupIdx +Joint)              # Current markup
   NIL
   doc (+Mup) )

(dm url> (Tab)
   (and (may Iorg) (list "iorg/doc.l" '*ID This)) )

(dm newMup> (Txt Sum Min Bin)
   (unless
      (and
         (: mup)
         (= Txt (in (blob @ 'txt) (till NIL T)))
         (= Bin (: mup bin)) )
      (dbSync)
      (protect
         (let Mup
            (new (db: +Mup) '(+Mup)
               'u *Login
               'd (date)
               't (time T)
               '< (: mup)
               'sum Sum
               'min Min
               'rpl (: mup rpl)
               'bin Bin )
            (put> Mup 'txt T)
            (out (blob Mup 'txt)
               (prin Txt) )
            (blob+ Mup 'txt)
            (put> Mup 'doc This) )
         (commit 'upd) ) ) )


## ** Markup
(class +Mup +Entity)
(rel doc (+Joint) mup (+Doc))          # Document
(rel u (+Aux +Ref +Link) (d t)         # User
   NIL (+User) )
(rel d (+Aux +Ref +Date) (t))          # Date
(rel t (+Time))                        # Time
(rel < (+Joint) > (+Mup))              # Previous
(rel > (+Joint) < (+Mup))              # Next
(rel txt (+Blob))                      # Text
(rel sum (+String))                    # Summary
(rel min (+Bool))                      # Minor edit
(rel rpl (+List +Link) (+Doc))         # Replies
(rel bin (+List +Link) (+Bin))         # Binaries

(dm url> (Tab)
   (and (may Iorg) (list "iorg/mup.l" '*ID This)) )

(dm nm> ()
   (pack "<" (: u nm) " " (datSym (: d)) " " (tim$ (: t)) ">") )

(dm diff> ()
   (and (may Iorg) (list "iorg/diff.l" '*ID This)) )


## ** Binary data
(class +Bin +Entity)
(rel nm (+String))                     # Name
(rel mt (+String))                     # Mime type
(rel d (+Blob))                        # Binary data

(dm upd> (X Old)
   (when (== X 'd)
      (put> This 'mt (mimetype (blob This 'd))) ) )


## * iOrg
## ** Org elements

## *** (Abstract) Org-Base-Element

## **** Comments

## - descendants :: joint link to descendant elements
## - label :: element ID (unique for parse-tree)
## - :contents :: normalized partial parse-tree

## **** Code

(class +orgBaseElement +Entity)
(rel :descendants (+List +Joint) ancestor (+orgElement))
(rel :label (+Ref +Symbol))
(rel :contents (+Any))

(dm url> ())
(dm upd> ())
(dm diff> ())
(dm new> ())
## return for relation Rel hashLbl number as string
(dm hashLblNum> (Rel)
   (pack
      (tail -1
         (head -1
            (chop
               (eval
                  (make (link ': Rel)) ) ) ) ) ) )

## ## prepare contents for `org-element-interpret-data'
## (dm prepCont> ()
##    (make
##       )

## *** (Abstract) Org-Element

## **** Comments

## - ancestor :: joint link to ancestor element
## - :3parent :: element/Object containing it
## - :post-blank :: number of trailing blank lines/spaces

## **** Code

(class +orgElement +orgBaseElement)
(rel :ancestor (+Joint) descendants (+orgBaseElement))
(rel :parent (+Ref +Symbol))
(rel :post-blank (+Number))

(dm interpret> ())

## *** Org-data

## **** Comments

## - ID :: (globally) unique parse-tree ID
## - input-file :: absolute path input-file
## - author :: author name
## - email :: authors email
## - creator :: version info for Emacs and Org-mode
## - description :: short description of org file

## **** Code

## Parse-Tree (meta data for containing org file,
## collected from Emacs environment, plus root elem-id)
(class +OrgData +orgBaseElement)
(rel :parse-tree-id (+Need +Sn +Idx +String))
(rel :input-file (+Need +Ref +String))
(rel :author (+Ref +String))
(rel :email (+Ref +String))
(rel :creator (+Sn +Idx +String))
(rel :description (+Sn +Idx +String))

(dm url> (Tab)
   (and (may OrgData) (list "iorg/orgData.l" '*ID This)) )

(dm upd> ())
(dm diff> ())
## ## return parse-tree in its (normalized) original form
## (dm origParseTree> ()
##    (msg "origParseTree> entered ...")
##    (make
##       (link
##          (: label)
##          (list
##             'org-data
##             (list
##                ':parse-tree-id (: parse-tree-id)
##                ':input-file (: input-file)
##                ':author (: author)
##                ':email (: email)
##                ':creator (: creator)
##                ':description (: description) )
##             ## (prepCont> (: contents))
##             (: contents)
##             (mapcar
##                'origSubTree>
##                (: descendants) ) ) ) ) )

## *** Headline

## **** Comments

## - ptree :: containing parse-tree object
## - :title :: parsed headline text with properties
## - :raw-value :: title string
## - title-object :: link to +OrgTitle object
## - :alt-title :: alternative title
## - properties :: list with links to +OrgProperty objects
## - category :: either name of input file or user attribute
## - :pre-blank :: number blank lines before content
## - :hiddpenp :: non-nil (symbol) if headline is hidden
## - :level :: reduced level of headline
## - :priority :: headlines priority as character
## - :tags :: headlines tags (without archive tag)
## - :todo-keyword :: todo keyword without quote/comment
## - :scheduled :: scheduled reference (timestamp or NIL)
## - scheduled-stamp :: link to +OrgTimestamp object
## - :deadline :: deadline reference (timestamp or NIL)
## - deadline-stamp :: link to +OrgTimestamp object
## - :closed :: closed reference (timestamp or NIL)
## - closed-stamp :: link to +OrgTimestamp object
## - :quotedp :: non-nil if headline contains quote keyword
## - :archivedp :: non-nil if headline has archived tag
## - :commentedp :: non-nil if headline has comment keyword
## - :footnote-section-p :: non-nil if headline is footnote-section

## **** Code

(class +OrgHeadline +orgElement)
(rel :ptree (+Aux +Ref +Link) (label) NIL (+OrgData))
(rel :title (+Any))
(rel :raw-value (+Any))
(rel :title-object (+Ref +Link) NIL (+OrgTitle))
## TODO deal with alt-title (just like with title?)
(rel :alt-title (+Any))
## (rel properties (+List +Ref +Link) NIL (+OrgProperty))
## (put> Obj 'properties
##    (make (while .. (link (list Key Val)))) )
(rel :properties (+List +Bag)
   ((+Ref +String))              # Key
   ((+String)) )                 # Value
## (rel timestamps (+List +Ref +Link) NIL (+OrgTimestamp))
## (rel CATEGORY (+Any))
(rel :category (+Ref +String))
(rel :pre-blank (+Number))
(rel :hiddenp (+Symbol))
(rel :level (+Need +Ref +Number))
(rel :priority (+Ref +Number))
(rel :tags (+List +Ref +String))
## (rel raw-tags (+String))
(rel :todo-keyword (+Ref +String))
(rel :scheduled (+Any))
(rel :scheduled-stamp (+Ref +Link) NIL (+OrgTimestamp))
(rel :deadline (+Any))
(rel :deadline-stamp (+Ref +Link) NIL (+OrgTimestamp))
(rel :closed (+Any))
(rel :closed-stamp (+Ref +Link) NIL (+OrgTimestamp))
(rel :quotedp (+Bool))
(rel :archivedp (+Symbol))
(rel :commentedp (+Bool))
(rel :footnote-section-p (+Bool))


(dm url> (Tab)
   (and (may OrgData) (list "iorg/orgHeadline.l" '*ID This)) )

## (dm category> ()
##    (if (lst? (: CATEGORY))
##       (last (: CATEGORY)) )
##      (category> (db 'CATEGORY '+OrgHeadline
##                    (makeHashRef (hashLblNum>
##                                    (car (: CATEGORY)) ) )
##                                    'ptree (: ptree) ) ) )

(dm upd> ())
(dm diff> ())
## (dm origSubTree> ()
##    (msg "origSubTree> entered ...")
##    (make
##       (link
##          (: label)
##          (list
##             'headline
##             (list
##                ':title (: title)
##                ':alt-title (: alt-title)
##                ':CATEGORY (: CATEGORY)
##                ':pre-blank (: pre-blank)
##                ':hiddenp (: hiddenp)
##                ':level (: level)
##                ':priority (: priority)
##                ':tags (: tags)
##                ':todo-keyword (: todo-keyword)
##                ':scheduled (: scheduled)
##                ':deadline (: deadline)
##                ':closed (: closed)
##                ':quotedp (: quotedp)
##                ':archivedp (: archivedp)
##                ':commentedp (: commentedp)
##                ':footnote-section-p (: footnote-section-p) )
##             ## (prepCont> (: contents))
##             (: contents)
##             (mapcar
##                'origSubTree>
##                (: descendants) ) ) ) ) )


## concat object attributes, converted to elisp-parse-tree syntax, with
## object's contents string (re-converted to elisp syntax too) and send it to
## Emacs (using function 'emx) for interpretation (-> creation of textual org
## file from parse-tree by `org-element-interpret-data'), finally return
## string with textual representation of headline object or parts of it. If
## 'ContentsOnly?' is non-nil, return only the textual representation of the
## headlines' s content.
(dm interpret> (ContentsOnly?)
   (if ContentsOnly?
      (writeParseTree
         (convertParseTree (: :contents)) )
      (writeParseTree
        (convertParseTree
           (prepareParseTree This) ) ) ) )

## (de prepareParseTree (Headline)
##    (let Plst (getl Headline)
##       (

## (de convertParseTree (Tree)

(de writeParseTree (Tree)
    (emx '(read)
       (pack
       "(add-to-list 'load-path "
       ## ## FIXME! should be:
       ## "\"" (absPath "@iorg/el/") "\"" )
       "\"/home/tj/git/iorg/picoLisp/iorg/el/\")" )
      "(require 'iorg-init)"
      "(require 'iorg)"
      "(require 'ox)"
      "(princ (org-element-interpret-data "
       "\"" (pack Tree) "\""
       "))" ) )

## *** Timestamp
## **** Comments

## - :type :: active or inactive [range]
## - :year-start :: number (convenience attribute)
## - :month-start :: number (convenience attribute)
## - :day-start :: number (convenience attribute)
## - :hour-start :: number (convenience attribute)
## - :minute-start :: number (convenience attribute)
## - :year-end :: number (convenience attribute)
## - :month-end :: number (convenience attribute)
## - :day-end :: number (convenience attribute)
## - :hour-end :: number (convenience attribute)
## - :minute-end :: number (convenience attribute)
## - start-dat ::  start date
## - start-tim :: time of start date
## - end-dat :: end date
## - end-tim :: time of end date
## - :repeater-type :: cumulative, catch-up or restart
## - :repeater-value :: number
## - :repeater-unit :: hour, day, week, month, year

## **** Code

(class +OrgTimestamp +orgElement)
(rel :type (+Ref +Symbol))
(rel :year-start (+Any))
(rel :month-start (+Any))
(rel :day-start (+Any))
(rel :hour-start (+Any))
(rel :minute-start (+Any))
(rel :year-end (+Any))
(rel :month-end (+Any))
(rel :day-end (+Any))
(rel :hour-end (+Any))
(rel :minute-end (+Any))
(rel :start-dat (+Aux +Ref +Date) (start-tim))
(rel :start-tim (+Time))
(rel :end-dat (+Aux +Ref +Date) (end-tim))
(rel :end-tim (+Time))
(rel :repeater-type (+Symbol))
(rel :repeater-value (+Number))
(rel :repeater-unit (+Symbol))

## return repeater-info as string in Org syntax
(dm repeaterString> ()
   (let (V (: :repeater-value)
         U (char (char (: :repeater-unit))) )
      (case (: :repeater-type)
         ("cumulate" (pack "+" V U))
         ("catch-up" (pack "++" V U))
         ("restart" (pack ".+" V U)) ) ) )


(dm rawValue> ()
   (let (TypeLst (mapcar pack (split (chop (: :type)) "-"))
         Range? (member "range" TypeLst)
         SameDate? (and Range? (= (: :start-dat) (: :end-dat)))
         Inactive? (member "inactive" TypeLst)
         Delimiters (if Inactive? (cons "[" "]") (cons "<" ">"))
         StartStamp (pack
                       (text (pack
                                (car Delimiters)
                                "@1 @2@3@4"
                                (cdr Delimiters) )
                          (dat$ (: :start-dat) "-")
                          (pack (head 2 (chop (day (: :start-dat)))))
                          (or
                             (and (eval (: :hour-start))
                                (pack " " (tim$ (: :start-tim))
                                   (and Range? SameDate?
                                      (pack "-" (tim$ (: :end-tim))) ) ) )
                             "" )
                          (or
                             (and (: :repeater-type)
                                (pack " " (repeaterString> This)) )
                             "" ) ) )
         EndStamp (and Range? (not SameDate?)
                     (pack
                        (text (pack
                                (car Delimiters)
                                 "@1 @2@3"
                                (cdr Delimiters) )
                           (dat$ (: :end-dat) "-")
                           (pack (head 2 (chop (day (: :end-dat)))))
                           (or
                              (and (eval (: :hour-end))
                                 (pack " "
                                    (tim$ (: :end-tim)) ) )
                              "" ) ) ) ) )
      (if EndStamp
         (pack StartStamp "--" EndStamp)
         StartStamp ) ) )


(dm convert2parsetree> () )

## *** Title
## **** Comments

## - title :: title string
## - timestamp :: optional title +OrgTimestamp object

## **** Code

(class +OrgTitle +orgElement)
(rel :title (+Ref +String))
(rel :timestamp (+Ref +Link) NIL (+OrgTimestamp))

(dm rawValue> ()
   (pack
      (: :title)
      (and (: :timestamp)
         (pack " " (rawValue> (: :timestamp))) ) ) )

## *** Node Properties
## **** Comments

## - key :: property key
## - value :: property value

## **** Code

## (class +OrgNodeProperty +orgElement)
## (rel key (+Ref +String))
## (rel value (+Ref +String))

## (dm rawValue> ()
##    (pack (: key) " " (: value)) )


## ** iOrg data

## * Obsolete Stuff FIXME:delete

## Expects partial parse-tree of type '(org-data (:ID ...))' created by
## `org-element-parse-buffer' and normalized by `iorg-normalize-parse-tree'.
## (dm T (ParseTree)
##    (let Plist (cadr ParseTree)
##       (super
##          'ID (cadr (memq ':ID Plist))
##          'elem-id (cadr (memq ':elem-id Plist))
##          'input-file (cadr (memq ':input-file Plist))
##          'author (cadr (memq ':author Plist))
##          'email (cadr (memq ':email Plist))
##          'creator (cadr (memq ':creator Plist))
##          'description (cadr (memq ':description Plist))) ) )

## Expects partial parse-tree of type '(headline (:raw-value ...))' created by
## `org-element-parse-buffer' and normalized by `iorg-normalize-parse-tree'.

## (dm T (ParseTree)
##    (let Plist (cadr ParseTree)
##       (super
##          'elem-id (cadr (memq ':elem-id Plist))
##          'begin (cadr (memq ':begin Plist))
##          'end (cadr (memq ':end Plist))
##          'post-blank (cadr (memq ':post-blank Plist))
##          'parent (cadr (memq ':parent Plist))
##          'raw-value (cadr (memq ':raw-value Plist))
##          'title (cadr (memq ':title Plist))
##          'alt-title (cadr (memq ':alt-title Plist))
##          'category (cadr (memq ':CATEGORY Plist))
##          'pre-blank (cadr (memq ':pre-blank Plist))
##          'hiddpenp (cadr (memq ':hiddpenp Plist))
##          'contents-begin (cadr (memq ':contents-begin Plist))
##          'contents-end (cadr (memq ':contents-end Plist))
##          'level (cadr (memq ':level Plist))
##          'priority (cadr (memq ':priority Plist))
##          'tags (cadr (memq ':tags Plist))
##          'todo-keyword (cadr (memq ':todo-keyword Plist))
##          'todo-type (cadr (memq ':todo-type Plist))
##          'scheduled (cadr (memq ':scheduled Plist))
##          'deadline (cadr (memq ':deadline Plist))
##          'closed (cadr (memq ':closed Plist))
##          'quotedp (cadr (memq ':quotedp Plist))
##          'archivedp (cadr (memq ':archivedp Plist))
##          'commentedp (cadr (memq ':commentedp Plist))
##          'footnote-section-p (cadr (memq ':footnote-section-p Plist))
##          'children (cadr (memq ':children Plist))
##          'contents (cadr (memq ':contents Plist)) ) ) )



## * Database sizes
(dbs
   (3 +Role +User (+User pw))          # 512 Prevalent objects
   (0 +Doc +Bin)                       # A:64 Tiny objects
   (1 +Mup)                            # B:128 Small objects
   (2 (+Role nm))                      # C:256 Small indexes
   (4 (+User nm name em ca new))       # D:1024 Normal indexes
   (4 (+Doc nm))                       # E:1024
   (4 (+Mup u))                        # F:1024
   (4 (+Mup d))                        # G:1024
   (6 (+Doc mup)) )                    # H:4096 Full-text index

## iorg/er.l ends here

(0)
