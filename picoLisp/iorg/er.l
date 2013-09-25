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
## - elem-id :: element ID (unique for parse-tree)
## - contents :: normalized partial parse-tree

## **** Code

(class +orgBaseElement +Entity)
(rel descendants (+List +Joint) ancestor (+orgElement))
(rel label (+Ref +Symbol))
(rel contents (+Any))

(dm url
(dm url> ())
(dm upd> ())
(dm diff> ())
(dm new> ())
## return label number as string
(dm lblNum> ()
   (pack
      (tail -1
         (head -1
            (chop (: label)) ) ) ) )

## *** (Abstract) Org-Element

## **** Comments

## - ancestor :: joint link to ancestor element
## - post-blank :: number of trailing blank lines/spaces
## - parent :: element/Object containing it

## **** Code

(class +orgElement +orgBaseElement)
(rel ancestor (+Joint) descendants (+orgBaseElement))
(rel post-blank (+Number))
(rel parent (+Ref +Symbol))

(dm interpret> ())
## return parent number as string
(dm parNum> ()
   (pack
      (tail -1
         (head -1
            (chop (: parent)) ) ) ) )

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
(rel parse-tree-id (+Need +Key +String))
(rel input-file (+Need +Ref +String))
(rel author (+Ref +String))
(rel email (+Ref +String))
(rel creator (+Sn +Idx +String))
(rel description (+Sn +Idx +String))

(dm url> (Tab)
   (and (may Iorg) (list "iorg/orgData.l" '*ID This)) )

(dm upd> ())
(dm diff> ())

## expects a partial parse-tree of type 'org-data' as created by
## `org-element-parse-buffer' and normalized by `iorg-normalize-parse-tree'

## (dm new> (Tree)
##  (and
##     (lst? Tree)
##     (eq (car Tree) 'org-data) ) )


## *** Headline

## **** Comments

## - raw-value :: raw headline text, no stars and tags
## - title :: parsed headline text with properties
## - alt-title :: alternative title
## - category :: either name of input file or user attribute
## - pre-blank :: number blank lines before content
## - hiddpenp :: non-nil (symbol) if headline is hidden
## - level :: reduced level of headline
## - priority :: headlines priority as character
## - tags :: headlines tags (without archive tag)
## - todo-keyword :: todo keyword without quote/comment
## - todo-type :: type of TODO keyword (done or todo)
## - scheduled :: scheduled reference (timestamp or NIL)
## - deadline :: deadline reference (timestamp or NIL)
## - closed :: closed reference (timestamp or NIL)
## - quotedp :: non-nil if headline contains quote keyword
## - archivedp :: non-nil if headline has archived tag
## - commentedp :: non-nil if headline has comment keyword
## - footnote-section-p :: non-nil if headline is footnote-section

## **** Code

(class +OrgHeadline +orgElement)
(rel raw-value (+Idx +String))
(rel title (+Any))
(rel alt-title (+Any))
(rel CATEGORY (+Any))
(rel pre-blank (+Number))
(rel hiddenp (+Symbol))
(rel level (+Need +Number))
(rel priority (+Ref +Number))
(rel tags (+List +String))
(rel todo-keyword (+Ref +String))
(rel todo-type (+Ref +String))
(rel scheduled (+Date))
(rel deadline (+Date))
(rel closed (+Date))
(rel quotedp (+Bool))
(rel archivedp (+Bool))
(rel commentedp (+Bool))
(rel footnote-section-p (+Bool))


(dm url> (Tab)
   (and (may Iorg) (list "iorg/orgHeadline.l" '*ID This)) )

(dm upd> ())
(dm diff> ())

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
         (convertParseTree (: contents)) )
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
       ## "\"" (absolutePath "@iorg/el/") "\"" )
       "\"/home/tj/git/iorg/picoLisp/iorg/el/\")" )
      "(require 'iorg-init)"
      "(require 'iorg)"
      "(require 'ox)"
      "(princ (org-element-interpret-data "
       "\"" (pack Tree) "\""
       "))" ) )

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
