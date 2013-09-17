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


## (Abstract) Org-Base-Element
(class +orgBaseElement +Entity)
(rel elem-id: (+Need +Number))            # Element ID (unique for parse-tree)

(dm url> ())
(dm upd> ())
(dm diff> ())


## (Abstract) Org-Element
(class +orgElement +orgBaseElement)
(rel begin (+Number))                    # Beginning buffer position
(rel end (+Number))                      # Ending buffer position
(rel post-blank (+Number))               # Number of trailing blank lines/spaces
(rel parent (+Joint) elem-id             # Element/Object containing it
   (+orgBaseElem)

(dm interpret> ())

## Parse-Tree (meta data for containing org file,
## collected from Emacs environment, plus root elem-id)
(class +OrgData +orgBaseElement)
(rel ID (+Need +Key +String))             # (Globally) unique parse-tree ID
(rel input-file (+Need +Ref +String))     # Absolute path input-file
(rel author (+Ref +String))               # Author name
(rel email (+Ref +String))                # Authors email
(rel creator (+Sn +Idx +String))           # Version info for Emacs and Org-mode
(rel description (+Sn +Idx +String))      # Short description of org file

## Expects partial parse-tree of type '(org-data (:ID ...))' created by
## `org-element-parse-buffer' and normalized by `iorg-normalize-parse-tree'.
(dm T (ParseTree)
   (let Plist (cadr ParseTree)
      (=: ID (cadr (member ':ID Plist)))
      (=: elem-id (cadr (member ':elem-id Plist)))
      (=: input-file (cadr (member ':input-file Plist)))
      (=: author (cadr (member ':author Plist)))
      (=: email (cadr (member ':email Plist)))
      (=: creator (cadr (member ':creator Plist)))
      (=: description (cadr (member ':description Plist))) ) )

(dm url> ())
(dm upd> ())
(dm diff> ())

## Headline
(class +OrgHeadline +orgElement)
(rel raw-value (+Sn +Idx +String)) # Raw headline text, no stars and tags
(rel title (+List))                # Parsed headline text with properties
(rel alt-title (+String))          # Alternative title
(rel category (+Ref +List))        # Either name of input file or user attribute
(rel pre-blank (+Number))          # Number blank lines before content
(rel hiddpenp (+Ref +Symbol))      # Non-nil (symbol) if headline is hidden
(rel contents-begin (+Number)      # Begin content
(rel contents-end (+Number))       # End content
(rel level (+Need +Ref +Number))   # Reduced level of headline
(rel priority (+Ref +Number))      # Headlines priority as character
(rel tags (+Ref +List))            # Headlines tags (without archive tag)
(rel todo-keyword (+Ref +String))  # TODO keyword without quote/comment
(rel todo-type (+Ref +String))     # Type of TODO keyword (done or todo)
(rel scheduled (+Ref +Date))       # SCHEDULED reference (timestamp or NIL)
(rel deadline (+Ref +Date))        # DEADLINE reference (timestamp or NIL)
(rel closed (+Ref +Date))          # CLOSED reference (timestamp or NIL)
(rel quotedp (+Ref +Bool))         # Non-nil if headline contains quote keyword
(rel archivedp (+Ref +Bool))       # Non-nil if headline has archived tag
(rel commentedp (+Ref +Bool))      # Non-nil if headline has comment keyword
(rel footnote-section-p            # Non-nil if headline is footnote-section
   (+Ref +Bool) )
(rel children (+List +Joint)       # Children
   elem-id (+OrgHeadline)
(rel contents (+List))             # Normalized partial parse-tree


## Expects partial parse-tree of type '(headline (:raw-value ...))' created by
## `org-element-parse-buffer' and normalized by `iorg-normalize-parse-tree'.
(dm T (ParseTree)
   (let Plist (cadr ParseTree)
      (=: elem-id (cadr (member ':elem-id Plist)))
      (=: begin (cadr (member ':begin Plist)))
      (=: end (cadr (member ':end Plist)))
      (=: post-blank (cadr (member ':post-blank Plist)))
      (=: parent (cadr (member ':parent Plist)))
      (=: raw-value (cadr (member ':raw-value Plist)))
      (=: title (cadr (member ':title Plist)))
      (=: alt-title (cadr (member ':alt-title Plist)))
      (=: category (cadr (member ':category Plist)))
      (=: pre-blank (cadr (member ':pre-blank Plist)))
      (=: hiddpenp (cadr (member ':hiddpenp Plist)))
      (=: contents-begin (cadr (member ':contents-begin Plist)))
      (=: contents-end (cadr (member ':contents-end Plist)))
      (=: level (cadr (member ':level Plist)))
      (=: priority (cadr (member ':priority Plist)))
      (=: tags (cadr (member ':tags Plist)))
      (=: todo-keyword (cadr (member ':todo-keyword Plist)))
      (=: todo-type (cadr (member ':todo-type Plist)))
      (=: scheduled (cadr (member ':scheduled Plist)))
      (=: deadline (cadr (member ':deadline Plist)))
      (=: closed (cadr (member ':closed Plist)))
      (=: quotedp (cadr (member ':quotedp Plist)))
      (=: archivedp (cadr (member ':archivedp Plist)))
      (=: commentedp (cadr (member ':commentedp Plist)))
      (=: footnote-section-p (cadr (member ':footnote-section-p Plist)))
      (=: children (cadr (member ':children Plist)))
      (=: contents (cadr (member ':contents Plist))) ) )

(dm url> ())
(dm upd> ())
(dm diff> ())

## concat object attributes, converted to elisp-parse-tree syntax, with
## object's contents string (re-converted to elisp syntax too) and send it to
## Emacs (using function 'emx) for interpretation (-> creation of textual org
## file from parse-tree by `org-element-interpret-data'), finally return
## string with textual representation of headline object or parts of it. If
## 'ContentsOnly?' is non-nil, return only the textual representation of the
## headlines' s content.
(dm interpret> (ContentsOnly?))

## ** iOrg data

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
