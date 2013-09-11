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

(class +orgElement +Entity)
(rel :begin (+Number))               # Beginning buffer position
(rel :end (+Number))                 # Ending buffer position
(rel :post-blank (+Number))          # Number of trailing blank lines/spaces
(rel :parent (+Link))                # Element/Object containing it

(dm url> ())
(dm upd> ())
(dm diff> ())
(dm interpret> ())


(class +OrgHeadline +orgElement)
(rel :raw-value (+String))          # Raw headline text, no stars and tags
(rel :title (+String))              # Parsed headline text, no stars and tags
(rel :alt-title (+String))          # Alternative title
(rel :pre-blank (+Number))          # Number blank lines before content
(rel :hiddpenp (+Bool))             # Non-nil if headline is hidden
(rel :contents-begin (+Number)      # Begin content
(rel :contents-end (+Number))       # End content
(rel :level (+Number))              # Reduced level of headline
(rel :priority (+Number))           # Headlines priority as character
(rel :tags (+List))                 # Headlines tags (without archive tag)
(rel :todo-keyword (+String))       # TODO keyword without quote/comment
(rel :todo-type (+String))          # Type of TODO keyword (done or todo)
(rel :scheduled (+Date))            # SCHEDULED reference (timestamp or NIL)
(rel :deadline (+Date))             # DEADLINE reference (timestamp or NIL)
(rel :closed (+Date))               # CLOSED reference (timestamp or NIL)
(rel :quotedp (+Bool))              # Non-nil if headline contains quote keyword
(rel :archivedp (+Bool))            # Non-nil if headline has archived tag
(rel :commentedp (+Bool))           # Non-nil if headline has comment keyword
(rel :footnote-section-p (+Bool))   # Non-nil if headline is footnote-section

(dm url> ())
(dm upd> ())
(dm diff> ())
(dm interpret> ())

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
