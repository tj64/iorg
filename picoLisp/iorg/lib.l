## * iorg/lib.l --- Library for iOrg
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

## *** Calling Emacs
## With action arguments:

## #+begin_quote
## `-l FILE'
## `--load=FILE'
##      Load a Lisp library named FILE with the function `load'.  If FILE
##      is not an absolute file name, Emacs first looks for it in the
##      current directory, then in the directories listed in `load-path'
##      (*note Lisp Libraries::).

##      *Warning:* If previous command-line arguments have visited files,
##      the current directory is the directory of the last file visited.

## `--eval=EXPRESSION'
## `--execute=EXPRESSION'
##      Evaluate Lisp expression EXPRESSION.
## #+end_quote

## With initial options:

## #+begin_quote
## `-batch'
## `--batch'
##      Run Emacs in "batch mode".  Batch mode is used for running
##      programs written in Emacs Lisp from shell scripts, makefiles, and
##      so on.  To invoke a Lisp program, use the `-batch' option in
##      conjunction with one or more of `-l', `-f' or `--eval' (*note
##      Action Arguments::).  *Note Command Example::, for an example.

##      In batch mode, Emacs does not display the text being edited, and
##      the standard terminal interrupt characters such as `C-z' and `C-c'
##      have their usual effect.  Emacs functions that normally print a
##      message in the echo area will print to either the standard output
##      stream (`stdout') or the standard error stream (`stderr') instead.
##      (To be precise, functions like `prin1', `princ' and `print' print
##      to `stdout', while `message' and `error' print to `stderr'.)
##      Functions that normally read keyboard input from the minibuffer
##      take their input from the terminal's standard input stream
##      (`stdin') instead.

##      `--batch' implies `-q' (do not load an initialization file), but
##      `site-start.el' is loaded nonetheless.  It also causes Emacs to
##      exit after processing all the command options.  In addition, it
##      disables auto-saving except in buffers for which auto-saving is
##      explicitly requested.
## #+end_quote


## * Variables

(setq *IorgGlobalSwitch NIL)

## * Utility functions

## ** Call Emacs

## generic function for calling Emacs from PicoLisp
## usage example: (emx '(read) "/path/to/file.el" "(princ \"Hello World\")")

## slow due to emacs startup for every call
(de emx (Exe . @)
   (in
      (list 'emacs "--no-site-file" "--batch"
         (extract
            '((X)
              (cond
                 ((num? (car (info X)))
                     (pack "--load=" X) )
                 ((= `(char "(") (char X))
                     (pack "--eval=" X) ) ) )
            (rest) ) )
      ## (out 'emx (echo)) ) )
      (eval Exe) ) )

## (de emx (Exe . @)
##    (in
##       (cons 'emacs "--no-site-file" "--batch"
##          (extract
##             '((X)
##               (cond
##                  ((num? (car (info X)))
##                      (pack "--load=" X) )
##                  ((= `(char "(") (char X))
##                      (pack "--eval=" X) ) ) )
##             (rest) ) )
##       (eval Exe) ) )


## generic function for calling Emacsclient from PicoLisp
## usage example: (emc '(read) "server-name" "(princ \"Hello World\")")

## pipe fixes issues of emacsclient
## use this with (echo) for strings
(de ems (Exe . @)
   (pipe
      (in
         (list 'emacsclient
            "-s" "iorg-server" "-e" (rest) )
         (while (echo "\\n" "\\\"" "\\#")
            (case @
               ("\\n" (prin "^J"))
               ("\\#" (prin "\#"))
               ("\\\"" (prin "\"")) ) ) )
      ## (out 'emc (echo)) ) )
      (eval Exe) ) )

## use this with (read) for lists
(de emc (Exe . @)
   (pipe
      (in
         (list 'emacsclient
            "-s" "iorg-server" "-e" (rest) )
         (while (echo "\\n")
            (case @
               ("\\n" (prin "^J")) ) ) )
         (eval Exe) ) )

## ** Retrieve document history

(be history (@Doc @Mup)
   (val @Mup @Doc mup) )

(be history (@Doc @Mup)
   (^ @C (box (; (-> @Doc) mup)))
   (_history @C @Mup) )

(be _history (@C @Prv) (^ @ (not (; (val (-> @C)) <))) T (fail))
(be _history (@C @Prv) (^ @Prv (set (-> @C) (; (val (-> @C)) <))))
(repeat)

## ** Make OrgData Objects

## *** Parse Tree

## expects a parse-tree as produced by `iorg-normalize-parse-tree'
(de processParseTree Tree
   (msg "entering processParseTree ...")
   (makeOrgData (car Tree) (cadr Tree)) )

## create a new +OrgData object and trigger creation of +OrgHeadline objects
## for the parse-tree's headlines
(de makeOrgData (Label Lst)
   (msg "entering makeOrgData ...")
   (setq *IorgGlobalSwitch (extractOptionValue "iorg" Lst))
   (msg *IorgGlobalSwitch " => *IorgGlobalSwitch ")
      (unless (= "nil" *IorgGlobalSwitch)
         (unless (== 'org-data (pop 'Lst))
            (quit "Bad type" DataType) )
         (unless (car Lst)
            (quit "No properties for type 'org-data' specified") )
         (dbSync)
         (let Obj
            (apply
               new
               (make
                  (for (L (pop 'Lst) L)
                     (if (meta '(+OrgData) (car L))
                        (link
                           (pop 'L)
                           (if (hashLbl? (car L))
                              (cut 2 'L)
                              (pop 'L) ) )
                        (pop 'L)
                        (and (hashLbl? (car L) (pop 'L))) ) ) )
               (db: +OrgData) '(+OrgData)
               ':contents (and
                            (== 'section (caadr Lst))
                            (cut 2 'Lst) )
               'label Label )
            (msg Obj " => New OrgData object created")
            (while Lst
               (makeOrgHeadline
                  (and
                     (hashLbl? (car Lst))
                     (pop 'Lst) )
                  (cdr (pop 'Lst))
                  Obj ) )
            (commit 'upd)
            (off *IorgGlobalSwitch)
            Obj ) ) )

## *** Headline

## create a new +OrgHeadline object
(de makeOrgHeadline (Label Lst Ptree)
   (msg "entering makeOrgHeadline ...")
   (let Obj
      (apply
         new
         (make
            (for (L (pop 'Lst) L)
               (if (meta '(+OrgHeadline) (car L))
                  (link
                     (pop 'L)
                     (if (hashLbl? (car L))
                        (cut 2 'L)
                        (pop 'L) ) )
                  (pop 'L)
                  (and (hashLbl? (car L) (pop 'L))) ) ) )
         (db: +OrgHeadline) '(+OrgHeadline)
         ':contents (and
                      (== 'section (caadr Lst))
                      (cut 2 'Lst) )
         'ptree Ptree
         'label Label )
      (put> Obj 'ancestor
         (db 'label '+orgBaseElement
            (makeHashLbl (hashLblNum> Obj 'parent)) ) )
      (msg Obj " => New OrgHeadline object created")
      (treatOrgHeadline Obj) )
(while Lst
   (makeOrgHeadline
      (and
         (hashLbl? (car Lst))
         (pop 'Lst) )
      (cdr (pop 'Lst))
      Ptree ) ) )


(de treatOrgHeadline (H)
  (msg "entering treatOrgHeadline ...")
   (with H
      (let (D (: :deadline)
            S (: :scheduled)
            C (: :closed)
            L (: :title)
            N (: :contents)
            Lst (and N (processContents (cadr N))) )
      (and D (put> This 'deadline-stamp (makeOrgTimestamp (car (tail -1 D))))
         (put> This ':deadline '(rawValue> (: deadline-stamp))) )
      (and S (put> This 'scheduled-stamp (makeOrgTimestamp (car (tail -1 S))))
         (put> This ':scheduled '(rawValue> (: scheduled-stamp))) )
      (and C (put> This 'closed-stamp (makeOrgTimestamp (car (tail -1 C))))
         (put> This ':closed '(rawValue> (: closed-stamp))) )
      (and L (put> This 'title-object (makeOrgTitle L This))
         (put> This ':raw-value '(rawValue> (: title-object)))
         (put> This ':title '(: title-object title)) )
      (and Lst (msg Lst " => Lst ") (put> This ':contents (car Lst))
         (put> This 'properties (processProperties (cdr Lst))) ) ) ) )

## (processContents (cadr '(\#8= (section ...)))
(de processContents (Lst)
   (let Props NIL
      (cons
         (filter
            '((X)
              (nor
                 (and (atom X) (hashLbl? X))
                 (== 'planning (car X))
                 (and (== 'property-drawer (car X))
                    (setq Props X) ) ) )
            Lst )
         Props ) ) )

(de convertContents (Lst) )


(de processProperties (Lst)
   (msg "entering processProperties ...")
   (pop 'Lst)
   (let Props (make
                 (mapcar
                 '((P) (and
                          (lst? P) (= (car P) 'node-property)
                          (link
                             (makeOrgProperty
                                (car (nth (cadr P) 2))
                                (car (nth (cadr P) 4)) ) ) ) )
                 Lst ) )
      Props ) )

## *** Timestamp

(de makeOrgTimestamp (Lst)
   (msg "entering makeOrgTimestamp ...")
   (let Obj
      (apply new
         (fish atom
            (make
               (for (L Lst L)
                  (if (meta '(+OrgTimestamp) (car L))
                     (link (cut 2 'L))
                     (cut 2 'L) ) ) ) )
         (db: +OrgTimestamp) '(+OrgTimestamp) )
      (msg Obj " => New OrgTimestamp object created")
      (treatOrgTimestamp Obj)
      Obj ) )

(de treatOrgTimestamp (S)
   (msg "entering treatOrgTimestamp ...")
   (with S
      (put> S 'start-dat
         (date (: :year-start)(: :month-start)(: :day-start)) )
      (put> S 'start-tim
         (time (: :hour-start)(: :minute-start)) )
      (put> S 'end-dat
         (date (: :year-end)(: :month-end)(: :day-end)) )
      (put> S 'end-tim
         (time (: :hour-end)(: :minute-end)) )
      (put> S ':year-start
         '(car (date (: start-dat))) )
      (put> S ':month-start
         '(cadr (date (: start-dat))) )
      (put> S ':day-start
         '(last (date (: start-dat))) )
      (put> S ':hour-start
         '(car (time (: start-tim))) )
      (put> S ':minute-start
         '(cadr (time (: start-tim))) )
      (put> S ':year-end
         '(car (date (: end-dat))) )
      (put> S ':month-end
         '(cadr (date (: end-dat))) )
      (put> S ':day-end
         '(last (date (: end-dat))) )
      (put> S ':hour-end
         '(car (time (: end-tim))) )
      (put> S ':minute-end
         '(cadr (time (: end-tim))) ) ) )


## *** Title

(de makeOrgTitle (Lst Headline)
   (msg "entering makeOrgTitle ...")
   (let (RawValue (; Headline raw-value)
         TimeStamp (and Lst
                      (car (fish '((X) (and (lst? X)
                                     (= (car X) 'timestamp) ) )
                              Lst ) ) )
         Obj (new (db: +OrgTitle) '(+OrgTitle)
                'title RawValue
                'timestamp (and TimeStamp
                              (makeOrgTimestamp (car (tail -1 TimeStamp))) ) ) )
      (msg Obj " => New OrgTitle object created")
      (treatOrgTitle Obj)
      Obj ) )

## FIXME put title too when no timestamp
(de treatOrgTitle (Title)
   (msg "entering treatOrgTitle ...")
   (when (; Title timestamp)
      (let L (chop (; Title title))
         (and
            (seek
               '((L)
                  (head
                     (cons "<" (chop (dat$ (; Title
                  timestamp start-dat) "-")))
                     (cdr L) ) )
               L )
            (con @) )
         (put> Title 'title (pack (trim L))) ) ) )

## *** Property

(de makeOrgProperty (Key Val)
    (msg "entering makeOrgProperty ...")
    (let Obj (new (db: +OrgProperty) '(+OrgProperty)
                'key Key
                'value Val )
      (msg Obj " => New OrgProperty object created")
      (msg (show Obj))
      Obj ) )

## *** Helper Functions

## converts 1 to \#1=
(de makeHashLbl (Num)
   (intern
      (pack "\#" Num "=") ) )

## converts 1 to \#1#
(de makeHashRef (Num)
   (intern
      (pack "\#" Num "#") ) )

## check if symbol is a label like #2= or #12=
(de hashLbl? (X)
   (and
      (sym? X)
      (let L (chop X)
         (and
            (= `(char "#") (char (car L)))
            (= `(char "=") (char (last L))) ) ) ) )

## check if symbol is a label like #2#= or #12#
(de hashRef? (X)
   (and
      (sym? X)
      (let L (chop X)
         (and
            (= `(char "#") (char (car L)))
            (= `(char "#") (char (last L))) ) ) ) )

## (de oldConvertparsetree (Ptree)
##    (cond
##       ((hashLbl? (car Ptree))
##        (pop 'Ptree)
##        (convertParseTree Ptree) )
##       ((eq '\# (car Ptree)) ) ) )


## (de convertParseTree (Lst)
##    (mapcan
##       '((X)
##          (cond
##             ((num? X) (cons X))
##             ((pair X) (cons (convertParseTree X)))
##             ((not X) (cons 'nil))
##             (T
##                (let L (chop X)
##                   (cond
##                      ((or (== '\# X) (<> "#" (car L)))
##                         (cons (pack (replace L "^J" "\\n"))) )
##                      ((and (= "#" (last L)) (format (head -1 (cdr L))))
##                         (cons 'nil) )
##                      ((and (= "=" (last L)) (format (head -1 (cdr L)))))
##                      (T (cons (pack (replace L "^J" "\\n")))) ) ) ) ) )
##       Lst ) )

## (de convertParseTree (Lst)
##    (mapcan
##       '((X)
##          (cond
##             ((num? X) (cons X))
##             ((pair X) (cons (convertParseTree X)))
##             ((not X) (cons 'nil))
##             (T
##                (let L (chop X)
##                   (cond
##                      ((str? X)
##                         (cons (pack (replace L "^J" "\\n"))) )
##                      ((or (== '\# X) (<> "#" (car L)))
##                         (cons X) )
##                      ((and (= "#" (last L)) (format (head -1 (cdr L))))
##                         (cons 'nil) )
##                      ((and (= "=" (last L)) (format (head -1 (cdr L)))))
##                     (T (cons X)) ) ) ) ) )
##       Lst ) )


(de convertParseTree (Lst)
   (mapcan
      '((X)             
         (use L
            (cond
               ((num? X) (cons X))
               ((pair X) (cons (convertParseTree X)))
               ((not (setq L (chop X))) (cons 'nil))
               ((str? X) (cons (pack (replace L "^J" "\\n"))))
               ((or (== '\# X) (<> "#" (car L)))
                  (cons X) )
               ((and (= "#" (last L)) (format (head -1 (cdr L))))
                  (cons 'nil) )
               ((and (= "=" (last L)) (format (head -1 (cdr L)))))
               (T (cons X)) ) ) ) 
      Lst ) )  


## (de convertParseTree (Lst)
##    (mapcan
##       '((X)
##         (cond
##            ((num? X) (cons X))
##            ((pair X) (cons (convertParseTree X)))
##            ((not X) (cons 'nil))
##            (T
##               (let L (chop X)
##                  (cond
##                     ((or (== '\# X) (<> "#" (car L)))
##                      (cons X) )
##                     ((= "#" (last L))
##                      (cons 'nil) )
##                     ((= "=" (last L)))
##                     (T (cons X)) ) ) ) ) )
##       Lst ) )


## (de interpretParseTree (Lst)
##    (out "/home/tj/em.dat"  # (tmp "em")
##       (pipe (print (convertParseTree Lst))
##          (while (echo "\\# (")
##             (prin "#(") ) ) )
##    (pipe
##       (in
##          (list 'emacsclient "-s" "iorg-server" "-e"
##             (pack 
##                "(princ "
##                "(org-element-interpret-data "
##                "(with-current-buffer "
##                "(find-file-noselect "
##                ## "\"" (tmp "em") "\")"
##                "\"/home/tj/em.dat\")"
##                "(goto-char (point-min))"
##                "(read (current-buffer)))))" ) )
##          (while (echo "\\n")
##             (prin "\^J") ) )
##       (till NIL T) ) )
##       ## (any (read)) ) )

(de interpretParseTree (Lst)
   (out (tmp "em")
      (pipe (print (convertParseTree Lst))
         (while (echo "\\# (")
            (prin "#(") ) ) )
   ## (pipe
      (in
         (list 'emacsclient "-s" "iorg-server" "-e"
            (pack 
               "(princ "
               "(org-element-interpret-data "
               "(with-current-buffer "
               "(find-file-noselect "
               "\"" (tmp "em") "\")"
               "(goto-char (point-min))"
               "(read (current-buffer)))))" ) )
         (while (echo "\\n")
            (prin "\^J") ) ) )
      ## (till NIL T) ) )
      ## (any (read)) ) )

## (de interpretParseTree (Lst)
##    (out "/home/tj/em.dat"  # (tmp "em")
##       (pipe (println (convertParseTree Lst))
##          (while (echo "\ \# (")
##             (prin "#(") ) ) )
##    (in
##       (list 'emacsclient "-s" "iorg-server" "-e"
##          (pack
##             "(princ "
##             "(org-element-interpret-data "
##             "(with-current-buffer "
##             "(find-file-noselect "
##             ## "\"" (tmp "em") "\")"
##             "\"/home/tj/em.dat\")"
##             "(goto-char (point-min))"
##             "(read (current-buffer)))))" ) )
##       (till NIL T) ) )

## (de interpretParseTree (Lst)
##    (call 'mkfifo (tmp "em"))
##    (out (tmp "em") (print (convertParseTree Lst)))
##    (in (list 'emacsclient "-e"
##           (pack
##              "(princ "
##              "(org-element-interpret-data "
##              "(with-current-buffer "
##              "(find-file-noselect "
##              "\"" (tmp "em") "\")"
##              "(goto-char (point-min))"
##              "(read (current-buffer)))))" ) ) ) )


(de interpretContents (This)
   (interpretParseTree
      (convertParseTree (: :contents)) ) )

## (de interpretContents (Obj)
##    (with Obj
##       (let Txt (pack "\""
##                   (interpretParseTree
##                              (convertParseTree (: :contents) ) )
##                   "\"" )
##          Txt ) ) )

## expects a parse-tree as produced by `iorg-normalize-parse-tree'
## and an option as string like e.g. "iorg"

(de extractOptionValue (Option ParseTree)
   (let
      (OrgDataContent
         (and
            (== (caar (nth ParseTree 4)) 'section)
            (car (nth ParseTree 4)) )
         Strg
         (glue " "
            (mapcar
              '((X)
                  (and
                     (== (car X) 'keyword)
                     (= (car (nth (cadr X) 2)) "OPTIONS")
                     (car (nth (cadr X) 4)) ) )
               OrgDataContent ) ) )
      (cadr (member Option (mapcar pack (split (chop Strg) ":" " ")))) ) )

## (de extractOptionValue (Option ParseTree)
##    (let (OrgDataContent (and
##                            (== (caar (nth ParseTree 4)) 'section)
##                            (car (nth ParseTree 4)) )
##          Strg (glue " "
##                  (mapcar
##                     '((X) (and
##                              (== (car X) 'keyword)
##                              (= (car (nth (cadr X) 2)) "OPTIONS")
##                              (car (nth (cadr X) 4)) ) )
##                     OrgDataContent ) ) )
##       (cadr (member Option (mapcar pack (split (chop Strg) ":" " ")))) ) )

(de emacsServerMessages ()
    (emc
       '(echo)
       (pack
          "(message \"%s\" "
          "(with-current-buffer \"*Messages*\" (buffer-string)))" ) ) )

(de emacsServerBacktrace ()
    (emc
       '(echo)
       (pack
          "(message \"%s\" "
          "(with-current-buffer \"*Backtrace*\" (buffer-string)))" ) ) )

(de emacsServerToggleDebugOnError ()
    (emc '(echo) "(toggle-debug-on-error)") )

## create graphical emacs-server frame with scratch-buffer selected
(de es ()
    (call 'emacsclient
       "-s" "iorg-server"
       "-c"
       "-e"
       (pack 
         "(progn "
         "(and (display-graphic-p) "
         "(set-background-color \"black\") "
         "(set-foreground-color \"wheat\")))" ) ) )
         ## "(setq revert-without-query "
         ## "(quote (\"/home/tj/\\.pil/tmp/.+\"))))" ) ) )

## edit File in graphical emacs-server frame
(de ee (File)
   (call 'emacsclient
      "-s" "iorg-server"
      "-c"
      "-e"
      (pack
         "(progn "
         "(and (display-graphic-p) "
         "(set-background-color \"black\") "
         "(set-foreground-color \"wheat\")) "
         ## "(setq revert-without-query "
         ## "(quote (\"/home/tj/\\.pil/tmp/.+\")))"
         "(let ((fn \"" File  "\"))"
         "(if (file-name-extension fn) "
         "(find-file (expand-file-name fn iorg-el-dir)) "
         "(find-file (expand-file-name "
         "(concat fn \".el\") iorg-el-dir)))))" ) ) )

## ** Others

## Search field hint list
(def 'search 'hint
   '((Str)
      (make
         (catch NIL
            (scan '(mup . +Doc)
               '((K V)
                  (unless (member (car K) (made))
                     (link (car K))
                     (and (nth (made) 24) (throw)) ) )
               (cons (setq Str (fold Str)))
               (cons (pack Str `(char T)) T) ) ) ) ) )

## Split list of characters into folded words
(de splitWords (Lst)
   (extract fold
      (mapcar pack
         (split Lst ~(chop "^J !,-.:;?{}")) ) ) )

## Get markup's title
(dm (title> . +Mup) ()
   (in (blob This 'txt)
      (skip)
      (use (@N @X)
         (and
            (match '(@N "{" @X "}" @) (line))
            (format @N)
            (pack @X) ) ) ) )

## Test if symbol S is a keyword-symbol
(de keywordSymbol? (S)
   (and
      (sym? S)
      (= (car (chop S)) ":") ) )


## scrape: display all fields, not only those with values
(de displayAll ()
   (prinl "###############")
   (print 'click:)
   (for (I . L) *Links
      (space)
      (print I (car L)) )
   (prinl) (prinl)
   (print 'press:)
   (for (I . B) *Buttons
      (space)
      (print I (car B)) )
   (prinl) (prinl)
   (print 'enter/value:)
   (for (I . F) *Fields
      (case (fin F)
         ((" " "@"))
         (T (space)
            (print I (trim F)) ) ) )
   (prinl) (prinl)
   ## (print 'enter/value:)
   ## (for (I . F)  *Fields
   ##    (and
   ##       (lst? F)
   ##       (space)
   ##       (print I (trim F)) ) )
   ## (prinl) (prinl)
   *Title )

## Download blob
(de download (This)
   (httpHead (: mt) 60 (: nm))
   (in (blob This 'd) (ht:Out *Chunked (echo))) )


## * Predefined Emacs Calls

## ** Read Parse Tree

## ## expects absolute PATH to org file
## (de readParseTree (Path)
##     (emx '(read)
##        (pack
##        "(add-to-list 'load-path "
##        ## ## FIXME! should be:
##        ## "\"" (absPath "@iorg/el/") "\"" )
##        "\"/home/tj/git/iorg/picoLisp/iorg/el/\")" )
##       "(require 'iorg-init)"
##       "(require 'iorg)"
##       "(require 'ox)"
##       (pack
##          "(with-current-buffer "
##          "(find-file "
##          "\"" Path "\")"
##          "(org-mode)"
##          "(let ((org-cycle-silently 1))"
##          "(princ (iorg-normalize-parse-tree))))" ) ) )


## expects absolute PATH to org file
## stores parse-tree and all its headlines in PicoLisp DB
(de readParseTree (Path)
   (msg "entering readParseTree ...")
   (eval
      (any
         (emc '(read)
            (pack
               "(with-current-buffer "
               "(find-file "
               "\"" (absPath Path) "\")"
               "(org-mode)"
               "(let ((org-cycle-silently 1))"
               "(princ "
               "(iorg-wrap-parse-tree "
               "(iorg-convert-parse-tree)))))" ) ) ) ) )


## ** Write Parse Tree

## expects a 'raw' (complete) parse-tree as string, rebuilt from the PicoLisp
## DB, and returns the textual representation of the parsed Org file
## (de writeParseTree (Ptree)
##    (emc '(echo)
##       (pack
##          "(with-output-to-string "
##          "(find-file "
##          "\"" Path "\")"
##          "(org-mode)"
##          "(let ((org-cycle-silently 1))"
##          "(princ "
##          "(iorg-wrap-parse-tree "
##          "(iorg-convert-parse-tree)))))" ) ) )) )

## ** Set Path


## expects an absolute path
(de addToEmacsLoadPath (Path)
  (call
     'emacsclient
     "-s" "iorg-server"
     "-e"
     (pack
        "(add-to-list 'load-path "
        "(expand-file-name "
        "\"" Path "\" ) )" ) ) )

## * Render document

## ** Render document as HTML

## FIXME fast, but renders lots of \n in html output
## let ox-html.el render Org-file
(de oxRender (Path)
   (ems '(echo)
      (pack
         "(with-temp-buffer "
         "(insert-file-contents "
         "\"" Path "\")"
         "(org-mode)"
         "(let ((org-cycle-silently 1))"
         "(princ "
         ## "(prin1 "
         ## "(format \"%s\" "
         "(org-export-as 'html nil nil t)))))" ) ) )

## ## FIXME: slow, but works - replace with fast emacsclient call
## ## let ox-html.el render Org-file
## (de oxRender (Path)
##    (emx '(echo)
##       "(require 'ox)"
##       (pack
##          "(with-temp-buffer "
##          "(insert-file-contents "
##          "\"" Path "\")"
##          "(org-mode)"
##          "(let ((org-cycle-silently 1))"
##          "(princ "
##          "(format \"%s\" "
##          "(org-export-as 'html nil nil t)))))" ) ) )

## process internal links (as last rendering step)
(de processInternalLinks ()
   (use C
      (loop
         (prin (till "=" T))
         (NIL (setq C (char)))
         (case (peek)
            ("^J"
             (ifn (= "^J" C)
                (prin C)
                (----)
                (char) ) )
            ("{"
             (char)  # Skip "{"
             (case C
                ("="  # Internal document link
                 (iorgLink (split (till "}") " " "^I" "^J" "^M"))
                 (char) ) ) )
            (T (prin C)) ) ) ) )

## redefined <this>
## print internal links with already html-escaped chars
(de <this1> (Var Val . Prg)
   (prin
      "<a href=\""
      (sesId *Url)
      '?
      Var
      '=
      (ht:Fmt Val)
      "\"" )
   (and *Style (htStyle @))
   (prin '>)
   (prinLink Prg 2)
   (prin "</a>") )

## redefined htPrin
## print internal links with already html-escaped chars
(de prinLink (Prg Ofs)
   (default Ofs 1)
   (for X Prg
      (if (atom X)
         (prin (eval X Ofs))
         (eval X Ofs) ) ) )


(de renderBlock (Fun)
   (let (N 0  C)
      (loop
         (NIL (setq C (char)))
         (T (and (=0 N) (= "}" C)))
         (case (Fun C)
            ("\\" (Fun (char)))
            ("{" (inc 'N))
            ("}" (dec 'N)) ) ) ) )

## ** Render document as LaTeX

(de renderLatex (This)
   (prinl "% " (datSym (date)))
   (prinl "% " (or (: u name) (: u nm)))
   (prinl)
   (tex "documentclass[10pt,a4paper]" "article")
   (tex "usepackage" "graphicx")
   (prinl)
   (prinl "\\textwidth 1.4\\textwidth")
   (prinl "\\\textheight 1.125\\textheight")
   (prinl "\\oddsidemargin 0em")
   (prinl "\\evensidemargin 0em")
   (prinl "\\headsep 0em")
   (prinl "\\parindent 0em")
   (prinl "\\parskip 6pt")
   (prinl)
   (and (title> This) (tex "title" @))
   (let A This
      (while (and (; A min) (; A <))
         (setq A @) )
      (let B A
         (while (; B <)
            (setq B @) )
         (setq A (; A u)  B (; B u))
         (if (== A B)
            (tex "author"
               (or (; A name) (; A nm))
               (; A em) )
            (texl "author"
               (list
                  (or (; B name) (; B nm))
                  (; B em) )
               " \\and "
               (list
                  (or (; A name) (; A nm))
                  (; A em) ) ) ) ) )
   (tex "date" (datStr (: d)))
   (prinl)
   (\\block "document"
      (tex "maketitle")
      (prinl)
      (in (blob This 'txt)
         (let Nest NIL
            (recur (Nest)
               (use C
                  (loop
                     (texPrin (till "^J123456&/!_*+-<@>=\^:\\#}" T))
                     (NIL (setq C (char)))
                     (T (and Nest (= C "}")))
                     (ifn (= "{" (peek))
                        (texPrin C)
                        (char)  # Skip "{"
                        (case C
                           (("1" "2" "3" "4" "5" "6")  # Heading
                              (_latex (if (>= "3" C) "section*" "subsection*")) )
                           ("&"
                              (do (format (till "}" T))
                                 (prin "\\\\") )
                              (char) )
                           ("/"  # Italic
                              (_latex "textit") )
                           ("!"  # Bold
                              (_latex "textbf") )
                           ("_"  # Underlined
                              (_latex "underline") )
                           ("*"  # Unordered list
                              (skip)
                              (\\block "itemize"
                                 (space 3)
                                 (recurse T)
                                 (char) ) )
                           ("+"  # Ordered list
                              (skip)
                              (\\block "enumerate"
                                 (space 3)
                                 (recurse T)
                                 (char) ) )
                           ("-"  # List item
                              (prin "\\item ")
                              (recurse T) )
                           (("<" "@" ">")  # Binary file
                              (let Nm (till "}" T)
                                 (char)
                                 (with (findBin Nm (: bin))
                                    (ifn (pre? "image/" (: mt))
                                       (texPrin Nm)
                                       (call 'convert (blob This 'd) (tmp "img.pdf"))
                                       (\\figure "[h]"
                                          (\\block
                                             (case C
                                                ("<" "flushleft")
                                                ("@" "center")
                                                (">" "flushright") )
                                             (tex "includegraphics" "img.pdf") ) ) ) ) ) )
                           (("=" "\^")  # Internal document / External web link
                              (let S (split (till "}") " " "^I" "^J" "^M")
                                 (char)
                                 (tex NIL "underline" (or (glue " " (cdr S)) (car S)))
                                 (tex NIL "footnote"
                                    (if (= "=" C)
                                       (srcUrl (pack "!iorg?" (car S)))
                                       (car S) ) ) ) )
                           (":"  # Pre-formatted
                              (if (= "^J" (peek))
                                 (prog
                                    (char)
                                    (\\block "verbatim" (renderBlock prin)) )
                                 (prin "\\texttt{")
                                 (renderBlock
                                    '((C)
                                       (and (sub? C "{}") (prin "\\"))
                                       (prin C) ) )
                                 (prin "}") ) )
                           ("\\" (prin "{"))  # Escaped brace
                           ("#" (renderBlock prog))  # Comment
                           (T (prin C "{")) ) ) ) ) ) ) ) ) )

(de _latex (S)
   (prin "\\" S "{")
   (recurse T)
   (prin "}") )

## * Export document
## ** Produce LaTeX document

(de latex (This)
   (ifn (ext? This)
      (noContent)
      (httpHead "application/x-latex" 20 (pack (or (: doc nm) "untitled") ".tex"))
      (ht:Out *Chunked (renderLatex This)) ) )

## ** Produce PDF document

(de pdf (This)
   (ifn (ext? This)
      (noContent)
      (let Nm (or (: doc nm) "untitled")
         (out (tmp Nm ".tex") (renderLatex This))
         (chdir (tmp)
            (in (list "pdflatex" "-interaction=batchmode" Nm))
            (httpEcho (pack Nm ".pdf") "application/pdf" 20) ) ) ) )

## * Old stuff (for later use)

## ** OrgData

## (de makeOrgHeadline1 (Label Lst Iorg?)
##    (msg "makeOrgHeadline entered...")
##    (msg Lst " =>  Lst")
##    ## (msg Label " => Label")
##    ## (msg Iorg? " => Iorg?")
##    (and Lst
##       (let (Tags (cadr (member 'tags (car Lst)))
##             Tag (and Tags
##                    (if (str? Tags)
##                       (and (or (= Tags "iorg") (= Tags "noiorg")) Tags)
##                       (or
##                          (car (member "iorg" Tags))
##                          (car (member "noiorg" Tags)) ) ) )
##             Prop (cadr (member 'IORG (car Lst))) )
##          ## (and
##          ##    (or
##          ##       (= "t" *IorgGlobalSwitch)
##          ##       (and
##          ##          (not (or (= Tag "noiorg") (= Prop "off")))
##          ##          (and (or Iorg? (= Tag "iorg") (= Prop "on"))) ) )
##          (ifn
##             (or
##                (= "t" *IorgGlobalSwitch)
##                (and
##                   (not (or (= Tag "noiorg") (= Prop "off")))
##                   (and (or Iorg? (= Tag "iorg") (= Prop "on"))) ) )

##             (let (List (make
##                           (for (L (pop 'Lst) L)
##                              (link
##                                 (pop 'L)
##                                 (if (hashLbl? (car L))
##                                    (cut 2 'L)
##                                    (pop 'L) ) ) ) ) )
##                (and (== 'section (caadr Lst)) (cut 2 'Lst) )
##                (makeOrgHeadline
##                   (and
##                      (hashLbl? (car List))
##                      (pop 'List) )
##                   (if (str? (car List) NIL (cdr (pop 'List))))
##                   ## (cdr (pop 'List))
##                   Iorg? ) )

##             (or Iorg? (on Iorg?))
##             (let Obj
##                (apply
##                   new
##                   (make
##                      (for (L (pop 'Lst) L)
##                         (link
##                            (pop 'L)
##                            (if (hashLbl? (car L))
##                               (cut 2 'L)
##                               (pop 'L) ) ) ) )
##                   (db: +OrgHeadline) '(+OrgHeadline)
##                   'contents (and
##                                (== 'section (caadr Lst))
##                                (cut 2 'Lst) ) )
##                (and Label (set @ Obj))
##                (msg (show Obj) " => New OrgHeadline object created") ) )
##          (while Lst
##             ## (msg "while OrgHeadline ...")
##             ## (msg (length Lst) "while OrgHeadline")
##             ## (msg Lst)
##             (makeOrgHeadline
##                (and
##                   (hashLbl? (car Lst))
##                   (pop 'Lst) )
##                (or
##                   (cdr (pop 'Lst))
##                   (car (pop 'Lst)) )
##                Iorg? ) ) ) ) )


## ** Render

## ## render original PicoLisp Wiki syntax
## (de render (Bin)
##    (let Nest NIL
##       (recur (Nest)
##          (use C
##             (loop
##                (ht:Prin (till "^J123456&/!_*+-<@>=\^:\\#}" T))
##                (NIL (setq C (char)))
##                (T (and Nest (= C "}")))
##                (case (peek)
##                   ("^J"
##                      (ifn (= "^J" C)
##                         (ht:Prin C)
##                         (----)
##                         (char) ) )
##                   ("{"
##                      (char)  # Skip "{"
##                      (case C
##                         (("1" "2" "3" "4" "5" "6")  # Heading
##                            (_render "h" C)
##                            (prinl)
##                            (skip) )
##                         ("&"
##                            (let? N (format (till "}" T))
##                               (prin "<br"
##                                  (and (lt0 N) " class=\"clr\"")
##                                  "/>" )
##                               (do (dec (abs N))
##                                  (prin "<br/>") ) )
##                            (char) )
##                         ("/"  # Italic
##                            (_render "i") )
##                         ("!"  # Bold
##                            (_render "b") )
##                         ("_"  # Underlined
##                            (_render "u") )
##                         ("*"  # Unordered list
##                            (_render "ul") )
##                         ("+"  # Ordered list
##                            (_render "ol") )
##                         ("-"  # List item
##                            (_render "li") )
##                         (("<" "@" ">")  # Binary file
##                            (let Nm (till "}" T)
##                               (char)
##                               (with (! findBin Nm Bin)
##                                  (cond
##                                     ((pre? "image/" (: mt))
##                                        (prin "<img src=\""
##                                           *SesId (blob This 'd)
##                                           "\" alt=\"" )
##                                        (ht:Prin Nm)
##                                        (prin
##                                           "\" class=\""
##                                           (case C
##                                              ("<" "left")
##                                              ("@" "nofloat")
##                                              (">" "right") )
##                                           "\"/>" ) )
##                                     ((pre? "video/" (: mt))
##                                        (prin "<video src=\""
##                                           *SesId (blob This 'd)
##                                           "\" class=\""
##                                           (case C
##                                              ("<" "left")
##                                              ("@" "nofloat")
##                                              (">" "right") )
##                                           "\">" )
##                                        (ht:Prin Nm)
##                                        (prin "</video>") )
##                                     (T
##                                        (<style> (cons 'type (: mt))
##                                           (<href> Nm (pack "!download?" (ht:Fmt This))) ) ) ) ) ) )
##                         ("="  # Internal document link
##                            (iorgLink (split (till "}") " " "^I" "^J" "^M"))
##                            (char) )
##                         ("\^"  # External web link
##                            (let S (split (till "}") " " "^I" "^J" "^M")
##                               (char)
##                               (if
##                                  (member
##                                     (car (split (car S) ":"))
##                                     (quote
##                                        `(chop "http")
##                                        `(chop "https")
##                                        `(chop "irc")
##                                        `(chop "mailto") ) )
##                                  (<href>
##                                     (or (glue " " (cdr S)) (pack (car S)))
##                                     (pack (car S)) )
##                                  (prin "???") ) ) )
##                         (":"  # Pre-formatted
##                            ((if (= "^J" (peek)) <pre> <span>) 'code
##                               (renderBlock ht:Prin)) )
##                         ("\\" (prin "{"))  # Escaped brace
##                         ("#" (renderBlock prog))  # Comment
##                         (T (prin C "{")) ) )
##                   (T (ht:Prin C)) ) ) ) ) ) )

## (de _render (C D)
##    (prin "<" C D ">")
##    (recurse T)
##    (prin "</" C D ">") )


## iorg/lib.l ends here
