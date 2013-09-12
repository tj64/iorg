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


## * Code

## ** Utility functions

## generic function for calling Emacs from PicoLisp
## usage example: (emx '(read) "/path/to/file.el" "(princ \"Hello World\")")
(de emx (Exe . @)
   (in
      (cons 'emacs "--no-site-file" "--batch"
         (extract
            '((X)
              (cond
                 ((num? (car (info X)))
                     (pack "--load=" X) )
                 ((= `(char "(") (char X))
                     (pack "--eval=" X) ) ) )
            (rest) ) )
      (eval Exe) ) )

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

## Retrieve document history
(be history (@Doc @Mup)
   (val @Mup @Doc mup) )

(be history (@Doc @Mup)
   (^ @C (box (; (-> @Doc) mup)))
   (_history @C @Mup) )

(be _history (@C @Prv) (^ @ (not (; (val (-> @C)) <))) T (fail))
(be _history (@C @Prv) (^ @Prv (set (-> @C) (; (val (-> @C)) <))))
(repeat)

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

## Replacement for Emacs Lisp read syntax for strings with text properties.
## Print a leading hashtag when evaluated.
(de hashtag (Lst)
   (pack "#" (sym Lst)) )

## ** Render document

## *** Render document as HTML

## let ox-html.el render Org-file
(de oxRender (Path)
   (emx '(echo)
      "(require 'ox)"
      (pack
         "(with-temp-buffer "
         "(insert-file-contents "
         "\"" Path "\")"
         "(org-mode)"
         "(let ((org-cycle-silently 1))"
         "(princ "
         "(format \"%s\" "
         "(org-export-as 'html nil nil t)))))" ) ) )

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

(de renderBlock (Fun)
   (let (N 0  C)
      (loop
         (NIL (setq C (char)))
         (T (and (=0 N) (= "}" C)))
         (case (Fun C)
            ("\\" (Fun (char)))
            ("{" (inc 'N))
            ("}" (dec 'N)) ) ) ) )

## *** Render document as LaTeX

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

## ** Export document
## *** Produce LaTeX document

(de latex (This)
   (ifn (ext? This)
      (noContent)
      (httpHead "application/x-latex" 20 (pack (or (: doc nm) "untitled") ".tex"))
      (ht:Out *Chunked (renderLatex This)) ) )

## *** Produce PDF document

(de pdf (This)
   (ifn (ext? This)
      (noContent)
      (let Nm (or (: doc nm) "untitled")
         (out (tmp Nm ".tex") (renderLatex This))
         (chdir (tmp)
            (in (list "pdflatex" "-interaction=batchmode" Nm))
            (httpEcho (pack Nm ".pdf") "application/pdf" 20) ) ) ) )

## iorg/lib.l ends here
