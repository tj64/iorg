## * iorg/lib.l --- utility functions for iOrg
## ** MetaData
##   :PROPERTIES:
##   :copyright: Thorsten Jolitz
##   :copyright-years: 2013
##   :version:  0.9
##   :licence:  GPL 2 or later (free software)
##   :licence-url: http://www.gnu.org/licenses/
##   :part-of-emacs: no
##   :author: Thorsten Jolitz
##   :author_email: tjolitz AT gmail DOT com
##   :inspiration:  org-babel picolisp
##   :keywords: emacs org-mode dhtml interactive-web-applications
##   :END:

## ** Commentary
## convenience functions for iOrg, especially for the interaction with
## Emacs/Emacs Lisp

## * Calling Emacs
## ** Info
## *** Action Arguments

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

## *** Initial Options

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

## * Code
## ** Namespace

(symbols 'iorg 'pico)

## ** Functions

## convert an elisp plist like (:key val :key val) into a
## PicoLisp plist ((val . :key) (val . :key))
(de elisp-plist-to-picolisp-plist (plist) ...)
   
## is symbol S a keyword-symbol like e.g. `:begin'?
(de keyword-symbol? (S)
   (and
      (sym? S)
      (= (car (chop S)) ":") ) )

## generic function for calling Emacs from PicoLisp
## courtesy of Alexander Burger
(de emx @
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
      (read) ) )
