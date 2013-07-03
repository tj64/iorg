;; * iorg.el --- interactive Org-mode
;; ** MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2013
;;   :version:  0.9
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :inspiration:  org-babel picolisp
;;   :keywords: emacs org-mode dhtml interactive-web-applications
;;   :END:
;; ** Commentary
;; *** About iOrg
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog

;; * Requires

;; ;; get it from ELPA 
;; ;; or from the [[https://github.com/nicferrier/emacs-kv][github-repo]]
;; (require 'kv)
;; (require 'paredit)
(require 'org-element)

(eval-when-compile (require 'cl))

;; * Variables
;; ** Consts
;; ** Vars

(defvar iorg-circ-obj-label-regexp
  "\\(#\\)\\([[:digit:]]+\\)\\(#\\|=\\)"
  "Regexp that matches the label of an object or the
  reference to such a labeled in a circular list.")

(defvar iorg-default-map-types
  (append org-element-all-elements org-element-all-objects)
  "Default types to be selected by `org-element-map'.")

(defvar iorg-default-map-fun-org-to-pico
  '(lambda (plst) (list (car plst) (kvplist->alist (cadr plst))))
  "Default function to be mapped on selected types.

This function will be used by `org-element-map' when converting an Org-mode
parse-tree into a PicoLisp readable alist.")

(defvar iorg-default-map-fun-pico-to-org
  '(lambda (alst) (list (car alst) (kvalist->plist (cadr alst))))
  "Default function to be mapped on selected types.

This function will be used by `org-element-map' when converting a
PicoLisp readable alist into Org-mode parse-tree (plist-)
format.")

;; ** Hooks
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions

(defun iorg--wrap-in-parens-with-backquote ()
  "Wrap following SEXP in parenthesis with backquote."
  (save-excursion
    (insert "`(")
    (forward-sexp)
    (insert ")")
    (forward-char -1)
    (backward-sexp)))

;; adapted `kvplist->alist' from library `kv.el'
(defun iorg--elisp-plist-to-picolisp-plist (plist)
  "Convert elisp PLIST to an PicoLisp plist."
  (when plist
    (destructuring-bind (key value &rest plist) plist
      (cons `(,value . ,key)
            (iorg-elisp-plist-to-picolisp-plist plist)))))

;; adapted `kvalist->plist' from library `kv.el'
(defun iorg--picolisp-plist-to-elisp-plist (pico-plist)
  "Convert an picolisp plist (PICO-PLIST) to an elisp plist."
  (loop for pair in pico-plist
     append (list (cdr pair) (car pair))))

(defun iorg--circular-obj-read-syntax-to-transient-sym (tree)
  "Transform elisp circular-obj syntax into PicoLisp transient symbols.

This function expects an Org-mode buffer parse TREE as a string,
as produced e.g. by this Emacs Lisp code (unquote the
double-quotes inside the source-block before evaluating):

#+begin_src emacs-lisp
  (let ((print-circle t))
    (message
          (format \"%s\"
             (with-current-buffer
                 (find-file-noselect
                  \"/path/to/my-file.org\")
               (org-element-parse-buffer)))))
#+end_src

It replaces the Emacs Lisp read syntax for circular objects with
assignments and references to PicoLisp transient symbols. Here is
a quote form `(info \"(elisp)Circular Objects\")' about the read
syntax:

#+begin_quote
To represent shared or circular structures within a complex of Lisp
objects, you can use the reader constructs `#N=' and `#N#'.

   Use `#N=' before an object to label it for later reference;
subsequently, you can use `#N#' to refer the same object in another
place.  Here, N is some integer.  For example, here is how to make a
list in which the first element recurs as the third element:

     (#1=(a) b #1#)

#+end_quote

This read syntax is replaced with assignments to PicoLisp transient symbols
and references to these symbols:

#+begin_quote
: (1 2 (3 4 . `(setq \"n1\" (5 6))) 7 . `\"n1\")
 -> (1 2 (3 4 5 6) 7 5 6)
#+end_quote

Thus, the circular list is resolved into a regular list that can
be easier processed by the standard mapping functions."
  (with-current-buffer (get-buffer-create "*iorg-tmp-buffer*")
  ;; (with-temp-buffer
    (insert tree)
    (goto-char (point-min))
    (while (re-search-forward
            iorg-circ-obj-label-regexp
            nil 'NOERROR)
      (let ((digit (match-string 2))
            (ref-p (string= (match-string 3) "#")))
        (replace-match "")
        (unless ref-p
          ;; (paredit-wrap-sexp)
          (iorg--wrap-in-parens-with-backquote)
          ;; (save-excursion
          ;;   (forward-char -1)
          ;;   (insert (format "%s" "`"))))
          (forward-char 2))
        (insert
         (format "%s\"n%s\"%s"
                 (if ref-p "`" "setq ")
                 digit
                 (if ref-p "" " '")))))))


;; FIXME: kind of out-of-date
(defun iorg-org-to-pico
  (&optional data buffer-or-file &rest args)
  "Converts an org-element parse-tree into an alist.

Optional argument DATA should should be part of or an entire
parse-tree as returned by `org-element-parse-buffer', optional
argument BUFFER-OR-FILE is either the name of an existing
Org-mode buffer or the name of an Org-mode file.

ARGS, if given, can be any combination of the following key/value
pairs (here given with example values):

  :granularity 'object
  :visible-only t
  :types '(append org-element-all-elements
           org-element-all-objects) 
  :fun '(lambda (L) (list (car L) (kvplist->alist (cadr L))))
  :info '(:option1 value1 :option2 value2)
  :first-match t
  :no-recursion '(headline table)
  :with-affiliated t

Since `iorg-parsetree-to-alist' is just a convenience function
built on top of `org-element-parse-buffer' and `org-element-map',
the arguments are the same as for these two functions, so you can
consult their doc-strings for more information."
  (let* ((buf (or (and buffer-or-file
                       (or (get-buffer buffer-or-file)
                           (if (and
                                (file-exists-p buffer-or-file)
                                (string=
                                 (file-name-extension buffer-or-file) "org"))
                               (find-file-noselect buffer-or-file)
                             (error "File %s is not a valid Org file"
                                    buffer-or-file))))
                  (current-buffer)))
         (gran (or (and args (plist-member args :granularity)
                        (plist-get args :granularity))
                   'object))
         (vis (and args (plist-get args :visible-only)))
         (dat (or data
                  (with-current-buffer buf
                    (org-element-parse-buffer gran vis))))
         (typ (or (and args (plist-get args :types))
                       iorg-default-map-types))
         (fun (or (and args (plist-get args :fun))
                       iorg-default-map-fun-org-to-pico))
         (inf (and args (plist-get args :info)))
         (1st-match (and args (plist-get args :first-match))) 
         (no-recur (and args (plist-get args :no-recursion))) 
         (with-affil (and args (plist-get args :with-affiliated))))
    (org-element-map dat typ fun inf 1st-match no-recur with-affil)))

(defun iorg-pico-to-org ()
  "")

;; ** Commands


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys
;; * Run hooks and provide
