;; * iorg.el --- interactive Org-mode
;; ** MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten_Jolitz
;;   :copyright-since: 2013
;;   :version:  0.9
;;   :licence:  GPL3+
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :git-repo: https://github.com/tj64/iorg
;;   :git-clone: git@github.com:tj64/iorg.git
;;   :authors: Thorsten_Jolitz
;;   :contact: <tjolitz@gmail.com>
;;   :inspiration:  org-mode picolisp
;;   :keywords: emacs org-mode dhtml interactive_web_applications
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

;; * Mode and Exporter definitions

;; ** Mode definitions

;; FIXME  \\[iorg-link] iorg-link ??
(define-minor-mode iorg-minor-mode
   "Minor mode for Org-mode buffers generated by outorg.
There is a mode hook, and a few commands:
\\[iorg-login] iorg-login
\\[iorg-logout] iorg-logout
\\[iorg-dired] iorg-dired
\\[iorg-new] iorg-new
\\[iorg-delete] iorg-delete
\\[iorg-edit] iorg-edit
\\[iorg-done] iorg-done
\\[iorg-insert-internal-link] iorg-insert-internal-link"
  :lighter " iOrg")

(define-derived-mode iorg-dired-mode ...)

;; ** Exporter backend definitions

(org-export-define-derived-backend 'ihtml 'html ...)

;; * Variables
;; ** Consts
;; ** Vars

;; (defvar iorg-circ-obj-label-regexp
;;   "\\(#\\)\\([[:digit:]]+\\)\\(#\\|=\\)"
;;   "Regexp that matches the label of an object or the
;;   reference to such a labeled in a circular list.")

(defvar iorg-default-map-types
  (append org-element-all-elements org-element-all-objects)
  "Default types to be selected by `org-element-map'.")

(defvar iorg-default-host-path "http://localhost:5000"
  "Default path (protocol, host, port) for iOrg server.")

(defvar iorg-default-callback-function
  '(lambda (CBARGS) (princ (buffer-string)))
  "Default callback function for calls to the iOrg server.")

;; FIXME all types covered?
(defvar iorg-all-types
  (append '(org-data) org-element-all-elements
  org-element-all-objects '(plain-text))
  "Default types to be selected by `org-element-map'.")

;; ** Hooks
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions

(defsubst iorg--elisp-plist-p (lst)
  "Return non-nil if LST is a list and its car a keyword."
  (and (listp lst) (keywordp (car lst))))

(defun iorg-retrieve-url(path &optional hostpath &rest args)
  "Generic function for calling the iOrg server from Emacs.

The PicoLisp application server uses a slightly specialized syntax when
communicating URLs to and from a client. The PATH part of an URL - which
remains when

- the preceding protocol, host and port specifications (HOSTPATH)

- and the trailing question mark plus arguments (ARGS)

are stripped off - is interpreted according so some rules. The most prominent
ones are:

- If a path starts with an exclamation-mark ('!'), the rest (without the '!')
  is taken as the name of a Lisp function to be called. All arguments
  following the question mark are passed to that function.

- If a path ends with \".l\" (a dot and a lower case 'L'), it is taken as a
  Lisp source file name to be (load)ed. This is the most common case, and we
  use it in our example \"project.l\".

- If the extension of a file name matches an entry in the global mime type
  table *Mimes, the file is sent to the client with mime-type and max-age
  values taken from that table.

- Otherwise, the file is sent to the client with a mime-type of
  \"application/octet-stream\" and a max-age of 1 second.

An application is free to extend or modify the *Mimes table with the mime
function. For example

#+begin_src picolisp
 (mime \"doc\" \"application/msword\" 60)
#+end_src

defines a new mime type with a max-age of one minute.

Argument values (ARGS) in URLs, following the path and the question mark, are
encoded in such a way that Lisp data types are preserved:

- An internal symbol starts with a dollar sign ('$')

- A number starts with a plus sign ('+')

- An external (database) symbol starts with dash ('-')

- A list (one level only) is encoded with underscores ('_')

- Otherwise, it is a transient symbol (a plain string)

In that way, high-level data types can be directly passed to functions encoded
in the URL, or assigned to global variables before a file is loaded. The
PicoLisp application-framework uses a somewhat specialised syntax when
communicating URLs."
  (let* ((base-url (or hostpath iorg-default-host-path))
         (url-no-args
          (concat base-url "/" path))
         (url (if args
                  (concat url-no-args
                          "?"
                          (mapconcat 'identity args "&"))
                url-no-args)))
    (car
     (read-from-string
      (with-current-buffer
          (url-retrieve-synchronously url)
        (buffer-substring-no-properties
         (point-min) (point-max)))))))

    ;; (with-current-buffer (url-retrieve url nil)
    ;;   (buffer-string))))

;; (defun kvplist->alist (plist)
;;   "Convert PLIST to an alist.

;; The keys are expected to be :prefixed and the colons are removed.
;; The keys in the resulting alist are symbols."
;;   ;; RECURSION KLAXON
;;   (when plist
;;     (destructuring-bind (key value &rest plist) plist
;;       (cons `(,(keyword->symbol key) . ,value)
;;             (kvplist->alist plist)))))

(defun iorg--add-elem-id (tree)
  "Add ':elem-id' property to each element of parse TREE."
  (let ((counter 1))
    (org-element-map tree (append '(org-data) iorg-default-map-types)
      (lambda (elem)
        (unless (eq (org-element-type elem) 'org-data)
          (org-element-put-property elem :elem-id counter))
        (setq counter (1+ counter)))))
  tree)

(defun iorg--unwind-circular-list (tree)
  "Replace circular links with unique ID's in parse TREE."
    (org-element-map tree iorg-all-types
      (lambda (elem)
        (org-element-put-property
         elem :parent
         (let ((par (org-element-property :parent elem)))
           (if (eq (org-element-type par) 'org-data)
               0
             (org-element-property :elem-id par))))))
    tree)

(defun iorg--elisp-plist-keys (plist)
  "Return a list with all keywords in PLIST."
  (and (iorg--elisp-plist-p plist)
       (remove
        t
        (mapcar
         (lambda (--elem)
           (or (not (keywordp --elem)) --elem))
         plist))))

(defun iorg--nil-and-t-to-uppercase (tree)
  "Takes a parse TREE as string and upcases nil and t."
  (and (stringp tree)
       (replace-regexp-in-string
        (concat
         "\\( t \\|(t \\| t)\\|(t)\\|"
         " nil \\|(nil \\| nil)\\|(nil)\\)")
        '(lambda (match)
           (cond
            ((string= match " t ")
             (format "%s" " T "))
            ((string= match "(t ")
             (format "%s" "(T "))
            ((string= match " t)")
             (format "%s" " T)"))
            ((string= match "(t)")
             (format "%s" "(T)"))
            ((string= match " nil ")
             (format "%s" " NIL "))
            ((string= match "(nil ")
             (format "%s" "(NIL "))
            ((string= match " nil)")
             (format "%s" " NIL)"))
            ((string= match "(nil)")
             (format "%s" "(NIL)"))))
        tree)))

(defun iorg--convert-plists-to-picolisp (tree)
  "Convert all elements plists in TREE to PicosLisp plists.

Assume TREE is a parse-tree produced by `org-element-parse-buffer' and turned
into a non-circular list by applying `iorg--add-elem-id' and
`iorg--unwind-circular-list' on it. Return the parse-tree in PicoLisp
compliant form."
  (org-element-map
      tree
      iorg-all-types
      ;; iorg-default-map-types
    (lambda (elem)
      ;; (let ((type (org-element-type elem)))
      ;;   ;; (and (not (eq type 'plain-text))
      ;;        ;; (not (eq type 'org-data))
      (let* ((plist (cadr elem))
             (props (if (iorg--elisp-plist-p plist)
                        (iorg--elisp-plist-keys plist)
                      (error "%s is not an Emacs Lisp plist"
                             plist))))
        (message "%s"
        (cons (car elem)
              (list
               (mapcar
                (lambda (prop)
                  (cons (org-element-property prop elem) prop))
                props)))))))
  ;; 'iorg--transform-elements-plist-to-picolisp-plist)
  (message "%s" tree)
  tree)

;; (defun iorg--transform-elements-plist-to-picolisp-plist (elem)
;;   "Convert elisp plist of ELEM into PicoLisp plist."
;;   (let ((type (org-element-type elem)))
;;     (and type
;;          (not (eq type 'plain-text))
;;          ;; (not (eq type 'org-data))
;;          (let* ((plist (cadr elem))
;;                 (props (if (iorg--elisp-plist-p plist)
;;                            (iorg--elisp-plist-keys plist)
;;                          (error "%s is not an Emacs Lisp plist"
;;                                 plist))))
;;            (cons (car elem)
;;                  (mapcar
;;                   (lambda (prop)
;;                     (cons (org-element-property prop elem) prop))
;;                   props))))))

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
  (let* ((map? (and args
                    (or
                     (plist-get args :types)
                     (plist-get args :fun)
                     (plist-get args :info)
                     (plist-get args :first-match)
                     (plist-get args :no-recursion)
                     (plist-get args :with-affiliated))))
         (print-circle t)
         (buf (or (and buffer-or-file
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
                  iorg-all-types))
         (fun (or (and args (plist-get args :fun)) 'identity))
         (inf (and args (plist-get args :info)))
         (1st-match (and args (plist-get args :first-match))) 
         (no-recur (and args (plist-get args :no-recursion))) 
         (with-affil (and args (plist-get args :with-affiliated))))
    (iorg--nil-and-t-to-uppercase
     (format "%s"
     (iorg--convert-plists-to-picolisp
      (iorg--unwind-circular-list
       (iorg--add-elem-id
        (if map?
            (org-element-map
                dat typ fun inf 1st-match no-recur with-affil)
          dat))))))))

(defun iorg-pico-to-org ()
  "")

;; ** Commands

(defun iorg-set-default-host-path (path)
  "Change `iorg-default-host-path' temporarily.

The new PATH will remain valid until set again or until `iorg.el'
is loaded again. In the latter case it will be reset to
\"http://localhost:5000\"."
 (interactive "sURL (e.g. http://localhost:5000): ")
 (setq iorg-default-host-path path))


(defun iorg-set-default-callback-function (fun)
  "Change `iorg-default-callback-function' temporarily.

The new function FUN will remain valid until set again or until
`iorg.el' is loaded again. In the latter case it will be reset to
'(lambda (CBARGS) (prin (buffer-string)))."
 (interactive "xFun: (e.g. (lambda (CBARGS) (prin (buffer-string))): ")
 (setq iorg-default-callback-function fun))

(defun iorg-login ()
  "")

(defun iorg-logout ()
  "")

(defun iorg-dired ()
  "")

(defun iorg-new ()
  "")

(defun iorg-delete ()
  "")

(defun iorg-edit ()
  "")

(defun iorg-done ()
  "")

(defun iorg-insert-internal-link ()
  "Insert internal-link in PicoLisp-Wiki syntax.

Such a link can take two forms:

 1. ={target}
 2. ={target label}

where 'target' is the name of the wiki document linked to and
'label' is the text that will be shown as clickable link when the
document is rendered in the wiki."
  (interactive)
  )


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys
;; * Run hooks and provide
