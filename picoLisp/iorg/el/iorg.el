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

;; (defvar iorg-circ-obj-label-regexp
;;   "\\(#\\)\\([[:digit:]]+\\)\\(#\\|=\\)"
;;   "Regexp that matches the label of an object or the
;;   reference to such a labeled in a circular list.")

(defvar iorg-default-map-types
  (append org-element-all-elements org-element-all-objects)
  "Default types to be selected by `org-element-map'.")

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


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys
;; * Run hooks and provide
