;; * iorg-scrape.el --- PicoLisp GUI-scripting ported to Emacs Lisp
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
;;   :inspiration: scrape.l
;;   :keywords: emacs-lisp picolisp gui-scripting
;;   :END:

;; ** Commentary
;; *** About iorg-scrape
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog

;; * Requires

(eval-when-compile (require 'cl))

;; * Mode and Exporter definitions
;; ** Mode definitions
;; ** Exporter backend definitions
;; * Variables
;; ** Consts
;; ** Vars

(defvar iorg-scrape-scr-host nil
  "Server host")

(defvar iorg-scrape-scr-port nil
  "Server port")

(defvar iorg-scrape-scr-gate nil
  "Server gate")

(defvar iorg-scrape-links nil
  "Page links")

(defvar iorg-scrape-forms nil
  "Page forms")

(defvar iorg-scrape-buttons nil
  "Page buttons")

(defvar iorg-scrape-fields nil
  "Page fields")

(defvar iorg-scrape-errors nil
  "Errors")

;; ** Hooks
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions

(defun iorg-scrape-expect (cons-cell)
    "Check expected result")

(defun iorg-scrape-click (lbl cnt)
    "Click on link")

(defun iorg-scrape-press (lbl cnt)
    "Press a button")

(defun iorg-scrape-value (fld cnt)
    "Retrieve a fields value")

(defun iorg-scrape-enter (fld str cnt)
    "Set a fields value")

(defun iorg-scrape-display ()
    "Inspect current page")

(defun iorg-scrape--target (lst lbl cnt)
    "Find target")

(defun iorg-scrape--field (fld cnt)
    "Find field")


;; ** Commands

(defun iorg-scrape-go (host port how)
  "Scape webpage accessed with HOW request at HOST:PORT"
  (interactive "sHost: \nsPort: \nsHow: ") )


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys
;; * Run hooks and provide

;; scrape.el ends here
