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

(require 'inferior-picolisp)

(eval-when-compile (require 'cl))

;; * Mode and Exporter definitions
;; ** Mode definitions
;; ** Exporter backend definitions
;; * Variables
;; ** Consts


;; ** Vars

(defvar iorg-scrape-connection nil
  "Holds the process object of the scrape connection.")

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

;; ** Commands

(defun iorg-scrape-connect
  (&optional name buffer-or-name program host port how)
  "Setup connection or return existing connection to iOrg host."
  (if (and (processp iorg-scrape-connection)
           (process-live-p iorg-scrape-connection))
      iorg-scrape-connection
    (let ((nm (or name "iorg-scrape"))
          (buf (or buffer-or-name
                   (generate-new-buffer-name "*iorg-scrape*")))
          (prg (or program "pil"))
          (hst (or host "localhost"))
          (prt (or port 5000))
          (process-connection-type nil))
      (setq iorg-scrape-connection
            (start-process nm buf prg
                           "-load \"@lib/http.l\" \"@lib/scrape.l\""
                           ;; (and args (mapconcat 'car args " "))
                           (format "-scrape \"%s\" %s %s"
                                   hst prt (or how "")))))))

;; workhorse function
(defun iorg-scrape-go (&optional how fun host port &rest args)
  "Call FUN on webpage accessed with HOW request at HOST:PORT"
  (interactive "sHow: \nsFun: \nsHost: \nsPort: \nArgs: ")
  (let ((hst (or host "localhost"))
        (prt (or port 5000))
        (fn (or fun "display")))
    ;; (call-process "pil"
    ;;               nil t nil
    ;;               (format "-%s %s %s %s" fun host port
    ;;                       how)))
    (call-process "pil"  nil t nil
                  "-load \"@lib/http.l\" \"@lib/scrape.l\""
                  ;; "-debug 'click"
                  ;; "-trace 'display"
                  (format "-scrape \"%s\" %s" ;  \"%s\""
                          hst prt); how)
                  "-display"
                  ;; "-enter 2 \"admin\""
                  ;; "-enter 3 \"admin\""
                  ;; "-press \"login\""
                  "-bye")))

                  ;; (format "-scrape \"%s\" %s \"%s\""
                  ;;         host port how)

                  (format "-%s %s"
                          fun
                          (mapconcat
                           (lambda (arg)
                             (concat "\"" arg "\""))
                           args " "))
                  ;; "-scrape \"localhost\" 5000 \"!iorg?home\""
                  "-bye")))


;; convenience user commands
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



;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys
;; * Run hooks and provide

;; scrape.el ends here
