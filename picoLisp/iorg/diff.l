## * iorg/diff.l --- Diff for iOrg
## ** MetaData 
##   :PROPERTIES:
##   :copyright: Alexander_Burger Thorsten_Jolitz
##   :copyright-since: 2012
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

## * Code
## ** Permissions

(must "Document" Iorg)

## ** Functions

(menu "Difference"
   (form NIL
      (<h2> NIL (<id> "Difference"))
      (<$> (nm> (: obj <)) (: obj <))
      "  ->  "
      (<$> (nm> (: obj)) (: obj))
      (<pre> NIL
         (in (list 'diff (blob (: obj <) 'txt) (blob (: obj) 'txt))
            (ht:Prin (till NIL T)) ) ) ) )

## iorg/diff.l ends here
