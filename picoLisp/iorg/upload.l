## * iorg/upload.l --- Upload for iOrg
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

## * Code

## ** Permissions

(must "Upload" Upload)

## ** Functions

(menu "Upload"
   (<h3> NIL "Upload")
   (form NIL
      (<grid> 2
         (gui '(+UpField) 30)
         (gui '(+Drop +Button) '(field -1) "Upload Logo (logo.png)"
            '(when (: drop)
               (call 'cp @ "iorg/pics/logo.png") ) )
         (gui '(+UpField) 30)
         (gui '(+Drop +Button) '(field -1) "Upload CSS File (iorg.css)"
            '(when (: drop)
               (call 'cp @ "iorg/css/iorg.css") ) ) ) ) )

## iorg/upload.l ends here
