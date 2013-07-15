# 08aug10abu
# (c) Software Lab. Alexander Burger

(must "Upload" Upload)

(menu "Upload"
   (<h3> NIL "Upload")
   (form NIL
      (<grid> 2
         (gui '(+UpField) 30)
         (gui '(+Drop +Button) '(field -1) "Upload Logo (logo.png)"
            '(when (: drop)
               (call 'cp @ "wiki/logo.png") ) )
         (gui '(+UpField) 30)
         (gui '(+Drop +Button) '(field -1) "Upload CSS File (wiki.css)"
            '(when (: drop)
               (call 'cp @ "wiki/wiki.css") ) ) ) ) )

# vi:et:ts=3:sw=3
