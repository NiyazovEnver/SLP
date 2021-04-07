;Задача №2
;Определите функицонал (MAPLIST fn список) для одного списочного аргумента

(defun my_MAPLIST (f args)
    (cond
        ((NULL args) nil)
        (t (cons (APPLY f args) (my_MAPLIST f (cdr args))))
    )
)

(print "Задача №2. Определите функицонал (MAPLIST fn список) для одного списочного аргумента")
(print (my_MAPLIST '* '(1 2 3 4 5 6) ))
(print (my_MAPLIST '+ '(1 2 3 4 5 6) ))
(print (my_MAPLIST '- '(1 2 3 4 5 6) ))
