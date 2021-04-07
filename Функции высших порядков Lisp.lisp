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

;Задача №4
;Определите функциональный предикат (КАЖДЫЙ пред список), который истинен в том и только в том случае, когда, являющейся функциональным аргументом предикат пред истинен для всех элементов списка список

(defun each1 (p l)
  (cond ((null l) t)
        ((funcall p (car l)) (each1 p (cdr l)))
        (t nil)))


(print "Задача №4")
(print "Определите функциональный предикат (КАЖДЫЙ пред список), который истинен в том и только в том случае, когда, являющейся функциональным аргументом предикат пред истинен для всех элементов списка список")
(print (each1 'atom '(a b c)))
(print (each1 'numberp '(1 2 a 3)))
(print (each1 'atom '(1 (a b c))))

