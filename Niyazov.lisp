
;Задача №5 
;Определите функцию,которая увеличивает элементы исходного списка на единицу
(defun _reverse (lst lst1)
    (cond
        ((null lst) lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)

(defun increase (x y) 
    (cond 
        ((null x) y)
        (T (increase (cdr x) (cons (+ (car x) 1) y)))
    )
)

(defun task (lst)
    (_reverse (increase lst ()) ())
)
    
(print (task '(10 5 4)))

;Задача №12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.

(defun mrg (lst)
    (cond ((null lst) lst)
          ((equal (car lst) (cadr lst)) (cons (car lst) (mrg (cddr lst))))
          (t (cons (car lst) (mrg (cdr lst))))
    )
)

(print (mrg '(1 1 1 2 2 2 3 3 3)))
(print (mrg '(a b a b a a b b a)))


;Задача №13

(defun _member (x y)
    (cond
        ((null y) nil)
        ((equal x (car y)) x)
        (t (_member x (cdr y)))
    )
)

(defun double (w)
    (cond 
        ((null w) nil)
        ((_member (car w) (cdr w)) (double (cdr w)))
        ((cons (car w) (double (cdr w))))
    )
)


(print (double '(a b b a a)))
(print (double '(123 css sscc css css a a)))

;Задача №14
;Определите функцию,осуществляющую перестановку двух элементов списка с заданными номерами.
(defun _reverse (lst lst1)
    (cond 
        ((Null lst)  lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)

(defun my_find (lst n cnt) ; нахождение элемента
    (cond
        ((equal cnt n) (car lst))
        (t (my_find (cdr lst) n (+ cnt 1)))
        )
    )

(defun both (lst i1 i2) 
    (list (my_find lst i1 1) (my_find lst i2 1))
    )

(defun swap (lst output i1 i2 v1 v2 cnt)
    (cond
        ((null lst) (_reverse output ())) ; вывод
        ((equal cnt 0) (swap lst output i1 i2 (cadr (both lst i1 i2)) (car (both lst i1 i2)) (+ cnt 1) ) ) ; инициализация
        ((equal cnt i1) (swap (cdr lst) (cons v1 output) i1 i2 v1 v2 (+ cnt 1))) ; замена
        ((equal cnt i2) (swap (cdr lst) (cons v2 output) i1 i2 v1 v2 (+ cnt 1)))
        (t (swap (cdr lst) (cons (car lst) output) i1 i2 v1 v2 (+ cnt 1))) 
        )
    )

(print (swap '(8 7 5 3 2 1 4 6) () 3 5 0 0 0))

;Задача №22
;Определите функцию,которая обращает список(аbс) и разбивает его на уровни (((с)b)а)
(defun conv (lst r)
   (cond 
       ((null lst) (car r))
       (t (conv (cdr lst) (list (append r (list (car lst))))))
   )
)

(defun _reverse (lst lst1)
    (cond
        ((null lst) lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)
 
(defun task (lst) 
    (conv (_reverse lst ()) ())
)
 

 
(print (task '(a b c)))


