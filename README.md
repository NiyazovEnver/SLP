# SLP
Задача №5 

(defun _reverse (lst &optional lst1)
    (cond
        ((null lst) lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)

(defun increase (x &optional y) 
    (cond 
        ((null x) y)
        (T (increase (cdr x) (cons (+ (car x) 1) y)))
    )
)

(defun task (lst)
    (_reverse (increase lst))
)
    
(print (task '(10 5 4)))

Задача №12

(defun mrg (lst)
    (cond ((null lst) lst)
          ((equal (car lst) (cadr lst)) (cons (car lst) (mrg (cddr lst))))
          (t (cons (car lst) (mrg (cdr lst))))
    )
)

(print (mrg '(1 1 1 2 2 2 3 3 3)))
(print (mrg '(a b a b a a b b a)))


Зажача №13

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

Задача №22

(defun conv (lst &optional r)
   (cond 
       ((null lst) (car r))
       (t (conv (cdr lst) (list (append r (list (car lst))))))
   )
)

(defun _reverse (lst &optional lst1)
    (cond
        ((null lst) lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)
 
(defun task (lst) 
    (conv (_reverse lst))
)
 

 
(print (task '(a b c)))
