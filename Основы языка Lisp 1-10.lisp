(defun _member (a l)
    (cond
        ((null l) nil) ; элемент не может принадлежать пустому множеству
        ((eq a (car l)) t) ; элемент принадлежит множеству, если в нем содержится
        (t (_member a (cdr l))) 
    )
)


(defun _reverse (lst lst1)
    (cond
        ((null lst) lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)

;Задача №5 
;Определите функцию,которая увеличивает элементы исходного списка на единицу

(defun increase (w)
    ((lambda (f1 f2)
        (cond ((null w) nil)
            ((atom f1) (cons (+ f1 1) (increase f2)))
            ((cons (increase f1) (increase f2)))
        ))
        (car w)
        (cdr w)
     )
)

(print "Задача №5. Определите функцию,которая увеличивает элементы исходного списка на единицу")
(print (increase '(1 2 3)))
(print (increase '((1 (2 3)) ((4 (5) 6)) ((7 8) 9))))
(print (increase nil))

;Задача №12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.

(defun mrg (lst)
    ((lambda (f1)
        (cond ((null lst) lst)
          ((equal f1 (cadr lst)) (cons f1 (mrg (cddr lst))))
          (t (cons f1 (mrg (cdr lst))))
        ))
        (car lst)
     )
)

(print ";Задача №12 Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.")
(print (mrg '(1 1 1 2 2 2 3 3 3)))
(print (mrg '(a b a b a a b b a)))
(print (mrg '((a b) (a b) a b a a b b a)))




;Задача №13
;Определите функцию, удаляющую в исходном списке все повторные вхождеению элементов.

(defun double (w)
    ((lambda (f1 f2)
        (cond 
            ((null w) nil)
            ((_member f1 f2) (double f2))
            ((cons f1 (double f2)))
        ))
        (car w)
        (cdr w)
    )
)

(print ";Задача №13. Определите функцию, удаляющую в исходном списке все повторные вхождеению элементов.")
(print (double '(a b b a a)))
(print (double '(123 css sscc css css a a)))
(print (double '(a (b c) 1 1 3 6)))


;Задача №14
;Определите функцию,осуществляющую перестановку двух элементов списка с заданными номерами.

(defun SWAP-TWO-ELEMENT (L N1 N2)
    (cond
        ((equal N1 N2) L)
        ((> N1 N2) (SWAP-TWO-ELEMENT L N2 N1))
        ((> N1 0) 
            (cons (car L) (SWAP-TWO-ELEMENT (cdr L) (- N1 1) (- N2 1))))
        ((> N2 1)
            ((lambda (result)
                (cons (car result) (cons (cadr L) (cdr result)))
             )
             (SWAP-TWO-ELEMENT (cons (car L) (cddr L)) (- N1 1) (- N2 1))
            )
        )
        
        (T 
            (cons (cadr L) (cons (car L) (cddr L))))
    )
)

(print "Задача №14. Определите функцию,осуществляющую перестановку двух элементов списка с заданными номерами.")
(print (SWAP-TWO-ELEMENT '(1 2 3) 1 2))
(print (SWAP-TWO-ELEMENT '(1 2 (4 5 6)) 1 2))
(print (SWAP-TWO-ELEMENT '(1 2 3) 2 0))
(print (SWAP-TWO-ELEMENT '(1 2 3) 2 2))

;Задача №22
;Определите функцию,которая обращает список(аbс) и разбивает его на уровни (((с)b)а)

(defun F (lst)
  ((lambda (f1)
      (cond 
          ((null f1) lst)
          (t (list (F f1) (car lst)))
      ))
      (cdr lst)
    )
)

(print ";Задача №22. Определите функцию,которая обращает список(аbс) и разбивает его на уровни (((с)b)а)")
(print (F '(a b c)))
(print (F '((a b c) b c)))
(print (F '(nil b c)))

;Задача №37
;Определите функцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств, т.е. множество из их общих элементов


(defun _intersection (a b)
    ((lambda (f1 f2)
        (cond
            ((null a) nil)
            ((null b) nil)
            ((member f1 b) (cons f1 (_intersection f2 b)) )
            (t (_intersection f2 b))
        ))
        (car a)
        (cdr a)
     )
)

(print "Задача №37. Определите функцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств, т.е. множество из их общих элементов")
(print (_intersection '(a b c) '(b c d)))
(print (_intersection '(a ) '( d) ))
(print (_intersection '(a ) 'nil ))

;Задача №40
;Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е. удаляющую из первого множества все общие со вторым множеством элементы.

(defun _set-difference (w v)
   ((lambda (f1 f2)
      (cond ((null w) w)
            ((_member f1 v) (_set-difference f2 v))
            ((cons f1 (_set-difference f2 v)))
      ))
      (car w)
      (cdr w)
    )
)

(print ";Задача №40 Определите функцию РАЗНОСТЬ, формирующую разность двух множеств, т.е. удаляющую из первого множества все общие со вторым множеством элементы.")
(print (_set-difference '(4 5 6 7) '(4 5 6 7)))
(print (_set-difference '(1 2 3 4 5) '(4 5 6 7)))
(print (_set-difference nil '(1 2 3)))

;Задача №43
;Определите функцию, подсчитывающую количество всех вершин данного дерева заданной высоты

;                                      7
;                                 /          \
;                           6                     12
;                       /       \              /      \
;                     2            -8        nil      nil
;                  /   \         /     \
                 nil    nil     nil    nil              
(defun tree-count (tree lv)
  (cond 
      ((null tree) 0)
      ((equal lv 0) 1)
      (t (+ (tree-count (car tree) (- lv 1)) (tree-count (caddr tree) (- lv 1))))
  )
)
 
(print "Задача №43 Определите функцию, подсчитывающую количество всех вершин данного дерева заданной высоты")
(print (tree-count '(((nil 2 nil) 6 (nil -8 nil)) 7 (nil 12 nil)) 1))
 
(print (tree-count '(((nil 2 nil) 6 (nil -8 nil)) 7 (nil 12 nil)) 2))
 
(print (tree-count '(((nil 2 nil) 6 (nil -8 nil)) 7 (nil 12 nil)) 3))
 
;Задание №48. 
;Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.

(defun ИМЕЕТ-СВОЙСТВО (symbol property &optional (propertyList (symbol-plist symbol)))
    (cond
       ((NULL propertyList) nil)
       ((equal (car propertyList) property) t)
       (t (ИМЕЕТ-СВОЙСТВО symbol property (cddr propertyList)))
        )
    )


(SETF (GET 'symbol 'property1) nil)
(SETF (GET 'symbol 'property2) 'value2)

(print "Задание №48. Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.")

(print (ИМЕЕТ-СВОЙСТВО 'symbol 'property1))
(print (ИМЕЕТ-СВОЙСТВО 'symbol 'property2))
(print (ИМЕЕТ-СВОЙСТВО 'symbol 'property3))

