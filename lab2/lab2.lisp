(defun group-triples (lst)
  "Групує послідовні трійки елементів у вкладені списки."
  (cond
    ((null lst) ; Якщо список порожній, повертаємо порожній список
     nil)
    ((null (cdr lst)) ; Якщо залишився тільки один елемент
     (list (list (car lst))))
    ((null (cddr lst)) ; Якщо залишилось два елементи
     (list (list (car lst) (cadr lst))))
    (t ; Якщо є три або більше елементів
     (cons (list (car lst) (cadr lst) (caddr lst)) ; Формуємо групу трійки
           (group-triples (cdddr lst)))))) ; Рекурсивно обробляємо залишок списку

(defun check-my-function-1 (name input expected)
  "Функція для перевірки результатів роботи group-triples."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%"
          (equal (group-triples input) expected)
          name))

(defun test-function-1 ()
  "Тестові набори для group-triples."
  (format t "Function 1 ~%")
  (check-my-function-1 "test-1" '(a b c d e f g) '((A B C) (D E F) (G)))
  (check-my-function-1 "test-2" '(1 2 3 4 5) '((1 2 3) (4 5)))
  (check-my-function-1 "test-3" nil nil)
  (check-my-function-1 "test-4" '(a) '((A)))
  (check-my-function-1 "test-5" '(1 2 3) '((1 2 3)))
  (check-my-function-1 "test-6" '(x y) '((X Y))))

(defun elem-in-list-p (x lst)
  "Допоміжна функція, що перевіряє, чи є елемент `x` у списку `lst`."
  (cond
    ((null lst) nil) ; Якщо список порожній, елемент не знайдено
    ((equal x (car lst)) t) ; Якщо `x` дорівнює першому елементу списку
    (t (elem-in-list-p x (cdr lst))))) ; Рекурсивно перевіряємо решту списку

(defun list-set-intersection-3 (lst1 lst2 lst3)
  "Основна функція, що знаходить перетин трьох множин, заданих списками."
  (cond
    ((null lst1) ; Якщо перший список порожній, повертаємо порожній список
     nil)
    ((and (elem-in-list-p (car lst1) lst2) ; Якщо елемент є в усіх трьох множинах
          (elem-in-list-p (car lst1) lst3))
     (cons (car lst1) ; Додаємо елемент до результату
           (list-set-intersection-3 (cdr lst1) lst2 lst3))) ; Рекурсивно обробляємо залишок
    (t 
     (list-set-intersection-3 (cdr lst1) lst2 lst3)))) ; Пропускаємо поточний елемент

(defun check-my-function-2 (name lst1 lst2 lst3 expected)
  "Функція для перевірки результатів роботи list-set-intersection-3."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%"
          (equal (list-set-intersection-3 lst1 lst2 lst3) expected)
          name))

(defun test-function-2 ()
  "Тестові набори для list-set-intersection-3."
  (format t "Function 2 ~%")
  (check-my-function-2 "test-1" '(1 2 3 4) '(3 4 5 6) '(1 3 4 6) '(3 4))
  (check-my-function-2 "test-2" '(a b c) '(b c d) '(c d e) '(C))
  (check-my-function-2 "test-3" '(x y z) '(a b c) '(1 2 3) nil)
  (check-my-function-2 "test-4" nil '(1 2 3) '(1 2 3) nil)
  (check-my-function-2 "test-5" '(1 2 3) '(1 2 3) nil nil)
  (check-my-function-2 "test-6" '(1 2 3) '(3 4 5) '(3) '(3)))

(defun group-triples-fast (lst)
  (let ((res nil)
        (copy (copy-list lst)))  ; Створення копії списку
    (do ((rest copy (cdddr rest)))  ; Використовуємо цикл do для обходу списку
        ((null rest) (nreverse res))  ; Коли список порожній, повертаємо результат
      (cond
        ((null (cdr rest))
         (setq res (cons (list (car rest)) res)))  ; Якщо залишився лише один елемент
        ((null (cddr rest))
         (setq res (cons (list (car rest) (cadr rest)) res)))  ; Якщо залишилось два елементи
        (t
         (setq res (cons (list (car rest) (cadr rest) (caddr rest)) res)))))))  ; Додаємо трійку елементів

(defun check-my-function-fast (name input expected)
  "Функція для перевірки результатів роботи group-triples-fast."
  (format t "~:[Failed....~;Passed!!!!~] ~a~%"
          (equal (group-triples-fast input) expected)
          name))

(defun test-function-fast ()
  "Тестові набори для group-triples-fast."
  (format t "Optimized Function ~%")
  (check-my-function-fast "test-1" '(a b c d e f g) '((A B C) (D E F) (G)))
  (check-my-function-fast "test-2" '(1 2 3 4 5 6 7 8) '((1 2 3) (4 5 6) (7 8)))
  (check-my-function-fast "test-3" '(a b) '((A B)))
  (check-my-function-fast "test-4" nil nil)
  (check-my-function-fast "test-5" '(a) '((A))))
