<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студентка: Шевчук Ярослава Олегівна КВ-11<p>
<p align="right">Рік: 2024<p>

## Загальне завдання:
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом. Вимоги до функцій:

1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).

2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.

3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.

4. Не допускається використання псевдофункцій (деструктивного підходу).

5. Не допускається використання циклів.

Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів.

Додатковий бал за лабораторну роботу можна отримати в разі виконання всіх наступних
умов:
- робота виконана до дедлайну (включно з датою дедлайну);
- крім основних реалізацій функцій за варіантом, також реалізовано додатковий
варіант однієї чи обох функцій, який працюватиме швидше за основну реалізацію,
не порушуючи при цьому перші три вимоги до основної реалізації (вимоги 4 і 5
можуть бути порушені), за виключенням того, що в разі необхідності можна також
використати стандартну функцію copy-list.

## Варіант 10(25):
1. Написати функцію <b>group-triples</b>, яка групує послідовні трійки 
елементів у списки:
```
CL-USER> (group-triples '(a b c d e f g))
((A B C) (D E F) (G))
```
2. Написати функцію <b>list-set-intersection-3</b> , яка визначає перетин трьох множин,
заданих списками атомів:
```
CL-USER> (list-set-intersection-3 '(1 2 3 4) '(3 4 5 6) '(1 3 4 6))
(3 4) ; порядок може відрізнятись
```
## Лістинг функції group-triples:
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```
(test-function-1) 
Function 1 
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
NIL
```
## Лістинг функції list-set-intersection-3
```lisp
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

```
### Тестові набори
```lisp
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
```
### Тестування
```
(test-function-2)
Function 2
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
Passed!!!! test-6
NIL
```
### Додаткове завдання 
```lisp
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
```
### Тестові набори
```lisp
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
```
### Тестування
```
(test-function-fast)
Optimized Function
Passed!!!! test-1
Passed!!!! test-2
Passed!!!! test-3
Passed!!!! test-4
Passed!!!! test-5
NIL
```
