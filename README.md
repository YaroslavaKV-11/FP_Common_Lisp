<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 1</b><br/>
"Обробка списків з використанням базових функцій"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студентка: Шевчук Ярослава Олегівна КВ-11<p>
<p align="right">Рік: 2024<p>

## Загальне завдання

### Пункт 1
Створіть список з <b>п'яти елементів</b>, використовуючи функції LIST і CONS . Форма створення списку має бути одна — використання SET чи SETQ (або інших допоміжних форм) для збереження проміжних значень не допускається. Загальна кількість елементів (включно з підсписками та їх елементами) не має перевищувати 10-12 шт. (дуже великий список робити не потрібно). Збережіть створений список у якусь змінну з SET або SETQ . Список має містити (напряму або у підсписках): 

- хоча б один символ
- хоча б одне число
- хоча б один не пустий підсписок
- хоча б один пустий підсписок
```
(setq first-list (cons 'a
                   (cons 1
                   (cons (list 2 3)
                   (cons '()
                   (list 'b))))))
```
Результат виконання:
```
(A 1 (2 3) NIL B)
```
### Пункт 2

Отримати голову списку :
```
(car first-list) 
```
Результат виконання:
```
A
```
### Пункт 3
Отримати хвіст списку:
```
(cdr first-list) 
```
Результат виконання:
```
(1 (2 3) NIL B)
```
### Пункт 4
Отримайте третій елемент списку:
```
(car (cdr(cdr first-list)))
```
Результат виконання:
```
(2 3)
```
### Пункт 5

Отримайте останній елемент списку:

1.
```
(car(last first-list))
```
Результат виконання:
```
B
```
2.
```
(car (cdr(cdr(cdr(cdr first-list)))))
```
Результат виконання:
```
B
```
### Пункт 6
Використайте предикати ATOM та LISTP на різних елементах списку (по 2-3
приклади для кожної функції):
1. Предикати ATOM (Функція ATOM перевіряє, чи є елемент атомом (не списком)):

    1.1. Перевіряємо перший елемент списку(який є символом, отже результат має бути Т): 
      ```
      (ATOM (car first-list) )
      ```
      Результат виконанння:
      ```
      T
      ```
    1.2. Перевіримо елемент списку, який є списком (2 3), результат має бути NIL, бо список не є атомом:
      ```
      (ATOM (car (cdr(cdr first-list))))
      ```
      Результат виконанння:
      ```
      NIL
      ```
    1.3. Також перевіримо як поводитиме себе при аргументі (), результатом має бути Т, так як пустий список - це також атом:
      ```
      (ATOM (car (cdr (cdr (cdr first-list)))))    
      ```
      Результат виконанння:
      ```
      T
      ```
2. Предикат LISTP(Дана функція визначає, чи є аргумент списком):

    2.1.  Перевіряємо перший елемент списку(який є символом, отже результат має бути NIL):
      ```
      (LISTP (car first-list) ) 
      ```
      Результат виконанння:
      ```
      NIL
      ```
    2.2. Перевіримо елемент списку, який є списком (2 3), результат має бути T, бо список є списком:
      ```
      (LISTP (car (cdr(cdr first-list))))
      ```
      Результат виконанння:
      ```
      T
      ```
    2.3. Також перевіримо як поводитиме себе при аргументі (), результатом має бути Т, так як пустий список - це також список:
      ```
      (LISTP (car (cdr (cdr (cdr first-list))))) 
      ```
      Результат виконанння:
      ```
      T
      ```
### Пункт 7
Використайте на елементах списку 2-3 інших предикати з розглянутих у розділі 4
навчального посібника:
- PLUSP(перевіряють знак числа, чи є він додатнім):
Розгялнемо два випадки перевірки, для елементу списку - числа (1) та перевірку елементу підсписку - числа (2):
  - Для числа (1):
  ```
  (plusp (car (cdr first-list))) 
  ```
  Результтат виконання:
  ```
  T
  ```
  - Для числа (2) з підсписку:
  ```
  (plusp (car (car (cdr(cdr first-list))))) 
  ```
  Результат виконанння:
  ```
  T
  ```
- EVENP(перевірка чисел на парність):
Розгялнемо два випадки перевірки, для елементу списку - числа (1) та перевірку елементу підсписку - числа (2):
   - Перевіряємо чи число 1 є парним:
   ```
   (EVENP (car (cdr first-list))) 
   ```
   Результат виконанння:
   ```
   NIL
   ```
   - Перевірямо чи число 2 з підсписка є парним:
   ```
   (EVENP (car (car (cdr(cdr first-list))))) 
   ```
   Результат виконанння:
   ```
   T
   ```
- EQUAL(можна порівнювати списки та рядки):

   - Порівняємо один і той же список (2 3):
   ```
   (EQUAL (car (cdr(cdr first-list))) (car (cdr(cdr first-list))))
   ```
   Результат виконання:
   ```
   T
   ```
   - Порівняємо два списки () та (2 3): 
   ```
   (EQUAL (car (cdr(cdr first-list))) (car (cdr (cdr (cdr first-list))))) 
   ``` 
   Результат виконання:
   ```
   NIL
   ```

### Пункт 8
Об'єднайте створений список з одним із його непустих підсписків. Для цього
використайте функцію APPEND:

Так, як у нашому списку непустий підсписок це 3 елемент списку, то:
```
(APPEND first-list (car (cdr(cdr first-list))))
```
  Результат виконання:
   ```
   (A 1 (2 3) NIL B 2 3)
   ```
## Варіант 1(25):
![var_1(25).png](https://github.com/YaroslavaKV-11/FP_Common_Lisp/blob/main/var_1(25).png)

Створіть список, що відповідає структурі списку, наведеній на рисунку (за варіантом).
Для цього допускається використання не більше двох форм.

Виконання завдання:
1.
```
(SET 'part_of_list '(B 1)) 
```
Результат виконання:
```
(B 1)
```
2.
```
(list 'A part_of_list 'C part_of_list)
```
Результат виконання:
```
(A (B 1) C (B 1))
```
