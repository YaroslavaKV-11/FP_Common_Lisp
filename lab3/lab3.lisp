(defun selection-sorting-1 (lst)                                          ; Функція реалізує сортування вибором за незменшенням (функціональний підхід)
  (if (null lst)                                                          ; Якщо список порожній, повертаємо порожній список
      nil
      (let ((m (find-min lst)))                                           ; Знаходимо мінімальний елемент у списку
        (cons m (selection-sorting-1 (remove m lst :count 1))))))         ; Рекурсивно сортуємо список без мінімального елемента

(defun find-min (lst)                                                     ; Функція знаходить мінімальний елемент у списку
  (if (null lst)                                                          ; Якщо список порожній, повертаємо nil
      nil
      (finding-min (car lst) (cdr lst))))                                 ; Викликаємо допоміжну функцію для пошуку мінімуму

(defun finding-min (curr-min remainder)                                   ; Допоміжна функція для рекурсивного пошуку мінімального елемента
  (if (null remainder)                                                    ; Якщо залишків списку більше немає
      curr-min                                                            ; Повертаємо поточний мінімум
      (let ((x (car remainder)))                                          ; Отримуємо поточний елемент
        (finding-min (if (< x curr-min) x curr-min) (cdr remainder)))))   ; Оновлюємо мінімум і обробляємо решту списку

(defun check-selection-sorting-1 (name input expected)
  (format t "~:[FAILED~;Passed~]!!! ~a~%" 
          (equal (selection-sorting-1 input) expected) 
          name))

(defun test-selection-sorting-1 ()
  (check-selection-sorting-1 "test 1" '(7 3 9 2 6 1 8) '(1 2 3 6 7 8 9)) 
  (check-selection-sorting-1 "test 2" '(10 -5 0 3 -2 8 1) '(-5 -2 0 1 3 8 10))
  (check-selection-sorting-1 "test 3" '(1 1 1 1 1) '(1 1 1 1 1)) 
  (check-selection-sorting-1 "test 4" '(100 50 20 10 0) '(0 10 20 50 100)) 
  (check-selection-sorting-1 "test 5" '(42) '(42))
  (check-selection-sorting-1 "test 6" nil nil))

(defun selection-sorting-2 (lst)                                         ; Функція реалізує сортування вибором за незменшенням (імперативний підхід)
  (let ((cl (copy-list lst))                                             ; Створюємо копію списку, щоб не змінювати оригінал
        (length (list-length lst)))                                      ; Отримуємо довжину списку
    (dotimes (start length)                                              ; Проходимо по всіх елементах списку
      (let ((min-val (nth start cl))                                     ; Припускаємо, що мінімум — це поточний елемент
            (min-idx start))                                             ; Індекс мінімального елемента
        (dotimes (i (- length (1+ start)))                               ; Переглядаємо решту списку
          (let ((candidate (nth (+ start 1 i) cl)))                      ; Отримуємо поточний елемент
            (when (< candidate min-val)                                  ; Якщо поточний елемент менший за мінімум
              (setf min-val candidate                                    ; Оновлюємо мінімум
                    min-idx (+ start 1 i)))))                            ; Оновлюємо індекс мінімального елемента
        (when (/= min-idx start)                                         ; Якщо мінімум не на своєму місці
          (rotatef (nth start cl) (nth min-idx cl)))))                   ; Міняємо місцями поточний елемент і мінімум
    cl))                                                                 ; Повертаємо відсортований список

(defun check-selection-sorting-2 (name input expected)
  (format t "~:[FAILED~;Passed~]!!! ~a~%" 
          (equal (selection-sorting-2 input) expected) 
          name))

(defun test-selection-sorting-2 ()
  (check-selection-sorting-2 "test 1" '(15 2 9 6 3 0) '(0 2 3 6 9 15)) 
  (check-selection-sorting-2 "test 2" '(-10 -20 -5 -1 0 5 3) '(-20 -10 -5 -1 0 3 5)) 
  (check-selection-sorting-2 "test 3" '(99 99 99 99) '(99 99 99 99)) 
  (check-selection-sorting-2 "test 4" '(3.5 2.2 1.1 4.6) '(1.1 2.2 3.5 4.6)) 
  (check-selection-sorting-2 "test 5" '(42) '(42)) 
  (check-selection-sorting-2 "test 6" '(10 20 30 40) '(10 20 30 40)) 
  (check-selection-sorting-2 "test 7" '(50 40 30 20 10) '(10 20 30 40 50)) 
  (check-selection-sorting-2 "test 8" nil nil)) 
