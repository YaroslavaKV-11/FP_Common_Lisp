(defun selection-sorting (lst &key (key #'identity) (test #'<))
  "Реалізація сортування вибором з використанням ключових параметрів key і test."
  (if (null lst)                                                           ; Якщо список порожній
      nil                                                                  ; Повертаємо порожній список
      (let ((min-element (reduce (lambda (x y)                             ; Шукаємо мінімальний елемент
                                   (if (funcall test (funcall key x) (funcall key y))
                                       x
                                       y))
                                 lst)))
        (cons min-element                                                  ; Додаємо мінімальний елемент до результату
              (selection-sorting (remove min-element lst :count 1)         ; Рекурсивно викликаємо сортування для залишку
                                :key key
                                :test test)))))

(defun check-my-selection-func (name input expected &key (key #'identity) (test #'<))
  "Функція для перевірки результату сортування вибором."
  (let ((result (selection-sorting input :key key :test test)))
    (format t "~:[Failed....~;Passed!!!!~] ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "  Expected: ~a~%  Got: ~a~%~%" expected result))))

(defun test-selection-func ()
  "Набір тестів для функції сортування вибором."
  (format t "Testing selection-sorting ~%")

  ;; Тести за замовчуванням
  (check-my-selection-func "test-1" '(7 2 9 4) '(2 4 7 9))
  (check-my-selection-func "test-2" nil nil)
  (check-my-selection-func "test-3" '(3 1 4 2) '(1 2 3 4))
  (check-my-selection-func "test-4" '(5 5 3 3 2) '(2 3 3 5 5))

  ;; Тести з вибраними ключовими параметрами
  (check-my-selection-func "test-5" '(10 3 8 1 6) '(1 3 6 8 10) :test #'<)
  (check-my-selection-func "test-6" '((3 . 1) (2 . 5) (1 . 4))
                                 '((1 . 4) (2 . 5) (3 . 1))
                                 :key #'car :test #'<)
  (check-my-selection-func "test-7" '((5 . 2) (4 . 3) (3 . 6))
                                 '((3 . 6) (4 . 3) (5 . 2))
                                 :key #'cdr :test #'>)
  (check-my-selection-func "test-8" '("apple" "kiwi" "banana" "cherry")
                                  '("kiwi" "apple" "cherry" "banana")
                                  :key #'length :test #'<)
  (check-my-selection-func "test-9" '(-7 3 -4 5 1) '(1 3 -4 5 -7) :key #'abs :test #'<))


(defun add-prev-fn (&key (transform #'identity))
  "Функція створює замикання для обробки списку, яке формує пари (поточний елемент . попередній елемент).
   За необхідності, застосовує функцію transform до елементів."
  (let ((prev nil))                                                              ; Зберігаємо попередній елемент
    (lambda (current)
      (let ((transformed-current (funcall transform current))                    ; Застосовуємо transform до поточного
            (transformed-prev (and prev (funcall transform prev))))              ; Застосовуємо transform до попереднього
        (setf prev current)                                                      ; Оновлюємо попередній елемент
        (cons transformed-current transformed-prev)))))                          ; Повертаємо пару

(defun check-add-prev-fn (name input expected &key (transform 'identity))
  "Функція, яка виконує перевірку фактичного результату з очікуваним і виводить повідомлення про те, чи пройшла перевірка."
  (let ((result (mapcar (add-prev-fn :transform transform) input)))
    (format t "~:[Failed....~;Passed!!!!~] ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "  Expected: ~a~%  Got: ~a~%~%" expected result))))

(defun test-add-prev-fn ()
  "Тестові набори для функції add-prev-fn."
  (format t "Testing add-prev-fn ~%")
  
  ;; Тести без transform
  (check-add-prev-fn "test-1" '(10 20 30) '((10 . NIL) (20 . 10) (30 . 20)))
  (check-add-prev-fn "test-2" nil nil)
  (check-add-prev-fn "test-3" '(7 5 3 1) '((7 . NIL) (5 . 7) (3 . 5) (1 . 3)))
  
  ;; Тести з використанням transform
  (check-add-prev-fn "test-4" '(2 4 6) '((3 . NIL) (5 . 3) (7 . 5)) :transform #'1+)
  (check-add-prev-fn "test-5" '(10 20 30) '((20 . NIL) (40 . 20) (60 . 40)) :transform (lambda (x) (* 2 x)))
  (check-add-prev-fn "test-6" '("x" "xx" "xxx") '(("X" . NIL) ("XX" . "X") ("XXX" . "XX")) :transform #'string-upcase))

