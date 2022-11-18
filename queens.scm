(declare (unit queens))

(define (enumerate-interval i j)
  (let ((op (if (< i j) + -)))
    (if (= i j)
        (list i)
        (cons i (enumerate-interval (op i 1) j)))))

(define (filter p seq)
  (cond ((null? seq) '())
        ((p (car seq)) (cons (car seq) (filter p (cdr seq))))
        (else (filter p (cdr seq)))))

(define (flatmap f seq)
  (foldr append '() (map f seq)))

(define (empty-board n)
  (if (= n 0)
      '()
      (cons '() (empty-board (- n 1)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board board-size))
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (gen-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board board-size))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))
  (queen-cols board-size))

(define (output-boards boards)
  (for-each (lambda (b) (for-each print b) (newline)) boards))

(define (adjoin-position row column board)
  (define (iter r b)
    (let ((e (if (= r row) '(1) '(0))))
      (if (null? b)
          '()
          (cons (append (car b) e) (iter (+ r 1) (cdr b))))))
  (iter 1 board))

(define (sum seq)
  (foldr + 0 seq))

(define (transpose-board board)
  (if (null? (car board))
      '()
      (cons (map car board) (transpose-board (map cdr board)))))

(define (nth n seq)
  (if (= n 0)
      (car seq)
      (nth (- n 1) (cdr seq))))

(define (take n seq)
  (if (= n 0)
      '()
      (cons (car seq) (take (- n 1) (cdr seq)))))

(define (until p f arg)
  (if (p arg)
      '()
      (let ((res (f arg)))
        (cons res (until p f res)))))

(define (diagonal board)
  (if (or (null? board) (null? (car board)))
      '()
      (cons (caar board) (diagonal (map cdr (cdr board))))))

(define (diagonals-major board)
  (filter
    (lambda (e) (not (null? e)))
    (append
      (map diagonal
          (until (lambda (l) (null? (car l)))
                  (lambda (b) (map cdr b))
                  board))
      (cons (diagonal board)
        (map diagonal (until null? cdr board))))))

(define (diagonals-minor board)
  (diagonals-major (map reverse board)))

(define (diagonals board)
  (append (diagonals-major board) (diagonals-minor board)))

(define (safe-rows? board)
  (null? (filter (lambda (x) (> x 1)) (map sum board))))

(define (safe-columns? board)
  (safe-rows? (transpose-board board)))

(define (safe-diagonals? board)
  (safe-rows? (diagonals board)))

(define (safe? k position)
  (and
    (safe-rows? position)
    (safe-columns? position)
    (safe-diagonals? position)))

(define board
  '((1 1 1)
    (1 0 0)
    (0 0 0)))

(define enum-board
  '(( 1  2  3  4)
    ( 5  6  7  8)
    ( 9 10 11 12)
    (13 14 15 16)))

; (
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
;   (8 7 6 5 4 3 2 1)
; )
