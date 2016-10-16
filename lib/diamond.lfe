(defmodule diamond
  (export all))

(defun char-for-row
  ([n y] (when (=< y n)) (list (+ #\A y)))
  ([n y] (list (+ #\A (- (* 2 n) y)))))

(defun char-at
  ([n x y] (when (orelse (== n (+ x y))
			 (== n (- x y))
			 (== n (- y x))
			 (== (* 3 n) (+ x y))))
   (char-for-row n y))
  ([_ _ _] " "))

(defun diamond-row
  [n y] (++ (lists:map (lambda [i] (char-at n i y))
	     (lists:seq 0 (* 2 n)))
	    '("\n")))

(defun diamond
  [n] (lists:map (lambda [i] (diamond-row n i))
		 (lists:seq 0 (* 2 n))))

(defun main
  ([(cons n _)]  (io:fwrite (lists:flatten (diamond (list_to_integer n))))))