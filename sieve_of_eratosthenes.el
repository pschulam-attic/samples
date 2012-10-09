;; The Sieve of Eratosthenes
;;
;; Usage: (sieve n)
;;
;;        where `n' is the maximum value of the list
;;        of numbers fed to the sieve.

(require 'cl)

(defun divides-p (factor num)
  (zerop (mod num factor)))

(defun remove-multiples (factor numbers)
  (remove-if (lambda (x) (divides-p factor x)) numbers))

(defun sieve (n)
  (labels ((sieve-filter (numbers)
			 (if (null numbers) nil
			   (cons (car numbers)
				 (sieve-filter (remove-multiples (car numbers)
								 (cdr numbers)))))))
    (sieve-filter (number-sequence 2 n))))
