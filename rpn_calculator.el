(require 'cl)

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defparameter *input* '(19 2.14 + 4.5 2 4.3 / - *))

(defun rpn-calc (expression)
  (evaluate-stack expression nil))

(defun evaluate-stack (stack-in stack-store)
  (let ((top (car stack-in)))
    (cond ((null stack-in)
	   (car stack-store))
	  ((numberp top)
	   (evaluate-stack (cdr stack-in) (cons top stack-store)))
	  ((symbolp top)
	   (evaluate-stack (cdr stack-in) (reduce-stack top stack-store))))))

(defun reduce-stack (op stack-store)
  (let ((x2 (pop stack-store))
	(x1 (pop stack-store)))
    (cons (funcall op x1 x2)
	  stack-store)))
