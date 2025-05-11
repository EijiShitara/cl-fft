(defun even-positions (lst)
  (if (null lst)
      lst
      (let ((count 0)
            result)
        (flet ((push-if-even (x)
                 (if (evenp count)
                     (progn
                       (push x result)
                       (setf count (1+ count)))
                     (setf count (1+ count)))))
        (mapcar #'push-if-even
                lst)
          (reverse result)))))

(defun odd-positions (lst)
  (if (null lst)
      lst
      (let ((count 0)
            result)
        (flet ((push-if-odd (x)
                 (if (oddp count)
                     (progn
                       (push x result)
                       (setf count (1+ count)))
                     (setf count (1+ count)))))
        (mapcar #'push-if-odd
                lst)
          (reverse result)))))


(odd-positions '(1 1 3 4 5))

(defun fft (a)
  "The length of array a must be a power of 2"
  (flet ((euler (n)
           (complex (cos (/ (* pi 2) n))
                    (sin (/ (* pi 2) n)))))
    (let ((len (length a)))
      (when (= 1 len)
        (return-from fft a))
      (let* ((omega-n (euler len))
             (omega 1)
             (subarray-even (even-positions a))
             (subarray-odd (odd-positions a))
             (y_0 (fft subarray-even))
             (y_1 (fft subarray-odd)))
        (let ((result (loop for i from 0 to (- len 1) collect 0)))
          (loop for i from 0 to (- (/ len 2) 1)
                do (setf (nth i result)
                         (+ (nth i y_0)
                            (* omega (nth i y_1))))
                   (setf (nth (+ i (/ len 2)) result)
                         (- (nth i y_0)
                            (* omega (nth i y_1))))
                   (setf omega (* omega omega-n)))
          result)))))

(defun inverse-fft (a)
  "The length of array a must be a power of 2"
  (flet ((euler (n)
           (complex (cos (/ (* pi 2) n))
                    (- (sin (/ (* pi 2) n))))))
    (let ((len (length a)))
      (when (= 1 len)
        (return-from fft a))
      (let* ((omega-n (euler len))
             (omega 1)
             (subarray-even (even-positions a))
             (subarray-odd (odd-positions a))
             (y_0 (inverse-fft subarray-even))
             (y_1 (inverse-fft subarray-odd)))
        (let ((result (loop for i from 0 to len collect 0)))
          (loop for i from 0 to (- (/ len 2) 1)
                do (setf (nth i result)
                         (+ (nth i y_0)
                            (* omega (nth i y_1))))
                   (setf (nth (+ i (/ len 2)) result)
                         (- (nth i y_0)
                            (* omega (nth i y_1))))
                   (setf omega (* omega omega-n)))
          result)))))

;;; Bit-reversal-permutation
(defun integer->bit-vector (integer &optional dim)
  "Create a bit-vector from a nonnegative integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum)
                    (integer->bit-list int (list 0)))
                   (dim
                    (let ((len (length accum))) ; need refactoring
                      (when (>= dim len)
                        (dotimes (i (- dim len))
                          (push 0 accum))
                        accum)))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun bit-vector->integer (bit-vector)
  "Create a nonnegative integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun f (n &optional dim)
  (bit-vector->integer
   (nreverse
    (integer->bit-vector n dim))))

(defun bit-reversal-permutation (sequence &optional (n (length sequence)))
  (let ((v (make-array n))
        (p (round (log n 2))))
    (loop for val in sequence
          for m from 0
          do (setf (aref v (f m p)) val))
    v))
    
