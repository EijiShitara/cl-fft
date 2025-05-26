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
    
;;; Iterative FFT
(defun iter-fft (a)
  "The length of array a must be a power of 2"
  (let ((len (length a))
        (a-perm (coerce (bit-reversal-permutation a) 'list)))
    (loop for s from 1 to (log len 2)
          do (let* ((m (expt 2 s))
                    (omega-m (complex (cos (/ (* pi 2) m))
                                      (sin (/ (* pi 2) m)))))
               (loop for k from 0 to (- len 1) by m
                     do (let ((omega 1))
                          (loop for j from 0 to (- (/ m 2) 1)
                                do (let ((v (* omega (nth (+ k j (/ m 2)) a-perm)))
                                         (u (nth (+ k j) a-perm)))
                                     (setf (nth (+ k j) a-perm) (+ u v)
                                           (nth (+ k j (/ m 2)) a-perm) (- u v)
                                           omega (* omega omega-m))))))))
    a-perm))
