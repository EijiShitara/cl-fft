(defun nth-root (n)
  (complex (cos (/ (* pi 2) n)) (sin (/ (* pi 2) n))))

(defun %fft (array &key inverse)
  "The length of the array must be a power of 2"
  (when (= (length array) 1)
    (return-from %fft array))
  (let ((even-terms (make-array 0 :adjustable t :fill-pointer 0))
        (odd-terms (make-array 0 :adjustable t :fill-pointer 0))
        (length 0))
    (loop for x across array
          for i from 0
          do (if (evenp i)
                 (vector-push-extend x even-terms)
                 (vector-push-extend x odd-terms))
             (incf length))
    (let ((omega-n (nth-root (if inverse
                                 (- length)
                                 length)))
          (omega 1)
          (y_0 (%fft even-terms))
          (y_1 (%fft odd-terms))
          (result (make-array length :element-type 'number)))
      (loop for j from 0 to (1- (/ length 2))
            do (setf (aref result j)
                     (+ (aref y_0 j)
                        (* omega (aref y_1 j))))
               (setf (aref result (+ j (/ length 2)))
                     (- (aref y_0 j)
                        (* omega (aref y_1 j))))
               (setf omega (* omega omega-n)))
      result)))

(defun fft (array)
  (%fft array :inverse nil))

(defun inverse-fft (array)
  (%fft array :inverse t))
