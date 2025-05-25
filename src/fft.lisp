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

;; (defun fft (array)
;;   "The length of the array must be a power of 2"
;;     (let ((len (length array)))
;;       (when (= 1 len)
;;         (return-from fft array))
;;       (let* ((omega-n (nth-root len))
;;              (omega 1)
;;              (subarray-even (even-positions array))
;;              (subarray-odd (odd-positions array))
;;              (y_0 (fft subarray-even))
;;              (y_1 (fft subarray-odd)))
;;         (let ((result (loop for i from 0 to (1- len) collect 0)))
;;           (loop for i from 0 to (- (/ len 2) 1)
;;                 do (setf (nth i result)
;;                          (+ (nth i y_0)
;;                             (* omega (nth i y_1))))
;;                    (setf (nth (+ i (/ len 2)) result)
;;                          (- (nth i y_0)
;;                             (* omega (nth i y_1))))
;;                    (setf omega (* omega omega-n)))
;;           result))))



(defun nth-root (n)
  (complex (cos (/ (* pi 2) n)) (sin (/ (* pi 2) n))))

(defun fft (array)
  "The length of the array must be a power of 2"
  (when (= (length array) 1)
    (return-from fft array))
  (let ((even-terms (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
        (odd-terms (make-array 0 :element-type 'number :adjustable t :fill-pointer 0))
        (length 0))
    (loop for x across array
          for i from 0
          do (if (evenp i)
                 (vector-push-extend x even-terms)
                 (vector-push-extend x odd-terms))
             (incf length))

    (let ((omega-n (nth-root length))
          (omega 1)
          (y_0 (fft even-terms))
          (y_1 (fft odd-terms))
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
