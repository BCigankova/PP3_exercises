;;
;;  SEMAFOR
;;


; (type (on-colors) (list of phases))
(defvar *semaphore-types-phases* (list (list :vehicle
                                             (list :red :orange :green)
                                             (list '(t nil nil)
                                                   '(t t nil)
                                                   '(nil nil t)
                                                   '(t nil t)))
                                       (list :pedestrian
                                             (list :red :green)
                                             (list '(t nil)
                                                   '(nil t)))))
(defvar *semaphore-block-length* 30)
(defvar *light-radius* 10)


(defclass semaphore (abstract-picture)
  ((semaphore-type :initform :pedestrian)
   (current-phase :initform 0)))



;;
;; typ semaforu
;;

(defmethod semaphore-type ((s semaphore))
  (slot-value s 'semaphore-type))

(defmethod set-semaphore-type ((s semaphore) type)
  (check-semaphore-type s type)
  (do-set-semaphore-type s type))

(defmethod check-semaphore-type ((s semaphore) type)
  (when (not (find type (mapcar #'first *semaphore-types-phases*)))
    (error "~type is not a semaphore type")))

(defmethod do-set-semaphore-type ((s semaphore) type)
  (setf (slot-value s 'semaphore-type) type)
  (do-set-items s (make-lights s type))
  (do-set-items s (append (items s) (list (outline s))))
  (set-current-phase s 0))

(defmethod semaphore-type-list ((s semaphore))
  (find-if (lambda (item)
             (eql (semaphore-type s) (first item)))
           *semaphore-types-phases*))


(defmethod outline ((s semaphore))
  (let ((xup (x (center (first (items s)))))
        (yup (y (center (first (items s)))))
        (ylow (y (center (car (last (items s))))))
        (half (/ *semaphore-block-length* 2)))
    (set-items 
     (make-instance 'polygon)
     (list (move (make-instance 'point)
                 (- xup half)
                 (- yup half))
           (move (make-instance 'point) 
                 (+ xup half)
                 (- yup half))
           (move (make-instance 'point) 
                 (+ xup half)
                 (+ ylow half))
           (move (make-instance 'point) 
                 (- xup half)
                 (+ ylow half))))))





;;
;; Svetla semaforu
;;

(defmethod light-number ((s semaphore))
  (length (second (semaphore-type-list s))))


(defmethod make-lights ((s semaphore) type)
  (let ((light-list '()))
    (dotimes (i (light-number s))
      (push
       (move-lights
        (set-on-color 
         (set-radius
          (make-instance 'light)
          *light-radius*)
         (color-of-light s type i))
        i)
       light-list))
    (reverse light-list)))

(defmethod color-of-light ((s semaphore) type position)
  (nth position (second (semaphore-type-list s))))

(defmethod move-lights ((l light) i)
  (move l
        (/ *semaphore-block-length* 2) 
        (* (/ 3 2) i *semaphore-block-length*)))

;;
;;  Faze
;;

(defmethod phase-count ((s semaphore))
  (length (third (semaphore-type-list s))))

(defmethod current-phase ((s semaphore))
  (slot-value s 'current-phase))

(defmethod set-current-phase ((s semaphore) phase)
  (check-phase s phase)
  (do-set-current-phase s phase))

(defmethod check-phase ((s semaphore) phase)
  (if (not (< phase (phase-count s)))
      (error "phase exceeds phase count for semaphore type")))

(defmethod do-set-current-phase ((s semaphore) phase)
  (setf (slot-value s 'current-phase) phase)
  (ensure-on-off s phase))

(defmethod next-phase ((s semaphore))
  (let ((next (mod (1+ (current-phase s))
                   (phase-count s))))
    (do-set-current-phase s next)))

(defmethod ensure-on-off ((s semaphore) phase)
  (dotimes (i (light-number s))
    (set-onp (nth i (items s)) 
             (nth i (nth phase (third (semaphore-type-list s))))))
  s)






;;
;; KRIZOVATKY
;;



(defclass crossroads (picture)
  ((current-phase :initform 0)
   (program :initform nil)))


#|(defmethod semaphores ((cr crossroads))
  (labels ((iter (item)
             (cond ((null (car item)) '())
                   ((typep (car item) 'semaphore) (cons (car item) 
                                                        (iter (cdr item))))
                   (t (iter (cdr item))))))
    (iter (items cr))))|#

(defmethod semaphores ((cr crossroads))
  (labels ((iter (items)
             (cond ((null (car items)) '())
                   ((typep (car items) 'semaphore) 
                    (cons (car items) (iter (cdr items))))
                   ((typep (car items) 'abstract-picture) 
                    (append (iter (items (car items))) (iter (cdr items))))
                   (t (iter (cdr items))))))
    (iter (items cr))))

(defmethod semaphore-count ((cr crossroads))
  (length (semaphores cr)))

#| (set-items cr1 (list (make-instance 'semaphore) (make-instance 'point) (set-items (make-instance 'picture) (list (make-instance 'semaphore))))) |#


;;
;; FAZE
;;

(defmethod current-phase ((cr crossroads))
  (slot-value cr 'current-phase))

(defmethod set-current-phase ((cr crossroads) value)
  (check-phase cr value)
  (do-set-current-phase cr value))

(defmethod check-phase ((cr crossroads) value)
  (when (not (< value (phase-count cr)))
      (error "phase exceeds phase count for program")))

(defmethod do-set-current-phase ((cr crossroads) value)
  (ensure-semaphore-phases cr (nth value (program cr)))
  (setf (slot-value cr 'current-phase) value)
  cr)

(defmethod phase-count ((cr crossroads))
  (length (program cr)))

(defmethod ensure-semaphore-phases ((cr crossroads) value)
  (dotimes (i (semaphore-count cr))
    (set-current-phase (nth i (semaphores cr))
                       (nth i value)))
  cr)

(defmethod next-phase ((cr crossroads))
  (let ((next (mod (1+ (current-phase cr))
                   (phase-count cr))))
    (do-set-current-phase cr next)))


;;
;; PROGRAM
;;

(defmethod program ((cr crossroads))
  (slot-value cr 'program))

(defmethod set-program ((cr crossroads) prog)
  (check-program cr prog)
  (do-set-program cr prog))

(defmethod check-program ((cr crossroads) prog)
  (reduce (lambda (x y)
            (and x y))
          (mapcar (lambda (phase)
                    (and (= (length phase) (semaphore-count cr))
                         (reduce (lambda (x y)
                                   (and x y))
                                 (mapcar #'numberp phase))))
                  prog)))


(defmethod do-set-program ((cr crossroads) prog)
  (setf (slot-value cr 'program) prog)
  (set-current-phase cr 0))