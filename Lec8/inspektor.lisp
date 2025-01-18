;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   INSPECTOR
;;


(defclass inspector-window (window)
  ((inspected-window :initform nil)
   (inspected-object :initform nil)))


;;
;; SLOTY
;;

(defmethod inspected-object ((irw inspector-window))
  (slot-value irw 'inspected-object))

(defmethod do-set-inspected-object ((irw inspector-window) obj)
  (setf (slot-value irw 'inspected-object) obj))



(defmethod inspected-window ((irw inspector-window))
  (slot-value irw 'inspected-window))

(defmethod do-set-inspected-window ((irw inspector-window) isw)
  (null-delegate irw)
  (setf (slot-value irw 'inspected-window) isw)
  (do-set-inspected-object irw nil)
  (set-delegate isw irw)
  (add-event isw 'ev-mouse-down-no-shape)
  (set-shape irw (display-properties irw)))

(defmethod do-set-null-inspected-window ((irw inspector-window))
  (null-delegate irw)
  (setf (slot-value irw 'inspected-window) nil)
  (do-set-inspected-object irw nil)
  (set-shape irw nil))

(defmethod set-inspected-window ((irw inspector-window) isw)
  (cond ((typep isw 'inspected-window) 
         (send-with-change irw 'do-set-inspected-window 'set-inspected-window isw))
        ((typep isw 'null) 
         (send-with-change irw 'do-set-null-inspected-window 'set-inspected-window))
        (t (error "not an inspected window or nil"))))

(defmethod null-delegate ((irw inspector-window))
  (when (inspected-window irw)
    (set-delegate (inspected-window irw) nil)))



;;
;; EVENTY
;;



(defmethod initialize-instance ((irw inspector-window) &key)
  (call-next-method)
  (add-event irw 'ev-mouse-down-no-shape))
;; vracet irw?

(defmethod ev-mouse-down-no-shape ((irw inspector-window) isw)
  (do-set-inspected-object irw nil)
  (set-shape irw (display-properties irw)))

(defmethod ev-mouse-down ((irw inspector-window) sender clicked button position)
  (call-next-method)
  (unless (find clicked (items (shape irw)))
    (do-set-inspected-object irw clicked)
    (set-shape irw (display-properties irw))))


(defmethod ev-change ((irw inspector-window) sender origin msg &rest args)
  (call-next-method)
  (when (eql (inspected-object irw) origin)
    (set-shape irw (display-properties irw))))



;;
;; DOUBLE CLICK
;;


(defmethod install-callbacks ((irw inspector-window))
  (call-next-method)
  (install-double-click-callback irw))

(defmethod install-double-click-callback ((irw inspector-window))
  (mg:set-callback 
   (slot-value irw 'mg-window) 
   :double-click (lambda (mgw button x y)
		 (declare (ignore mgw))
		 (window-double-click
                  irw
                  button 
                  (move (make-instance 'point) x y))))
  irw)

(defmethod window-double-click ((irw inspector-window) button position)
  (let ((proptext (find-clicked-shape irw position)))
    (when proptext
      (update-property irw proptext))))


(defmethod update-property ((irw inspector-window) proptext)
  (let ((text (display-prompt))
        (ins (if (inspected-object irw) (inspected-object irw) (inspected-window irw))))
    (when (second text)
        (funcall (setter-name (property proptext)) ins (car text))
        (display-properties irw))))

#|
(defun find-prop (text-shape)
  (subseq (text text-shape) 0 (search ":" (text text-shape))))
|#

(defun display-prompt ()
  (multiple-value-list
   (capi:prompt-for-value "Zadejte novou hodnotu")))


(defun setter-name (prop)
  (values (find-symbol (format nil "SET-~a" prop))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TEXT
;;

(defclass property-text (text-shape)
  ((property :initform nil)))

(defmethod set-property ((proptext property-text) prop)
  (send-with-change proptext 'do-set-property 'set-property prop))

(defmethod do-set-property ((proptext property-text) prop)
  (setf (slot-value proptext 'property) prop))

(defmethod property ((proptext property-text))
  (slot-value proptext 'property))


(defmethod display-properties ((irw inspector-window))
  (let ((pic (make-property-picture irw)))
    (dotimes (i (length (items pic)))
      (move (nth i (items pic))
          0
          (* 15 i)))
    pic))


(defmethod make-property-picture ((irw inspector-window))
  (let* ((obj (if (inspected-object irw) (inspected-object irw) (inspected-window irw)))
         (property-list (properties obj)))
    (set-items (make-instance 'picture)
               (append (list (move (set-text 
                              (make-instance 'property-text)
                              (format nil "~a" (type-of obj)))
                                   20 20))
                       (mapcar (lambda (property)
                                 (make-property-text irw property obj))
                               property-list)))))

(defmethod make-property-text ((irw inspector-window) property obj)
  (move
   (set-property
    (set-text
     (make-instance 'property-text)
     (format nil "~a:  ~a" property 
             (funcall property (if obj
                                   obj
                                 (inspected-window irw)))))
    property)
   20 20))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INSPECTED
;;



;; dedit od abstract-window nebo window?
(defclass inspected-window (window)
  ())

(defmethod mouse-down-no-shape ((isw inspected-window) button position)
  (send-event isw 'ev-mouse-down-no-shape))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   VLASTNOST VLASTNOSTI
;;

(defmethod properties ((s shape))
  (list 'color 'thickness 'filledp))

(defmethod properties ((pt point))
  (append (list 'x 'y 'r 'phi) (call-next-method)))

(defmethod properties ((cr circle))
  (append (list 'radius) (call-next-method)))

(defmethod properties ((ap abstract-polygon))
  (append (list 'closedp) (call-next-method)))

(defmethod properties ((aw abstract-window))
  (list 'background))



#|

TESTY

(setf irw1 (make-instance 'inspector-window))
(setf cr1 (move (set-filledp (set-color (set-radius (make-instance 'circle) 40) :red) t) 40 40)
      cr2 (move (set-filledp (set-color (set-radius (make-instance 'circle) 20) :green) t) 100 100))
(setf pic1 (set-items (make-instance 'picture) (list cr1 cr2)))
(setf isw1 (set-shape (make-instance 'inspected-window) pic1))
(set-inspected-window irw1 isw1)

|#