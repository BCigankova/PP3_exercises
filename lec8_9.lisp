(defclass select-window (window)
  ((selected-items-frames :initform '())))

(defmethod selection ((sw select-window))
  (mapcar (lambda (item bool) (when bool item))
          (items (shape sw))
          (slot-value sw 'selected-list)))

(defmethod select-shape ((sw select-window) sel-shape)
  (mark-selected-list sw sel-shape t)
  (send-with-change sw 'add-border 'select-shape sel-shape))

(defmethod add-border ((sw select-window) shape)
  (let ((

(defmethod selectedp ((sw select-window) shape)
  (find shape (selection sw)))

(defmethod mark-selected-list ((sw select-window) sel-shape bool)
  (setf (nth (position sel-shape (selection sw)) 
             (slot-value sw 'selected-list))
        bool))
        

(defmethod deselect-shape ((sw select-window) sel-shape)
  (mark-selected-list sw sel-shape nil)
  (send-with-change sw 'remove-border 'deselect-shape sel-shape))

(defmethod set-shape ((sw select-window) shape)
  (call-next-method)
  (update-selected-list))


(defmethod ev-mouse-down ((sw select-window) sender origin button position)
  (call-next-method)
  (if (selectedp sw origin)
      (deselect-shape sw origin)
    (select-shape sw origin)))

(defmethod ev-change ((sw select-window) sender origin message &rest msg-args)
  (call-next-method)
  (when (selectedp sw origin)
    (send-with-change sw 'do-add-border 'add-border origin)))

(defmethod mouse-down-no-shape ((sw select-window) button position)
  (let ((shape (find-clicked-shape-nosolid sw position)))
    (when (not shape)
      (deselect-all sw))))


(defmethod find-clicked-shape-nosolid ((sw select-window) position)
  (when (shape sw)
    (find-if (lambda (shape) (contains-point-p shape position))
             (items (shape sw)))))





;(defmethod ev-select-pic-change ((sw select-window) shape))

