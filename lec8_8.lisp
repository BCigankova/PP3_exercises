(defclass menu-window (window)
  ())

(defmethod initialize-instance ((mw menu-window) &key)
  (call-next-method)
  (setf (slot-value mw 'shape)
        (make-instance 'menu-picture))
  (set-delegate (shape mw) mw)
  (add-event (menu-pic mw) 'ev-right-click-inside-shape)
  (add-event (menu-pic mw) 'ev-click-inside-menu)
  mw)

        
(defmethod do-set-shape ((mw menu-window) shape)
  (set-items (shape mw) shape)
  (when shape (set-delegate shape (shape mw)))
  mw)

(defmethod shape ((mw menu-window))
  (cdr (items (menu-pic mw))))

(defmethod menu-pic ((mw menu-window))
  (slot-value mw 'shape))

(defmethod mouse-down-no-shape ((mw menu-window) button position)
  (cond ((and (not (menu (menu-pic mw))) (eql button :right))
         (add-menu mw position))
        ((menu (menu-pic mw))
         (delete-menu mw))
        (t nil))
  mw)

(defmethod add-menu ((mw menu-window) position)
  (send-with-change mw 'make-menu 'add-menu position))

(defmethod make-menu ((mw menu-window) position)
  (push (do-make-menu mw position) (items (menu-pic mw))))

(defmethod delete-menu ((mw menu-window) position)
  (send-with-change mw 'do-delete-menu 'delete-menu))

(defmethod do-delete-menu ((mw menu-window))
  (pop (items (menu-pic mw))))

(defmethod ev-right-click ((mw menu-window) clicked-shape)
  ())
  

(defclass menu-picture (abstract-picture)
  ())

(defmethod menu ((mp menu-picture))
  (when (typep (first (items mp)) 'menu)
    (first (items mp))))


(defmethod ev-mouse-down ((mp menu-picture) sender clicked button position)
  (call-next-method)
  (cond ((eql button :right)
    (send-event mp 'ev-right-click-inside
        

;dodelat menu

(defclass menu (abstract-picture)
  ())