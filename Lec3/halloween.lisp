(defun make-ghost (color scale-coeff)
  (scale-ghost 
   (set-ghost-color 
    (assemble-ghost)
    color)
   scale-coeff))

;; make-eye vytvori s(ocket) nebo p(upil) pro l(eft) oko nebo r(ight) oko

(defun assemble-ghost ()
  (let* ((head (make-head))
         (body (make-body head)))
    (set-filled-all 
     (set-items (make-instance 'picture) 
                (list (make-eye head 'l 'p)
                      (make-eye head 'r 'p)
                      (make-eye head 'l 's)
                      (make-eye head 'r 's) 
                      head 
                      body 
                      (make-tail head body))))))


(defun set-ghost-color (ghost color)
  (set-color (fifth (items ghost)) color)
  (set-color (sixth (items ghost)) color)
  (set-color (seventh (items ghost)) color)
  ghost)

(defun scale-ghost (ghost scale-coeff)
  (dolist (part (items ghost))
    (scale part scale-coeff (make-instance 'point)))
  ghost)

(defun make-head ()
  (let ((head (make-instance 'circle)))
    (set-y 
     (center (set-radius head 75))
     -25)
    head))

(defun make-body (circ)
  (let ((cent (center circ))
        (rad (radius circ)))
    (set-items 
     (make-instance 'polygon) 
     (list (set-body-point 0 cent #'-)
           (set-body-point 0 cent #'+)
           (set-body-point rad cent #'-)
           (set-body-point rad cent #'+)))))

(defun set-body-point (rad cent fun)
  (set-y
   (set-x (make-instance 'point)
          (funcall fun (x cent) rad))
   (+ (y cent) rad)))

(defun set-tail-point (x y rad i)
  (set-y
   (set-x (make-instance 'point)
          (+ x (* (/ i 3) rad)))
   y))


(defun make-tail (circ body)
  (let* ((rad (radius circ))
         (up-y (y (fourth (items body))))
         (lw-y (+ up-y
                  (* rad (/ 2 3))))
         (x (fourth (items body))))
    (dotimes (i 5)
      (set-tail-point x (if (isoddp i) 
    (set-y 
     (set-x up1 
            (x (fourth (items body))))
     (y (fourth (items body))))

    (set-y 
     (set-x up5 
            (x (third (items body))))
     (y (third (items body))))


    (set-y 
     (set-x lw1 (x up1))
     (+ (y up1) 
        (* rad (/ 2 3))))


    (set-y 
     (set-x lw4 (x up5))
     (y lw1))


    (set-y 
     (set-x up2 (+ (x up1)
                  (/ rad 3)))
     (y up1))
    (set-y 
     (set-x lw2 (+ (x up2)
                   (/ rad 3)))
     (y lw1))
    (set-y 
     (set-x up3 (+ (x lw2)
                   (/ rad 3)))
     (y up1))
    (set-y 
     (set-x lw3 (+ (x up3)
                   (/ rad 3)))
     (y lw1))
    (set-y 
     (set-x up4 (+ (x lw3)
                   (/ rad 3)))
     (y up1))
    (set-items
     (make-instance 'polygon)
     (list (set-tail-point (x (fourth (items body))) up-y 0)
           (set-tail-point (x (fourth (items body))) 0 lw-y 0)


up1 lw1 up2 lw2 up3 lw3 up4 lw4 up5))))




(defun make-eye (circ dir shape)
  (let ((eye (make-instance 'circle))
        (rad (radius circ))
        (cent (center circ)))
    (set-y 
     (center (set-radius eye 
                         (/ rad
                            (if (eql shape 's) 3 10))))
     (y cent))
    (set-x (center eye)
           (cond ((and (eql dir 'l) (eql shape 's))
                  (- (x cent)
                     (/ rad 2)))
                 ((and (eql dir 'r) (eql shape 's))
                  (+ (x cent)
                     (/ rad 2)))
                 ((and (eql dir 'l) (eql shape 'p))
                  (- (x cent)
                     (* rad (/ 6 10))))
                 ((and (eql dir 'r) (eql shape 'p))
                  (+ (x cent)
                     (* rad (/ 6 10))))))
    (set-color eye (if (eql shape 's) :white :black))))

(defun set-filled-all (ghost)
  (dolist (item (items ghost))
    (set-filledp item t))
  ghost)

;;Z nejakeho duvodu se mi duchove nechteji vykreslit ihned po zavolani funkce, musim nastavit okno do promenne v listeneru a pak ho redrawnout tam, aby se duchove zobrazili. Nevim jestli je to takhle schvalne nebo mi na tom neco nefunguje... zkusila jsem udelat okno jako lokalni promennou, redrawnout ho, redrawnout ho a vracet...

(defun display-halloween-window (ghost-count)
  (set-shape (set-background 
              (make-instance 'window) 
                             :black)
             (make-ghosts ghost-count)))

(defun make-ghosts (count)
  (let ((ghosts (make-instance 'picture)))
    (dotimes (i count)
      (set-items ghosts (cons (make-random-ghost) 
                               (items ghosts))))
    ghosts))

(defun make-random-ghost ()
  (move 
   (rotate 
    (make-ghost (select-random-color) 
                (/ 1 (random-from-range 2 4)))
    (* pi (random-from-range 0 2)) (make-instance 'point))
   (random 200) 
   (random 200)))

(defun select-random-color ()
  (let ((lst (list :red :yellow :white :blue :green)))
    (nth (random 5) lst)))

(defun random-from-range (from to)
  (+ from
     (* (random pi)
        (/ (- to from)
           pi))))