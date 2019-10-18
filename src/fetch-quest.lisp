(in-package :fetch-quest)

;;;; game config and menu

(defvar *fetch-quest-config*
  (make-config (*default-config*)
               ('config-resource-dirs (list "./resources"))
               ;; ('window-icon "window-icon.png") TODO
               ('default-font "fonts/liberation_sans/LiberationSans-Regular.ttf")
               ('resizable-window t)
               ('enable-vsync t)
               ('enable-compositor nil)
               ('fullscreen-p t)
               ('game-name "Fetch Quest")))

(defun game-menu ()
  (setf (clear-color *engine-manager*) *black*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                (getconfig 'game-name *config*)
                ("Play" (run-action
                         (change-scene *engine-manager* (launch-fetch-quest))))
                ("Quit" (run-action (quit))))))
    menu))

;;;; basic objects

(defclass rectangle (static-sprite)
  ((path-to-sprite :initform (resource-path "rectangle.png")
                   :reader path-to-sprite))
  (:documentation "A basic rectangle with obb collision detection."))

;;;; player

(defvar *player* nil)

(defclass player (animated-sprite direction-tracker agent input-handler)
  ((recurse.vert:animations
    :initform (labels ((frame-at (row col)
                         (let ((frame-width 16)
                               (frame-height 32))
                           (list (* col frame-width) (* row frame-height) frame-width frame-height))))
                (list :facing-north (make-animation :spritesheet (resource-path "character.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 2 0)))
                                                    :time-between-frames-ms 500)
                      :facing-south (make-animation :spritesheet (resource-path "character.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 0 0)))
                                                    :time-between-frames-ms 500)
                      :facing-east (make-animation :spritesheet (resource-path "character.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 1 0)))
                                                    :time-between-frames-ms 500)
                      :facing-west (make-animation :spritesheet (resource-path "character.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 3 0)))
                                                    :time-between-frames-ms 500)
                      :walking-north (make-animation :spritesheet (resource-path "character.png")
                                                     :frames (vector (apply #'make-sprite-source (frame-at 2 0))
                                                                     (apply #'make-sprite-source (frame-at 2 1))
                                                                     (apply #'make-sprite-source (frame-at 2 2))
                                                                     (apply #'make-sprite-source (frame-at 2 3)))
                                                    :time-between-frames-ms 250)
                      :walking-south (make-animation :spritesheet (resource-path "character.png")
                                                     :frames (vector (apply #'make-sprite-source (frame-at 0 0))
                                                                     (apply #'make-sprite-source (frame-at 0 1))
                                                                     (apply #'make-sprite-source (frame-at 0 2))
                                                                     (apply #'make-sprite-source (frame-at 0 3)))
                                                    :time-between-frames-ms 250)
                      :walking-east (make-animation :spritesheet (resource-path "character.png")
                                                     :frames (vector (apply #'make-sprite-source (frame-at 1 0))
                                                                     (apply #'make-sprite-source (frame-at 1 1))
                                                                     (apply #'make-sprite-source (frame-at 1 2))
                                                                     (apply #'make-sprite-source (frame-at 1 3)))
                                                    :time-between-frames-ms 250)
                      :walking-west (make-animation :spritesheet (resource-path "character.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 3 0))
                                                                    (apply #'make-sprite-source (frame-at 3 1))
                                                                    (apply #'make-sprite-source (frame-at 3 2))
                                                                    (apply #'make-sprite-source (frame-at 3 3)))
                                                    :time-between-frames-ms 250))))
   (previous-position :initform (vector2)))
  (:documentation "Player controlled game object"))

(defmethod update-user ((player player) delta-t-ms scene)
  #+nil
  (with-slots (previous-position) player
    (setf (x previous-position) (x player)
          (y previous-position) (y player))
    (values)))

(defun %player-moving-p (player)
  (with-slots (previous-position) player
    (or (/= (x previous-position) (x player))
         (/= (y previous-position) (y player)))))

(defmethod get-new-animation ((player player))
  (prog1
      (ecase (first (facing player))
        (:north (cond
                  ((%player-moving-p player) :walking-north)
                  (t :facing-north)))
        (:south (cond
                  ((%player-moving-p player) :walking-south)
                  (t :facing-south)))
        (:east (cond
                  ((%player-moving-p player) :walking-east)
                  (t :facing-east)))
        (:west (cond
                  ((%player-moving-p player) :walking-west)
                  (t :facing-west))))
    (with-slots (previous-position) player
      (setf (x previous-position) (x player)
            (y previous-position) (y player))
      (values))))

(set-default-input-command-map
 player
 ("controller" (:13 :move-left)
               (:12 :move-down)
               (:14 :move-right)
               (:11 :move-up))
 ("sdl-keyboard" (:scancode-right :move-right)
                 (:scancode-l :move-right)
                 (:scancode-left :move-left)
                 (:scancode-h :move-left)
                 (:scancode-up :move-up)
                 (:scancode-k :move-up)
                 (:scancode-down :move-down)
                 (:scancode-j :move-down)))

(let* ((move-delta 2))
  (set-default-command-action-map
   player
   (:move-right (while-active
                 (push-direction player :east)
                 (incf (x player) move-delta)))
   (:move-left (while-active
                (push-direction player :west)
                (decf (x player) move-delta)))
   (:move-up (while-active
              (push-direction player :north)
              (decf (y player) move-delta)))
   (:move-down (while-active
                (push-direction player :south)
                (incf (y player) move-delta)))))

;;;; game scene

(defclass my-scene-input-handler (input-handler)
  ())

(set-default-input-command-map
 my-scene-input-handler
 ("sdl-keyboard" (:scancode-q :quit)))

(set-default-command-action-map
 my-scene-input-handler
 (:quit              (on-deactivate (quit))))

(defclass myscene (game-scene physics-context-2d)
  ((scene-input-handler
    :documentation "Input handler for the main scene."
    :initform (make-instance 'my-scene-input-handler
                             :active-input-device *all-input-id*)))
  (:documentation "Default scene for my game"))

(defmethod update :before ((scene myscene) delta-t-ms world-context)
  (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

(defun launch-fetch-quest ()
  (let* ((demo-width (first (getconfig 'game-resolution *config*)))
         (demo-height (second (getconfig 'game-resolution *config*)))
         (world (make-instance 'myscene
                               :drag-y 0.99
                               :width demo-width
                               :height demo-height
                               ;; :background (make-instance 'static-sprite
                               ;;                            :path-to-sprite (resource-path "background.png")
                               ;;                            :width demo-width
                               ;;                            :height demo-height)
                               ;; :music (resource-path "mysong.wav")
                               :camera (make-instance 'camera
                                                      :width (first (getconfig 'game-resolution *config*))
                                                      :height (second (getconfig 'game-resolution *config*))
                                                      :min-x 0 :min-y 0
                                                      :max-x demo-width
                                                      :max-y demo-height
                                                      :target-max-offset 0)))
         (player (make-instance 'player
                                :facing (list :south)
                                :simultainous-directions-p nil
                                :x (/ demo-width 2)
                                :y (/ demo-height 2)
                                :width 16
                                :height 32))
         (objects (list player
                        ;; put an invisible box around the boundary
                        (make-instance 'obb
                                       :x -1
                                       :y 0
                                       :width 1
                                       :height demo-height)
                        (make-instance 'obb
                                       :x 0
                                       :y -1
                                       :width demo-width
                                       :height 1)
                        (make-instance 'obb
                                       :x demo-width
                                       :y 0
                                       :height demo-height
                                       :width 1)
                        (make-instance 'obb
                                       :x 0
                                       :y demo-height
                                       :height 1
                                       :width demo-width))))
    (setf *player* player)
    (setf (clear-color *engine-manager*)
          *green*)
    (loop for game-object in objects do
         (add-to-scene world game-object))
    (setf (target (camera world)) player)
    (setf (active-input-device player) -1)
    world))

#+nil
(recurse.vert:main #'fetch-quest::game-menu
                   :config *fetch-quest-config*
                   :block nil
                   :dev-mode (make-config ()
                                          ('dev-mode-performance-hud t)
                                          ('dev-mode-render-collision-hitboxes nil)))

#+nil
(recurse.vert:main #'fetch-quest::game-menu
                   :config  (make-config (*fetch-quest-config*)
                                         ('enable-compositor t)
                                         ('fullscreen-p nil))
                   :block nil
                   :dev-mode (make-config ()
                                          ('dev-mode-performance-hud t)
                                          ('dev-mode-render-collision-hitboxes nil)))
