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

(defclass tile (animated-sprite static-object)
  ())

(defclass my-scene-input-handler (input-handler)
  ())

(set-default-input-command-map
 my-scene-input-handler
 ("sdl-keyboard" (:scancode-q :quit)))

(set-default-command-action-map
 my-scene-input-handler
 (:quit              (on-deactivate (quit))))

(defvar *world-layer-0* 0)
(defvar *object-layer-0* 1)

(defclass fetch-quest-scene (tiled-scene)
  ((scene-input-handler
    :documentation "Input handler for the main scene."
    :initform (make-instance 'my-scene-input-handler
                             :active-input-device *all-input-id*))
   (tile-width-px :initform nil
                  :accessor tile-width-px)
   (tile-height-px :initform nil
                   :accessor tile-height-px))
  (:documentation "Default scene for my game"))

(defmethod initialize-instance :after ((scene fetch-quest-scene) &rest args)
  (declare (ignore args))
  (with-slots (update-area) scene
    (setf update-area
          (make-active-area :min-x 0.0 :max-x 0.0 :min-y 0.0 :max-y 0.0))))

(defmethod on-map-read ((scene fetch-quest-scene) tiled-map-path map-num-cols map-num-rows map-tile-width map-tile-height)
  (setf (tile-width-px scene) map-tile-width
        (tile-height-px scene) map-tile-height
        (width scene) (* (tile-width-px scene) map-num-cols)
        (height scene) (* (tile-height-px scene) map-num-rows))
  (when (recurse.vert::max-x (camera scene))
    (setf (recurse.vert::max-x (camera scene)) (width scene)))
  (when (recurse.vert::max-y (camera scene)) (height scene)
        (setf (recurse.vert::max-y (camera scene)) (height scene))))

(defmethod on-tile-read ((tiled-scene fetch-quest-scene) layer-json tileset tile-map-col tile-map-row tile-source-col tile-source-row)
  (labels ((prop-val (key object)
             (cdr (assoc key (rest (assoc :properties object))))))
    (add-to-scene
     tiled-scene
     (make-instance 'tile
                    :width (tile-width-px tiled-scene) :height (tile-height-px tiled-scene)
                    :x (* (tile-width-px tiled-scene) tile-map-col)
                    :y (* (tile-height-px tiled-scene) tile-map-row)
                    :z (read-from-string (null-fallback (prop-val :z layer-json) "0"))
                    :animations (list :static (make-animation :spritesheet (tileset-image-source tileset)
                                                              :frames (vector (make-sprite-source
                                                                               (* (tileset-tile-width tileset) tile-source-col)
                                                                               (* (tileset-tile-height tileset) tile-source-row)
                                                                               (tileset-tile-width tileset)
                                                                               (tileset-tile-height tileset)))))))))

(defmethod update :before ((scene fetch-quest-scene) delta-t-ms world-context)
  (with-slots (update-area camera) scene
    (let ((update-radius 200))
      (setf (active-area-min-x update-area) (- (x camera) update-radius)
            (active-area-max-x update-area) (+ (x camera) (width camera) update-radius)
            (active-area-min-y update-area) (- (y camera) update-radius)
            (active-area-max-y update-area) (+ (y camera) (height camera) update-radius))))
  (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

;;;; Game Launcher

(defun launch-fetch-quest ()
  (let* ((demo-width (first (getconfig 'game-resolution *config*)))
         (demo-height (second (getconfig 'game-resolution *config*)))
         (world (make-instance 'fetch-quest-scene
                               :tiled-map (resource-path "tiled/overworld.json")
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
                                :x (* 16 4)
                                :y (* 16 4)
                                :z *object-layer-0*
                                :width 16
                                :height 32))
         (objects (list player
                        ;; put an invisible box around the boundary
                        (make-instance 'obb
                                       :x -1
                                       :y 0
                                       :width 1
                                       :height (height world))
                        (make-instance 'obb
                                       :x 0
                                       :y -1
                                       :width (width world)
                                       :height 1)
                        (make-instance 'obb
                                       :x (width world)
                                       :y 0
                                       :height (height world)
                                       :width 1)
                        (make-instance 'obb
                                       :x 0
                                       :y (height world)
                                       :height 1
                                       :width (width world)))))
    (setf *player* player)
    (setf (clear-color *engine-manager*) *black*)
    (loop :for game-object :in objects :do
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
