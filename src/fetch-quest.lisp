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
                         (change-scene *engine-manager* (launch-overworld))))
                ("Quit" (run-action (quit))))))
    menu))

;;;; collisions

(defclass simple-motion (obb)
  ((velocity :initform (vector2))))

(defmethod update-motion ((moving-object simple-motion) timestep scene)
  (with-slots (velocity) moving-object
    (unless (= 0.0 (x velocity) (y velocity))
      (with-collision-check (moving-object scene)
        (:position-update
         (incf (x moving-object) (x velocity))
         (incf (y moving-object) (y velocity)))
        (:on-collision other-object
                       (decf (x moving-object) (x velocity))
                       (decf (y moving-object) (y velocity))))
      (setf (x velocity) 0.0
            (y velocity) 0.0)))
  (values))

;;;; objects

(defclass rectangle (static-sprite)
  ((path-to-sprite :initform (resource-path "rectangle.png")
                   :reader path-to-sprite))
  (:documentation "A basic rectangle with obb collision detection."))

(defclass gift (static-sprite)
  ((path-to-sprite :initform (resource-path "gift.png")
                   :reader path-to-sprite)))

(defclass static-obb (obb static-object)
  ())

;;;; faceable entity
(defclass faceable (animated-sprite direction-tracker obb)
  ((recurse.vert::simultanious-directions-p :initform nil)
   (previous-position :initform (vector2)))
  (:documentation "An entity which can face or run north,south,east,west."))

(defmethod initialize-instance :after ((faceable faceable) &rest args)
  (declare (ignore args))
  (with-slots (animations) faceable
    (loop :for animation-name :in '(:facing-north
                                    :walking-north
                                    :facing-south
                                    :walking-south
                                    :facing-east
                                    :walking-east
                                    :facing-west
                                    :walking-west)
       :do
         (unless (find animation-name animations)
           (error "~A is faceable and must define an animation for ~A" faceable animation-name)))))

(defmethod get-new-animation ((faceable faceable))
  (labels ((update-movement-tracker (faceable)
             (with-slots (previous-position) faceable
               (prog1 (or (/= (x previous-position) (x faceable))
                          (/= (y previous-position) (y faceable)))
                 ;; rest previous position
                 (setf (x previous-position) (x faceable)
                       (y previous-position) (y faceable))))))
    (let ((moving-p (update-movement-tracker faceable)))
      (ecase (first (facing faceable))
        (:north (cond
                  (moving-p :walking-north)
                  (t :facing-north)))
        (:south (cond
                  (moving-p :walking-south)
                  (t :facing-south)))
        (:east (cond
                 (moving-p :walking-east)
                 (t :facing-east)))
        (:west (cond
                 (moving-p :walking-west)
                 (t :facing-west)))))))

;;;; npc

(defclass npc (faceable)
  ((recurse.vert:animations
    :initform (labels ((frame-at (row col)
                         (let ((frame-width 16)
                               (frame-height 32))
                           (list (* col frame-width) (* row frame-height) frame-width frame-height))))
                (list :facing-south (make-animation :spritesheet (resource-path "NPC_test.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 0 0)))
                                                    :time-between-frames-ms 500)
                      :facing-north (make-animation :spritesheet (resource-path "NPC_test.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 2 0)))
                                                    :time-between-frames-ms 500)
                      :facing-east (make-animation :spritesheet (resource-path "NPC_test.png")
                                                    :frames (vector (apply #'make-sprite-source (frame-at 1 0)))
                                                    :time-between-frames-ms 500)
                      :facing-west (make-animation :spritesheet (resource-path "NPC_test.png")
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
                                                    :time-between-frames-ms 250))))))

(defclass gift-collector-npc (npc)
  ((num-gifts-to-win-game :initform 3
                          :documentation "When player delivers this number of packages to the NPC the game is won.")
   (num-gifts-received :initform 0)))

;;;; player

(defvar *player* nil)

(defclass player (simple-motion faceable agent input-handler)
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
   (speed :initform 2)
   (collected-gifts :initform (list))
   (health :initarg :health :initform nil)
   (max-health :initform 3)
   (hud :initform nil))
  (:documentation "Player controlled game object"))

(defmethod initialize-instance :after ((player player) &rest args)
  (declare (ignore args))
  (with-slots (hud health max-health) player
    (unless health
      (setf health max-health))
    (setf hud (make-instance 'player-hud
                             :player player
                             :width (first (getconfig 'game-resolution *config*))
                             :height (second (getconfig 'game-resolution *config*))))))

(defmethod get-new-animation ((player player))
  ;; facing animations
  (call-next-method player))

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

(set-default-command-action-map
 player
 (:move-right (while-active
               (push-direction player :east)
               (setf (x (slot-value player 'velocity)) 2.0)))
 (:move-left (while-active
              (push-direction player :west)
              (setf (x (slot-value player 'velocity)) -2.0)))
 (:move-up (while-active
            (push-direction player :north)
            (setf (y (slot-value player 'velocity)) -2.0)))
 (:move-down (while-active
              (push-direction player :south)
              (setf (y (slot-value player 'velocity)) 2.0))))

(defmethod collision :after ((player player) (gift gift))
  (with-slots (collected-gifts) player
    (unless (find gift collected-gifts)
      (play-sound-effect *audio* (resource-path "gift-collect.wav"))
      (push gift collected-gifts)))
  (remove-from-scene *scene* gift))

(defmethod collision :after ((player player) (npc npc))
  (labels ((face-towards (object object-to-face)
             "Change OBJECT's direction to face OBJECT-TO-FACE"
             (declare (direction-tracker object object-to-face))
             (let ((object-to-face-direction (first (facing object-to-face))))
               (push-direction object
                               (ecase object-to-face-direction
                                 (:north :south)
                                 (:south :north)
                                 (:east :west)
                                 (:west :east))))))
    (face-towards npc player)))

(defmethod collision :after ((player player) (npc gift-collector-npc))
  (with-slots (collected-gifts) player
    (with-slots (num-gifts-to-win-game num-gifts-received) npc
      (loop :while (> (length collected-gifts) 0) :do
           (pop collected-gifts)
           (incf num-gifts-received))
      (when (>= num-gifts-received num-gifts-to-win-game)
        (change-scene *engine-manager* (won-game-menu) )))))

;;;; player HUD

(defmethod add-to-scene :after (scene (player player))
  (add-to-scene scene (slot-value player 'hud)))

(defmethod remove-from-scene :after (scene (player player))
  (remove-from-scene scene (slot-value player 'hud)))

(defclass player-hud (overlay)
  ((player :initform (error ":player requred") :initarg :player)
   (hearts :initform (make-array 0
                                 :element-type 'static-sprite
                                 :adjustable t
                                 :fill-pointer 0))
   (gifts :initform (make-array 0
                                :element-type 'static-sprite
                                :adjustable t
                                :fill-pointer 0))))

(defmethod update-user :before ((hud player-hud) delta-t-ms scene)
  (let* ((hud-icon-size 16)
         (w 16) (h 16)
         (full-heart-source (make-sprite-source (* w 4) (* h 0) w h))
         (empty-heart-source (make-sprite-source (* w 8) (* h 0) w h)))
    (labels ((create-health (hud row)
               (with-slots (player hearts) hud
                 (with-slots ((current-health health) max-health) player
                   (cond ((> (length hearts) max-health)
                          (loop :for i :from max-health :below (length hearts) :do
                               (release-resources (elt hearts i))
                               (setf (parent (elt hearts i)) nil))
                          (setf (fill-pointer hearts) max-health))
                         ((< (length hearts) max-health)
                          (loop :for i :from (length hearts) :below max-health :do
                               (let ((heart (make-instance 'static-sprite
                                                           :path-to-sprite (resource-path "objects.png")
                                                           :sprite-source full-heart-source
                                                           :parent hud
                                                           :x (* i hud-icon-size)
                                                           :y (* row hud-icon-size)
                                                           :width hud-icon-size
                                                           :height hud-icon-size)))
                                 (vector-push-extend heart hearts))
                               (load-resources (elt hearts i) (rendering-context *engine-manager*))))
                         (t (loop :for i :from 0
                               :for heart :across hearts :do
                                 (if (>= i current-health)
                                     (setf (sprite-source heart) empty-heart-source)
                                     (setf (sprite-source heart) full-heart-source))))))))
             (create-gifts (hud row)
               (with-slots (player (hud-gifts gifts)) hud
                 (with-slots ((player-gifts collected-gifts)) player
                   (cond ((> (length hud-gifts) (length player-gifts))
                          (loop :for i :from (length player-gifts) :below (length hud-gifts) :do
                               (release-resources (elt hud-gifts i))
                               (setf (parent (elt hud-gifts i)) nil))
                          (setf (fill-pointer hud-gifts) (length player-gifts)))
                         ((< (length hud-gifts) (length player-gifts))
                          (loop :for i :from (length hud-gifts) :below (length player-gifts) :do
                               (let ((gift (make-instance 'static-sprite
                                                           :path-to-sprite (resource-path "gift.png")
                                                           :parent hud
                                                           :x (* i hud-icon-size)
                                                           :y (* row hud-icon-size)
                                                           :width hud-icon-size
                                                           :height hud-icon-size)))
                                 (vector-push-extend gift hud-gifts))
                               (load-resources (elt hud-gifts i) (rendering-context *engine-manager*)))))))))
      (let ((row -1))
        (create-health hud (incf row))
        (create-gifts hud (incf row))))))

;;;; game scene

(defclass tile (animated-sprite static-object)
  ())

(defclass my-scene-input-handler (input-handler)
  ())

(set-default-input-command-map
 my-scene-input-handler
 ("sdl-keyboard" (:scancode-q :quit)
                 (:scancode-r :reload)))

(set-default-command-action-map
 my-scene-input-handler
 (:quit              (on-deactivate (quit)))
 (:reload            (on-deactivate (reload *scene*))))

(defvar *world-layer-0* 0)
(defvar *object-layer-0* 1)

(defclass fetch-quest-scene (tiled-scene)
  ((scene-input-handler
    :documentation "Input handler for the main scene."
    :initform (make-instance 'my-scene-input-handler
                             :active-input-device *all-input-id*))
   (reload-fn :initarg :reload-fn
              :initform nil
              :documentation "A (optional) zero-arg function which will reset the scene to its initial state (except for the player's position)")
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
        ;; pad out the size of the tiles a little bit
        (width scene) (* (tile-width-px scene) (+ map-num-cols 4))
        (height scene) (* (tile-height-px scene) (+ map-num-rows 4)))
  (when (recurse.vert::max-x (camera scene))
    (setf (recurse.vert::max-x (camera scene))
          (* (tile-width-px scene) (+ map-num-cols 0))))
  (when (recurse.vert::max-y (camera scene)) (height scene)
        (setf (recurse.vert::max-y (camera scene))
              (* (tile-height-px scene) (+ map-num-rows 0)))))

(defmethod on-tile-read ((tiled-scene fetch-quest-scene) layer-json tileset tile-number tile-map-col tile-map-row tile-source-col tile-source-row)
  (labels ((prop-val (key object)
             (cdr (assoc key (rest (assoc :properties object))))))
    (let ((tile (make-instance 'tile
                               :width (tile-width-px tiled-scene) :height (tile-height-px tiled-scene)
                               :x (* (tile-width-px tiled-scene) tile-map-col)
                               :y (* (tile-height-px tiled-scene) tile-map-row)
                               :z (read-from-string (null-fallback (prop-val :z layer-json) "0"))
                               :animations (list :static (make-animation :spritesheet (tileset-image-source tileset)
                                                                         :frames (vector (make-sprite-source
                                                                                          (* (tileset-tile-width tileset) tile-source-col)
                                                                                          (* (tileset-tile-height tileset) tile-source-row)
                                                                                          (tileset-tile-width tileset)
                                                                                          (tileset-tile-height tileset))))))))
      (add-to-scene tiled-scene tile)

      ;; add any objects attached to the tile to the scene and make these objects children of the tile
      (let ((tile-objects-array (gethash tile-number (tileset-tile-objects tileset))))
        (when tile-objects-array
          (loop :for tile-object-json :across tile-objects-array :do
               (let ((game-object (on-object-read
                                   tiled-scene
                                   (make-tiled-object :props tile-object-json))))
                 (setf (parent game-object) tile))))))))

(defmethod on-object-read ((tiled-scene fetch-quest-scene) (object tiled-object))
  (labels ((prop-val (key object)
             (cdr (assoc key (tiled-object-props object))))
           (prop-val-symbol (key object)
             (alexandria:symbolicate (string-upcase (prop-val key object))))
           (custom-prop-val (key object)
             (cdr (assoc key (rest (assoc :properties (tiled-object-props object)))))))
    (let* ((*package* (find-package :fetch-quest))
           (game-object (eval `(make-instance ',(prop-val-symbol :type object)
                                              :object-id ,(prop-val :id object)
                                              :x ,(prop-val :x object)
                                              :y ,(prop-val :y object)
                                              :z ,(read-from-string (null-fallback  (custom-prop-val :z object) "0"))
                                              :width ,(prop-val :width object)
                                              :height ,(prop-val :height object)
                                              ,@(loop :with custom-props = (list)
                                                   :for custom-prop :in (prop-val :properties object) :do
                                                     (push (read-from-string (cdr custom-prop)) custom-props)
                                                     (push (car custom-prop) custom-props)
                                                   :finally (return custom-props))))))
      (add-to-scene tiled-scene game-object)
      game-object)))

(defmethod update :before ((scene fetch-quest-scene) delta-t-ms world-context)
  (with-slots (update-area camera) scene
    (let ((update-radius 200))
      (setf (active-area-min-x update-area) (- (x camera) update-radius)
            (active-area-max-x update-area) (+ (x camera) (width camera) update-radius)
            (active-area-min-y update-area) (- (y camera) update-radius)
            (active-area-max-y update-area) (+ (y camera) (height camera) update-radius))))
  (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

(defmethod reload ((scene fetch-quest-scene))
  (with-slots (reload-fn) scene
    (when (and *dev-mode* reload-fn)
      (let ((player-x (x *player*))
            (player-y (y *player*))
            (player-z (z *player*))
            (new-scene (funcall reload-fn)))
        (change-scene *engine-manager* new-scene t t)
        (schedule new-scene
                  0
                  (lambda ()
                    (setf (x *player*) player-x
                          (y *player*) player-y
                          (z *player*) player-z)))))))

;;;; Won game menu
(defun won-game-menu ()
  (setf (clear-color *engine-manager*) *black*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                "YOU WIN!!!"
                ("Play Again" (run-action
                               (change-scene *engine-manager* (launch-overworld))))
                ("Quit" (run-action (quit))))))
    menu))

;;;; Game Launcher

(defun launch-overworld ()
  (let* ((demo-width (first (getconfig 'game-resolution *config*)))
         (demo-height (second (getconfig 'game-resolution *config*)))
         (world (make-instance 'fetch-quest-scene
                               :reload-fn #'launch-overworld
                               :tiled-map (resource-path "tiled/overworld.json")
                               :music (resource-path "overworld-theme.wav")
                               :camera (make-instance 'camera
                                                      :width (* demo-width 2)
                                                      :height (* demo-height 2)
                                                      :min-x 0 :min-y 0
                                                      :max-x 1000 ; will be changed by tiled initializer
                                                      :max-y 1000 ; will be changed by tiled initializer
                                                      :target-max-offset 0))))
    (block find-player
      (do-spatial-partition (object (spatial-partition world))
        (when (typep object 'player)
          (setf *player* object)
          (return-from find-player)))
      (error "No player in map"))
    (setf (clear-color *engine-manager*) *black*)
    (setf (target (camera world)) *player*)
    (setf (active-input-device *player*) -1)
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
