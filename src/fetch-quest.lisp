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


(defclass rectangle (static-sprite)
  ((path-to-sprite :initform (resource-path "rectangle.png")
                   :reader path-to-sprite))
  (:documentation "A basic rectangle with obb collision detection."))

(defvar *player* nil)

(defclass player (rectangle kinematic-object agent input-handler)
  ()
  (:documentation "Player-controlled rectangle."))

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

(let* ((move-delta 300)
       (right-vec  (make-acceleration-vector-seconds :x move-delta))
       (left-vec   (make-acceleration-vector-seconds :x (- move-delta)))
       (up-vec     (make-acceleration-vector-seconds :y (- move-delta)))
       (down-vec   (make-acceleration-vector-seconds :y move-delta)))
  (set-default-command-action-map
   player
   (:move-right (while-active
                 (apply-vector player right-vec)))
   (:move-left (while-active
                (apply-vector player left-vec)))
   (:move-up (while-active
              (apply-vector player up-vec)))
   (:move-down (while-active
                (apply-vector player down-vec)))))

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
                                :color *orange*
                                :x (/ demo-width 2)
                                :y (/ demo-height 2)
                                :width 5
                                :height 5))
         (objects (list player
                        ;; put an invisible box around the boundary
                        (make-instance 'obb
                                       :x 0
                                       :y 0
                                       :width 1
                                       :height demo-height)
                        (make-instance 'obb
                                       :x 0
                                       :y 0
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
