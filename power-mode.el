;;; power-mode.el --- Imbue Emacs with power! -*- lexical-binding:t -*-

;; Version: 0.1.0
;; Author: Eliza Velasquez
;; Created: 3 Jun 2021
;; Keywords: power-mode
;; URL: https://github.com/elizagamedev/power-mode.el

;;; Commentary:

;; At long last, Power Mode is available for the best text editor.
;;
;; See README.md for more information.

;;; Code:

(require 'cl-lib)
(require 'hl-line)

(defgroup power-mode nil
  "Imbue Emacs with power."
  :group 'emacs)

(defcustom power-mode-streak-combo-threshold
  20
  "Streak required before combo counter is displayed.

Set to nil to disable combo counter.

(Not implemented yet.)"
  :type '(choice integer (const nil))
  :group 'power-mode)

(defcustom power-mode-streak-shake-threshold
  20
  "Streak required before shake effects activate.

Set to nil to disable shake effects."
  :type '(choice integer (const nil))
  :group 'power-mode)

(defcustom power-mode-streak-particle-threshold
  20
  "Streak required before particle effects activate.

Set to nil to disable particle effects."
  :type '(choice integer (const nil))
  :group 'power-mode)

(defcustom power-mode-streak-timeout
  10
  "Timeout to reset the streak counter, in seconds."
  :type 'integer
  :group 'power-mode)

(defcustom power-mode-shake-strength
  6
  "Strength of shake effect."
  :type 'integer
  :group 'power-mode)

(defcustom power-mode-particle-range
  '(1 . 3)
  "Range of particles to spawn for each character, inclusive."
  :type (cons 'integer 'integer)
  :group 'power-mode)

(defcustom power-mode-particle-limit
  20
  "Maximum number of particles that can be on-screen."
  :type 'integer
  :group 'power-mode)

;;;; Common

(defvar power-mode--dummy-buffer nil)

(defun power-mode--random-float ()
  "Return a random float from 0 to 1, exclusive."
  (/ (random (- most-positive-fixnum 1)) (float most-positive-fixnum)))

(defun power-mode--random-range (min max)
  "Return a random number between MIN and MAX, inclusive."
  (+ min (random (+ 1 (- max min)))))

;;;; Streak

(defvar power-mode--streak 0)
(defvar power-mode--streak-timeout-timer nil)

(defun power-mode--streak-timeout ()
  "Streak timeout function."
  (setq power-mode--streak 0
        power-mode--streak-timeout-timer nil))

;;;; Shake Effect

(defvar power-mode--shake-frames nil
  "Alist of parent to child.")
(defvar power-mode--shake-amplitude 0)
(defvar power-mode--shake-frame nil)
(defvar power-mode--shake-timer nil)

(defun power-mode--shake ()
  "Shake effect function to be called at an interval."
  (if (<= power-mode--shake-amplitude 0)
      (progn
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil)
        (dolist (parameter '(left top))
          (set-frame-parameter power-mode--shake-frame parameter 0)))
    (progn
      (let ((angle (* (power-mode--random-float) 2.0 float-pi)))
        (set-frame-parameter
         power-mode--shake-frame 'left
         (truncate (* (cos angle) power-mode--shake-amplitude)))
        (set-frame-parameter
         power-mode--shake-frame 'top
         (truncate (* (sin angle) power-mode--shake-amplitude)))
        (cl-decf power-mode--shake-amplitude)))))

(defun power-mode--make-shake-frame (frame)
  "Make a shake frame and insert it into FRAME."
  (let ((frame-parameters (copy-alist (frame-parameters frame))))
    ;; Copy given frame parameters, overriding some of its options.
    (dolist (key '(parent-id
                   window-id
                   outer-window-id))
      (setq frame-parameters (assq-delete-all key frame-parameters)))
    (dolist (pair `((left . 0)
                    (top . 0)
                    (parent-frame . ,frame)))
      (setcdr (assq (car pair) frame-parameters) (cdr pair)))
    ;; Make old parent window point to dummy buffer.
    (with-selected-frame frame
      (delete-other-windows)
      (switch-to-buffer power-mode--dummy-buffer)
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) t) t))
    ;; Hide old parent cursor.
    (set-frame-parameter frame
                         'power-mode--cursor-type
                         (frame-parameter frame 'cursor-type))
    (set-frame-parameter frame 'cursor-type nil)
    ;; Replace old title. The top-level buffer is no longer meaningful and Emacs
    ;; randomly switches between single and multi frame mode, causing the title
    ;; to flicker.
    (set-frame-parameter frame
                         'power-mode--title
                         (frame-parameter frame 'title))
    (set-frame-parameter frame 'title "GNU Emacs POWER MODE")
    ;; Make and focus new frame.
    (let ((new-frame (make-frame frame-parameters)))
      (setq power-mode--shake-frames
            (cons `(,frame . ,new-frame) power-mode--shake-frames))
      (select-frame-set-input-focus new-frame))))

(defun power-mode--delete-shake-frame (parent-frame shake-frame)
  "Remove SHAKE-FRAME from PARENT-FRAME and restore its previous state."
  (let ((buffer (with-selected-frame shake-frame
                  (current-buffer))))
    (delete-frame shake-frame)
    (with-selected-frame parent-frame
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) t) nil)
      (switch-to-buffer buffer))
    ;; Restore old cursor.
    (set-frame-parameter
     parent-frame
     'cursor-type
     (frame-parameter parent-frame 'power-mode--cursor-type))
    (set-frame-parameter parent-frame 'power-mode--cursor-type nil)
    ;; Restore old title.
    (set-frame-parameter
     parent-frame
     'title
     (frame-parameter parent-frame 'power-mode--title))
    (set-frame-parameter parent-frame 'power-mode--title nil)))

;;;; Particle Effect

(defvar power-mode--particle-live-frames nil)
(defvar power-mode--particle-dead-frames nil)
(defvar power-mode--particle-timer nil)

(defun power-mode--point-frame-position ()
  "Get the position of the point relative to the top-left corner of the frame."
  (let ((edges (frame-edges))
        (position (window-absolute-pixel-position)))
    `(,(+ (- (car position) (nth 0 edges)) (/ (frame-char-width) 2))
      . ,(+ (- (cdr position) (nth 1 edges)) (/ (frame-char-height) 2)))))

(defun power-mode--foreground-color-before-point ()
  "Get the foreground color of the character before the point."
  (let ((mode hl-line-mode)
        (global-mode global-hl-line-mode))
    (hl-line-mode -1)
    (global-hl-line-mode -1)
    (save-excursion
      (when (< 0 (current-column))
        (backward-char))
      (let ((color (foreground-color-at-point)))
        (hl-line-mode mode)
        (global-hl-line-mode global-mode)
        color))))

(defun power-mode--spawn-particles-at-point ()
  "Spawn particles at the point."
  (unless power-mode--particle-timer
    (setq power-mode--particle-timer
          (run-with-timer 0 0.05
                          #'power-mode--animate-particles)))
  (let ((position (power-mode--point-frame-position))
        (count (power-mode--random-range (car power-mode-particle-range)
                                         (cdr power-mode-particle-range)))
        (color (power-mode--foreground-color-before-point))
        (parent-frame (selected-frame)))
    (dotimes (_ count)
      (when-let ((frame (pop power-mode--particle-dead-frames)))
        (setq power-mode--particle-live-frames
              (cons frame power-mode--particle-live-frames))
        (set-frame-parameter frame 'parent-frame parent-frame)
        (set-frame-parameter frame 'background-color color)
        (set-frame-parameter frame 'power-mode--life 10)
        (set-frame-parameter frame 'power-mode--vx
                             (power-mode--random-range -5 5))
        (set-frame-parameter frame 'power-mode--vy
                             (power-mode--random-range -10 -6))
        (let ((x (- (car position)
                    (/ (frame-pixel-width frame) 2)))
              (y (- (cdr position)
                    (/ (frame-pixel-height frame) 2))))
          (set-frame-parameter frame 'left x)
          (set-frame-parameter frame 'top y)
          (set-frame-parameter frame 'power-mode--x x)
          (set-frame-parameter frame 'power-mode--y y))
        (set-frame-parameter frame 'visibility t)))))

(defun power-mode--animate-particles ()
  "Periodic function to be called in a timer to animate particles."
  (let ((live-particles nil))
    (dolist (frame power-mode--particle-live-frames)
      (let ((life (- (frame-parameter frame 'power-mode--life) 1)))
        (if (<= life 0)
            (progn
              (setq power-mode--particle-dead-frames
                    (cons frame power-mode--particle-dead-frames))
              (set-frame-parameter frame 'visibility nil)
              (set-frame-parameter frame 'parent-frame nil))
          (progn
            (setq live-particles (cons frame live-particles))
            (set-frame-parameter frame 'power-mode--life life)
            (let* ((vx (frame-parameter frame 'power-mode--vx))
                   (vy (frame-parameter frame 'power-mode--vy))
                   (x (+ vx (frame-parameter frame 'power-mode--x)))
                   (y (+ vy (frame-parameter frame 'power-mode--y))))
              (set-frame-parameter frame 'power-mode--x x)
              (set-frame-parameter frame 'power-mode--y y)
              (set-frame-parameter frame 'power-mode--vy (+ vy 1))
              (if (or (< x 0) (>= x (frame-native-width))
                      (< y 0) (>= y (frame-native-height)))
                  (set-frame-parameter frame 'visibility nil)
                (progn
                  (set-frame-parameter frame 'left x)
                  (set-frame-parameter frame 'top y)
                  (set-frame-parameter frame 'visibility t))))))))
    (setq power-mode--particle-live-frames live-particles)
    (unless live-particles
      (cancel-timer power-mode--particle-timer)
      (setq power-mode--particle-timer nil))))

(defun power-mode--make-particle-frame ()
  "Make an invisible particle frame."
  (let ((frame (make-frame `((name . "particle")
                             (width . 2)
                             (height . 1)
                             (min-width . 0)
                             (min-height . 0)
                             (unsplittable . t)
                             (minibuffer . nil)
                             (border-width . 0)
                             (internal-border-width . 0)
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)
                             (left-fringe . 0)
                             (right-fringe . 0)
                             (menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (line-spacing . 0)
                             (no-accept-focus . t)
                             (no-other-frame . t)
                             (no-focus-on-map . t)
                             (cursor-type . nil)
                             (visibility . nil)
                             (power-mode--life . 0)
                             (power-mode--x . 0)
                             (power-mode--y . 0)
                             (power-mode--vx . 0)
                             (power-mode--vy . 0)))))
    ;; Shrink font.
    (set-face-attribute 'default frame
                        :height (/ (face-attribute
                                    'default :height
                                    frame) 4))
    ;; Switch to dummy buffer.
    (with-selected-frame frame
      (switch-to-buffer power-mode--dummy-buffer)
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) t) t))
    frame))

;;;; Hooks

(defun power-mode--post-self-insert-hook ()
  "Power-mode hook for `post-self-insert-hook'."
  (unless (minibufferp (current-buffer))
    (cl-incf power-mode--streak)
    ;; Reset streak timeout.
    (when power-mode--streak-timeout-timer
      (cancel-timer power-mode--streak-timeout-timer))
    (setq power-mode--streak-timeout-timer
          (run-with-timer power-mode-streak-timeout nil
                          #'power-mode--streak-timeout))
    ;; Start shake effect timer if needed.
    (when (and power-mode-streak-shake-threshold
               (>= power-mode--streak power-mode-streak-shake-threshold)
               (rassq (selected-frame) power-mode--shake-frames))
      (setq power-mode--shake-amplitude power-mode-shake-strength)
      (unless power-mode--shake-timer
        (setq power-mode--shake-frame (selected-frame)
              power-mode--shake-timer (run-with-timer 0 0.05
                                                      #'power-mode--shake))))
    ;; Spawn particles.
    (when (and power-mode-streak-particle-threshold
               (>= power-mode--streak power-mode-streak-particle-threshold))
      (power-mode--spawn-particles-at-point))))

(defun power-mode--delete-frame-function (child-frame)
  "Power-mode hook for `delete-frame-functions'.

Accepts CHILD-FRAME."
  (when-let (parent-frame (car (rassq child-frame power-mode--shake-frames)))
    (setq power-mode--shake-frames (assq-delete-all
                                    parent-frame power-mode--shake-frames))
    (delete-frame parent-frame)))

(defun power-mode--window-size-change-function (parent-frame)
  "Power-mode hook for `window-size-change-functions'.

Accepts PARENT-FRAME."
  (when (framep parent-frame)
    (when-let ((child-frame (cdr (assq parent-frame power-mode--shake-frames))))
      (set-frame-size child-frame
                      (frame-width parent-frame)
                      (frame-height parent-frame)))))

;;;###autoload
(define-minor-mode power-mode
  "Imbue Emacs with power."
  :init-value nil
  :lighter " power"
  :global t
  :group 'power-mode
  (if power-mode
      (progn
        (add-hook 'post-self-insert-hook
                  #'power-mode--post-self-insert-hook)
        (add-hook 'delete-frame-functions
                  #'power-mode--delete-frame-function)
        (add-hook 'window-size-change-functions
                  #'power-mode--window-size-change-function)
        ;; Create dummy buffer.
        (setq power-mode--dummy-buffer
              (let ((buffer (get-buffer-create " *power-mode*")))
                (with-current-buffer buffer
                  (setq-local mode-line-format nil
                              buffer-read-only t))
                buffer))
        ;; Make shake frames for all top-level frames.
        (when power-mode-streak-shake-threshold
          (dolist (frame (frame-list))
            (unless (frame-parent frame)
              (power-mode--make-shake-frame frame))))
        ;; Make particle frames.
        (when power-mode-streak-particle-threshold
          (dotimes (_ power-mode-particle-limit)
            (setq power-mode--particle-dead-frames
                  (cons (power-mode--make-particle-frame)
                        power-mode--particle-dead-frames)))))
    (progn
      (remove-hook 'post-self-insert-hook
                   #'power-mode--post-self-insert-hook)
      (remove-hook 'delete-frame-functions
                   #'power-mode--delete-frame-function)
      (remove-hook 'window-size-change-functions
                   #'power-mode--window-size-change-function)
      ;; Delete shake frames.
      (dolist (pair power-mode--shake-frames)
        (power-mode--delete-shake-frame (car pair) (cdr pair)))
      (setq power-mode--shake-frames nil)
      ;; Delete particle frames.
      (dolist (frame power-mode--particle-live-frames)
        (delete-frame frame))
      (setq power-mode--particle-live-frames nil)
      (dolist (frame power-mode--particle-dead-frames)
        (delete-frame frame))
      (setq power-mode--particle-dead-frames nil)
      ;; Kill dummy buffer.
      (kill-buffer power-mode--dummy-buffer)
      (setq power-mode--dummy-buffer nil)
      ;; Kill timers.
      (when power-mode--streak-timeout-timer
        (power-mode--streak-timeout))
      (when power-mode--shake-timer
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil))
      (when power-mode--particle-timer
        (cancel-timer power-mode--particle-timer)
        (setq power-mode--particle-timer nil)))))

(provide 'power-mode)

;;; power-mode.el ends here
