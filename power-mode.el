;;; power-mode.el --- Imbue Emacs with power! -*- lexical-binding:t -*-

;; Copyright (c) 2021 Eliza Velasquez

;; Author: Eliza Velasquez
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Created: 3 Jun 2021
;; Keywords: games
;; URL: https://github.com/elizagamedev/power-mode.el

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; At long last, Power Mode [1] is available for the best text editor.
;;
;; * What does this do?
;;
;; While `power-mode' is enabled, typing quickly and without pause will
;; eventually rack up a combo-meter with each keypress shaking the Emacs window
;; and showering each newly inserted character with explosive particles.
;;
;; * Caveats
;;
;; - Don't use this on EXWM or you'll regret it.
;;
;; - If running Emacs as a daemon, frames might behave weirdly if shaking is
;;   enabled and you exit power-mode.  It's safe to close them and re-open new
;;   ones.
;;
;; - The shaking windows feature may altogether behave terribly and ruin your
;;   Emacs session.  You can disable this while still retaining particle effects
;;   by setting `power-mode-streak-shake-threshold' to nil.
;;
;; [1] https://github.com/codeinthedark/awesome-power-mode


;;; Code:

(require 'cl-lib)
(require 'hl-line)

(defgroup power nil
  "Imbue Emacs with power."
  :group 'emacs)

(defcustom power-mode-streak-combo-threshold
  20
  "Streak required before combo counter is displayed.

Set to nil to disable combo counter.

Not implemented yet."
  :type '(choice integer (const nil))
  :group 'power)

(defcustom power-mode-streak-shake-threshold
  20
  "Streak required before shake effects activate.

Set to nil to disable shake effects."
  :type '(choice integer (const nil))
  :group 'power)

(defcustom power-mode-streak-static-particle-color
  nil
  "User-defined static streak particle color.

If nil, the dynamic foreground text color is used."
  :type '(choice color (const nil))
  :group 'power)

(defcustom power-mode-streak-particle-threshold
  20
  "Streak required before particle effects activate.

Set to nil to disable particle effects."
  :type '(choice integer (const nil))
  :group 'power)

(defcustom power-mode-streak-timeout
  10
  "Timeout to reset the streak counter, in seconds."
  :type 'integer
  :group 'power)

(defcustom power-mode-shake-strength
  6
  "Strength of shake effect."
  :type 'integer
  :group 'power)

(defcustom power-mode-particle-range
  '(1 . 3)
  "Range of particles to spawn for each character, inclusive."
  :type '(cons integer integer)
  :group 'power)

(defcustom power-mode-particle-limit
  20
  "Maximum number of particles that can be on-screen."
  :type 'integer
  :group 'power)

(defcustom power-mode-particle-life-expectancy
  10
  "The time a particle lives before it has to go."
  :type 'integer
  :group 'power)

(defcustom power-mode-particle-visibility-method
  (if (boundp 'pgtk-initialized)
      'yeet
    'toggle-visibility)
  "Method to use when hiding particles."
  :type '(choice (const :tag "Toggle frame visibility" toggle-visibility)
                 (const :tag "Move into the bottom-right corner (best on pgtk)" yeet)))

;;;; Common

(defvar power-mode--dummy-buffer nil)

(defun power-mode--random-float ()
  "Return a random float from 0 to 1, exclusive."
  (/ (random (- most-positive-fixnum 1)) (float most-positive-fixnum)))

(defun power-mode--random-range (min max)
  "Return a random number between MIN and MAX, inclusive."
  (+ min (random (+ 1 (- max min)))))

(defun power-mode--make-dummy-buffer ()
  "Make dummy buffer with no chrome."
  (let ((buffer (get-buffer-create " *power-mode*")))
    (with-current-buffer buffer
      (setq-local mode-line-format nil
                  header-line-format nil
                  frame-title-format ""
                  truncate-lines t
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  show-trailing-whitespace nil
                  display-line-numbers nil
                  left-fringe-width nil
                  right-fringe-width nil
                  left-margin-width nil
                  right-margin-width nil
                  fringes-outside-margins 0))
    buffer))

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
(defvar power-mode--allow-focus-parent nil)

(defconst power-mode--parent-parameters
  '((cursor-type . nil)
    ;; Replace old title. The top-level buffer is no longer meaningful and Emacs
    ;; randomly switches between single and multi frame mode, causing the title
    ;; to flicker.
    (title . "GNU Emacs POWER MODE")
    (no-other-window . t)))

(defun power-mode--make-save-key (key)
  "Make key based on KEY for saving and restoring parent frame parameters."
  (intern (concat "power-mode--"
                  (symbol-name key))))

(defun power-mode--shake ()
  "Shake effect function to be called at an interval."
  (if (<= power-mode--shake-amplitude 0)
      (progn
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil)
        (modify-frame-parameters
         power-mode--shake-frame `((left . 0)
                                   (top . 0))))
    (progn
      (let ((angle (* (power-mode--random-float) 2.0 float-pi)))
        (modify-frame-parameters
         power-mode--shake-frame
         `((left . ,(truncate (* (cos angle) power-mode--shake-amplitude)))
           (top . ,(truncate (* (sin angle) power-mode--shake-amplitude)))))
        (cl-decf power-mode--shake-amplitude)))))

(defun power-mode--make-shake-frame (frame)
  "Make a shake frame and insert it into FRAME."
  (let ((frame-parameters (copy-alist (frame-parameters frame))))
    ;; Copy given frame parameters, overriding some of its options.
    (dolist (pair `((parent-id . nil)
                    (window-id . nil)
                    (outer-window-id . nil)
                    (left . 0)
                    (top . 0)
                    (parent-frame . ,frame)))
      (setf (alist-get (car pair) frame-parameters nil t) (cdr pair)))
    ;; Make old parent window point to dummy buffer.
    (with-selected-frame frame
      (delete-other-windows)
      (switch-to-buffer power-mode--dummy-buffer)
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) frame) t))
    ;; Override parent parameters.
    (dolist (pair power-mode--parent-parameters)
      (set-frame-parameter frame
                           (power-mode--make-save-key (car pair))
                           (frame-parameter frame (car pair)))
      (set-frame-parameter frame (car pair) (cdr pair)))
    ;; Make and focus new frame.
    (let ((new-frame (make-frame frame-parameters)))
      (select-frame-set-input-focus new-frame)
      (push `(,frame . ,new-frame) power-mode--shake-frames))))

(defun power-mode--delete-shake-frame (parent-frame shake-frame)
  "Remove SHAKE-FRAME from PARENT-FRAME and restore its previous state."
  (let ((power-mode--allow-focus-parent t)
        (buffer (with-selected-frame shake-frame
                  (current-buffer))))
    (delete-frame shake-frame)
    (with-selected-frame parent-frame
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) parent-frame) nil)
      (switch-to-buffer buffer))
    ;; Restore old parameters.
    (dolist (pair power-mode--parent-parameters)
      (let ((save-key (power-mode--make-save-key (car pair))))
        (set-frame-parameter parent-frame (car pair)
                             (frame-parameter parent-frame save-key))
        (set-frame-parameter parent-frame save-key nil)))))

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
  (if power-mode-streak-static-particle-color
    power-mode-streak-static-particle-color
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
        color)))))

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
        (push frame power-mode--particle-live-frames)
        (let ((x (- (car position)
                    (/ (frame-pixel-width frame) 2)))
              (y (- (cdr position)
                    (/ (frame-pixel-height frame) 2))))
          (modify-frame-parameters
           frame
           `((parent-frame . ,parent-frame)
             (background-color . ,color)
             (power-mode--life . ,power-mode-particle-life-expectancy)
             (power-mode--vx . ,(power-mode--random-range -5 5))
             (power-mode--vy . ,(power-mode--random-range -10 -6))
             (power-mode--x . ,x)
             (power-mode--y . ,y)
             (left . ,x)
             (top . ,y)
             (visibility . t))))))))

(defun power-mode--animate-particles ()
  "Periodic function to be called in a timer to animate particles."
  (let ((live-particles nil))
    (dolist (frame power-mode--particle-live-frames)
      (let ((life (- (frame-parameter frame 'power-mode--life) 1)))
        (if (<= life 0)
            (progn
              (push frame power-mode--particle-dead-frames)
              (power-mode--hide-particle-frame frame))
          (push frame live-particles)
          (let* ((vx (frame-parameter frame 'power-mode--vx))
                 (vy (frame-parameter frame 'power-mode--vy))
                 (x (+ vx (frame-parameter frame 'power-mode--x)))
                 (y (+ vy (frame-parameter frame 'power-mode--y))))
            (modify-frame-parameters
             frame
             `((power-mode--life . ,life)
               (power-mode--vy . ,(+ vy 1))
               (power-mode--x . ,x)
               (power-mode--y . ,y)))
            (if (and (>= x 0) (< x (frame-native-width))
                     (>= y 0) (< y (frame-native-height)))
                (modify-frame-parameters frame `((left . ,x)
                                                 (top . ,y)
                                                 (visibility . t)))
              (power-mode--hide-particle-frame frame))))))
    (setq power-mode--particle-live-frames live-particles)
    (unless live-particles
      (cancel-timer power-mode--particle-timer)
      (setq power-mode--particle-timer nil))))

(defun power-mode--make-particle-frame (parent-frame)
  "Make an invisible particle attached to PARENT-FRAME."
  (let ((frame-inhibit-implied-resize nil)
        (frame (make-frame `((name . "particle")
                             (parent-frame . ,parent-frame)
                             (width . 2)
                             (height . 1)
                             (border-width . 0)
                             (child-frame-border-width . 0)
                             (cursor-type . nil)
                             (desktop-dont-save . t)
                             (horizontal-scroll-bars . nil)
                             (internal-border-width . 0)
                             (inhibit-double-buffering . t)
                             (left-fringe . 0)
                             (line-spacing . 0)
                             (menu-bar-lines . 0)
                             (min-height . 1)
                             (min-width . 1)
                             (minibuffer . nil)
                             (no-accept-focus . t)
                             (no-focus-on-map . t)
                             (no-other-frame . t)
                             (no-special-glyphs . t)
                             (power-mode--life . 0)
                             (right-fringe . 0)
                             (tab-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (undecorated . t)
                             (unsplittable . t)
                             (vertical-scroll-bars . nil)
                             (visibility . nil)
                             (left . ,(if (eq power-mode-particle-visibility-method 'yeet)
                                          (frame-native-width) 0))
                             (top . ,(if (eq power-mode-particle-visibility-method 'yeet)
                                         (frame-native-width) 0))))))
    ;; Shrink font.
    (set-face-attribute 'default frame
                        :height (/ (face-attribute
                                    'default :height
                                    frame)
                                   (power-mode--random-range 3 6)))

    ;; Switch to dummy buffer.
    (with-selected-frame frame
      (switch-to-buffer power-mode--dummy-buffer)
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) t) t))
    frame))

(defun power-mode--hide-particle-frame (frame)
  "Hide a particle FRAME.

Use the method `power-mode-particle-visibility-method'."
  (let ((parameters (cond
                     ((eq power-mode-particle-visibility-method 'toggle-visibility)
                      '((visibility . nil)))
                     ((eq power-mode-particle-visibility-method 'yeet)
                      `((left . ,(frame-native-width))
                        (top . ,(frame-native-height))
                        (background-color . ,(frame-parameter nil 'background-color))))
                     (t (error "Unknown `power-mode-particle-visibility-method': %s"
                               power-mode-particle-visibility-method)))))
    (modify-frame-parameters frame parameters)))

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
                      (frame-height parent-frame))))
  (when (eq power-mode-particle-visibility-method 'yeet)
    (dolist (frame power-mode--particle-dead-frames)
      (power-mode--hide-particle-frame frame))))

(defun power-mode--window-selection-change-function (parent-frame)
  "Power-mode hook for `window-selection-change-functions'.

Accepts PARENT-FRAME."
  (when (and (not power-mode--allow-focus-parent)
             (framep parent-frame))
    (when-let ((child-frame (cdr (assq parent-frame power-mode--shake-frames))))
      (select-frame-set-input-focus child-frame))))

;;;###autoload
(define-minor-mode power-mode
  "Imbue Emacs with power."
  :init-value nil
  :lighter " power"
  :global t
  :group 'power
  (if power-mode
      (progn
        (add-hook 'post-self-insert-hook
                  #'power-mode--post-self-insert-hook)
        (add-hook 'delete-frame-functions
                  #'power-mode--delete-frame-function)
        (add-hook 'window-size-change-functions
                  #'power-mode--window-size-change-function)
        (add-hook 'window-selection-change-functions
                  #'power-mode--window-selection-change-function)
        ;; Create dummy buffer.
        (setq power-mode--dummy-buffer (power-mode--make-dummy-buffer))
        ;; Make particle frames.
        (when power-mode-streak-particle-threshold
          (dotimes (_ power-mode-particle-limit)
            (setq power-mode--particle-dead-frames
                  (cons (power-mode--make-particle-frame (selected-frame))
                        power-mode--particle-dead-frames))))
        ;; Make shake frames for all top-level frames.
        (when power-mode-streak-shake-threshold
          (dolist (frame (frame-list))
            (unless (frame-parent frame)
              (power-mode--make-shake-frame frame)))))
    (progn
      (remove-hook 'post-self-insert-hook
                   #'power-mode--post-self-insert-hook)
      (remove-hook 'delete-frame-functions
                   #'power-mode--delete-frame-function)
      (remove-hook 'window-size-change-functions
                   #'power-mode--window-size-change-function)
      (remove-hook 'window-selection-change-functions
                   #'power-mode--window-selection-change-function)
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
