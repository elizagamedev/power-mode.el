;;; power-mode.el --- -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defgroup power-mode nil
  "Imbue Emacs with power."
  :group 'emacs)

(defcustom power-mode-streak-combo-threshold
  20
  "Streak required before combo counter is displayed.

Set to nil to disable combo counter."
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

(defvar power-mode--dummy-buffer nil)

(defvar power-mode--shake-frames nil
  "Alist of parent to child.")
(defvar power-mode--shake-amplitude 0)
(defvar power-mode--shake-frame nil)
(defvar power-mode--shake-timer nil)

(defvar power-mode--streak 0)
(defvar power-mode--streak-timeout-timer nil)

(defun power-mode--streak-timeout ()
  "Streak timeout function."
  (setq power-mode--streak 0
        power-mode--streak-timeout-timer nil))

(defun power-mode--random-angle ()
  (/ (* 2 float-pi (random 1024)) 1024))

(defun power-mode--shake ()
  "Shake effect function to be called at an interval."
  (if (<= power-mode--shake-amplitude 0)
      (progn
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil)
        (dolist (parameter '(left top))
          (set-frame-parameter power-mode--shake-frame parameter 0)))
    (progn
      (let ((angle (power-mode--random-angle)))
        (set-frame-parameter
         power-mode--shake-frame 'left
         (truncate (* (cos angle) power-mode--shake-amplitude)))
        (set-frame-parameter
         power-mode--shake-frame 'top
         (truncate (* (sin angle) power-mode--shake-amplitude)))
        (cl-decf power-mode--shake-amplitude)))))

(defun power-mode--post-self-insert-hook ()
  "Power-mode hook for post-self-insert."
  (cl-incf power-mode--streak)
  ;; Reset streak timeout.
  (when power-mode--streak-timeout-timer
    (cancel-timer power-mode--streak-timeout-timer))
  (setq power-mode--streak-timeout-timer
        (run-with-timer power-mode-streak-timeout nil
                        #'power-mode--streak-timeout))
  ;; Start shake effects if they aren't already.
  (when (and power-mode-streak-shake-threshold
             (>= power-mode--streak power-mode-streak-shake-threshold)
             (rassq (selected-frame) power-mode--shake-frames))
    (setq power-mode--shake-amplitude power-mode-shake-strength)
    (unless power-mode--shake-timer
      (setq power-mode--shake-frame (selected-frame)
            power-mode--shake-timer (run-with-timer 0 0.05
                                                    #'power-mode--shake)))))

(defun power-mode--make-shake-frame (frame)
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
    ;; Make and focus new frame.
    (let ((new-frame (make-frame frame-parameters)))
      (setq power-mode--shake-frames
            (cons `(,frame . ,new-frame) power-mode--shake-frames))
      (select-frame-set-input-focus new-frame))))

(defun power-mode--delete-shake-frame (parent-frame shake-frame)
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
    (set-frame-parameter parent-frame 'power-mode--cursor-type nil)))

(defun power-mode--make-particle-frame (parent-frame color x y)
  (let ((frame (make-frame `((parent-frame . ,parent-frame)
                             (width . 2)
                             (height . 1)
                             (min-width . 0)
                             (min-height . 0)
                             (left . ,x)
                             (top . ,y)
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
                             (background-color . ,color)
                             (visibility . nil)))))
    (set-face-attribute 'default frame
                        :height (/ (face-attribute
                                    'default :height
                                    parent-frame) 4))
    (with-selected-frame frame
      (switch-to-buffer power-mode--dummy-buffer)
      (set-window-dedicated-p
       (get-buffer-window (current-buffer) t) t))
    (set-frame-parameter frame 'visibility t)
    frame))

(defun power-mode--delete-frame-function (child-frame)
  (when-let (parent-frame (car (rassq child-frame power-mode--shake-frames)))
    (setq power-mode--shake-frames (assq-delete-all
                                    parent-frame power-mode--shake-frames))
    (delete-frame parent-frame)))

(defun power-mode--window-size-change-function (parent-frame)
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
              (let ((buffer (get-buffer-create " *power-mode dummy*")))
                (with-current-buffer buffer
                  (setq-local mode-line-format nil
                              buffer-read-only t))
                buffer))
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
      ;; Delete shake frames.
      (dolist (pair power-mode--shake-frames)
        (power-mode--delete-shake-frame (car pair) (cdr pair)))
      (setq power-mode--shake-frames nil)
      ;; Kill dummy buffer.
      (kill-buffer power-mode--dummy-buffer)
      (setq power-mode--dummy-buffer nil)
      ;; Force pending timeouts to activate.
      (when power-mode--shake-timer
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil))
      (when power-mode--streak-timeout-timer
        (power-mode--streak-timeout)))))

(provide 'power-mode)

;;; power-mode.el ends here
