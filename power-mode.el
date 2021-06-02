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

(defcustom power-mode-strength
  4
  "Strength of shake effect."
  :type 'integer
  :group 'power-mode)

(defvar power-mode--streak 0)
(defvar power-mode--streak-timeout-timer nil)

(defvar power-mode--shake-frame nil)
(defvar power-mode--shake-initial-fringe nil)
(defvar power-mode--shake-parity 0)
(defvar power-mode--shake-timer nil)
(defvar power-mode--shake-timeout-timer nil)

(defun power-mode--streak-timeout ()
  "Streak timeout function."
  (setq power-mode--streak 0
        power-mode--streak-timeout-timer nil))

(defun power-mode--shake ()
  "Shake effect function to be called at an interval."
  (setq power-mode--shake-parity (% (+ power-mode--shake-parity 1) 2))
  (set-frame-parameter
   power-mode--shake-frame 'left-fringe
   (+ power-mode--shake-initial-fringe (* power-mode--shake-parity 4))))

(defun power-mode--shake-timeout ()
  "Shake effect timeout function."
  (cancel-timer power-mode--shake-timer)
  (setq power-mode--shake-timer nil
        power-mode--shake-timeout-timer nil)
  (when (not (eq power-mode--shake-parity 0))
    (set-frame-parameter power-mode--shake-frame 'left-fringe
                         power-mode--shake-initial-fringe)))

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
  (when (and (not power-mode--shake-timer)
             power-mode-streak-shake-threshold
             (>= power-mode--streak power-mode-streak-shake-threshold))
    (setq power-mode--shake-frame (selected-frame))
    (setq power-mode--shake-initial-fringe (frame-parameter
                                            power-mode--shake-frame
                                            'left-fringe)
          power-mode--shake-parity 1
          power-mode--shake-timer (run-with-timer 0 0.1 #'power-mode--shake)))
  ;; Reset shake effect timeout.
  (when power-mode--shake-timeout-timer
    (cancel-timer power-mode--shake-timeout-timer)
    (setq power-mode--shake-timeout-timer nil))
  (when power-mode--shake-timer
    (setq power-mode--shake-timeout-timer
          (run-with-timer 0.2 nil #'power-mode--shake-timeout))))

;;;###autoload
(define-minor-mode power-mode
  "Imbue Emacs with power."
  :init-value nil
  :lighter " power"
  :global t
  :group 'power-mode
  (if power-mode
      (add-hook 'post-self-insert-hook #'power-mode--post-self-insert-hook)
    (progn
      (remove-hook 'post-self-insert-hook #'power-mode--post-self-insert-hook)
      ;; Force pending timeouts to activate.
      (when power-mode--shake-timeout-timer
        (power-mode--shake-timeout))
      (when power-mode--streak-timeout-timer
        (power-mode--streak-timeout)))))

(provide 'power-mode)

;;; power-mode.el ends here
