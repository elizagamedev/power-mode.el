# power-mode.el

At long last, [Power Mode](https://github.com/codeinthedark/awesome-power-mode)
is available for the best text editor.

![](screencast.gif)

(It looks way better than in the gif, trust me.)

## Installation

Currently, this package is not on M?ELPA. In the meantime, please clone and
extend your load path. For example:

```elisp
(use-package power-mode
  :load-path "site-lisp/power-mode.el"
  :init
  (add-hook 'after-init-hook #'power-mode))
```

## Caveats

- Don't use this on EXWM or you'll regret it.
- If running Emacs as a daemon, frames might behave weirdly if shaking is
  enabled and you exit power-mode. It's safe to close them and re-open new ones.
- The shaking windows feature may altogether behave terribly and ruin your Emacs
  session. You can disable this while still retaining particle effects by
  setting `power-mode-streak-shake-threshold` to nil.

## Wow! How does it work?

[Giving me access to child frames was a
mistake.](https://www.youtube.com/watch?v=qPHMSBmdpCs)
