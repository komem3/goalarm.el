# goalarm.el

package of manage the goalarm process.

## Dependencies

https://github.com/komem3/goalarm

## Functions

- goalarm-start:
  Start goalarm.  ARGS are time of alarm.
When you want to loop, write 'l' at the args end.
- goalarm-routine:
  Start goalarm routine.
Read a routine of 'goalarm-routine-list' from minibuffer and execute it.
Read function is 'goalarm-read-routine-function'
- goalarm-stop:
  Resume goalarm.
- goalarm-pause:
  Pause goalarm.
- goalarm-restart:
  Restart goalarm.


## Settings

### Basic

```elisp
(require 'goalarm)

(setq goalarm-sound-file "/home/sample/Music/bell.mp3")
(add-to-list 'goalarm-routine-list '("break-time" .
                                     (:value
                                        (((name . "break-time") (range . 55)))
                                        :loop nil)))
```

### Read Routine by ivy

```elisp
(defun custom-goalarm-ivy-read-string (routine-list)
    "Read string by ivy-read."
    (ivy-read "Which routine does start? " (mapcar 'car routine-list)))
(setq goalarm-read-routine-function 'custom-goalarm-ivy-read-string)
```

### Modeline
```elisp
(defun custom-modeline-goalarm ()
    (let ((left-time (goalarm-get-time)))
      (if left-time
          (format "%s(%s)" left-time (goalarm-get-alarm-name)))))
(add-to-list 'global-mode-string '(t (:eval (custom-modeline-goalarm))))
```

## Author

komem3
