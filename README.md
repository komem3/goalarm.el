# goalarm.el

Tool of manage the [goalarm](https://github.com/komem3/goalarm) process.

By using the alarm server, we don't let emacs manage your time and tasks.

## Dependencies

- https://github.com/komem3/goalarm

```shell
$ go get github.com/komem3/goalarm/cmd/goalarm
```

- https://github.com/hajimehoshi/oto#prerequisite

## Variables
- goalarm-read-routine-function:
  Function of reading routine from minibuffer.
- goalarm-sound-file:
  Sound file of alarm.  File is allowed mp3 or wav.
- goalarm-routine-list:
  List of routine.
- goalarm-start-hook:
  List of functions to be called before goalarm has been started.
- goalarm-exit-hook:
  List of functions to be called before goalarm has been exit.
- goalarm-stop-hook:
  List of functions to be called before goalarm has been stop.
- goalarm-finish-hook:
  List of functions to be called before goalarm has been finish.

## Functions

### Interactive
- goalarm-start:
  Start goalarm.  ARGS are time of alarm.
When you want to loop, write 'l' at the args end.
- goalarm-routine:
  Start goalarm routine.
Read a routine of `goalarm-routine-list` from minibuffer and execute it.
Read function is `goalarm-read-routine-function`.
- goalarm-stop:
  Resume goalarm.
- goalarm-pause-or-resume:
  Pause or resume goalarm.
- goalarm-restart:
  Restart goalarm.

### Convenient functions
- goalarm-get-time:
  Get time from goalarm process.
- goalarm-process-status:
  Get latest goalarm status.
Status is `running`, `pause` or empty string.
- goalarm-get-alarm-name:
  Get running alarm name.

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
