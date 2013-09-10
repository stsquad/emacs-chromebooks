;;; chromebook.el --- Emacs Chromebook support.

;; Copyright (C) 2013  Alex Bennée

;; Author: Alex Bennée <alex@bennee.com>
;; Maintainer: Alex Bennée <alex@bennee.com>
;; Version: 0.1
;; Homepage: https://github.com/stsquad/emacs-chromebook

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension is designed to support running emacs running under
;; the host-x11 script of crouton. Crouton is a set of scripts that
;; allow running of a conventional Linux distribution under a chroot
;; environment under ChromeOS. The host-x11 script allows X11 programs
;; to use ChromeOS's X server. However this has some limitations as
;; the native window manager does not normally deal with windows other
;; than that of Google Chrome.
;;
;; To support this mode of running we:
;;
;;   - configure Emacs to run in full-frame mode
;;   - periodically poke the power daemon to prevent sleeping
;;
;; Optionally when we enter this mode we can:
;;   - disable the touch pad to prevent stray touches
;;   - remap the search key to an additional control
;;
;; This is done by adding the appropriate hooks:
;;   (add-hook 'crmbk-frame-mode-hook 'crmbk-remap-search)
;;   (add-hook 'crmbk-frame-mode-hook 'crmbk-disable-touchpad)
;;

;;; Code:

;; uncomment to debug
;; (setq debug-on-error t)
;; (setq edebug-all-defs t)

(defvar crmbk-powerd-timer
  'nil
  "Timer id of periodic timer task")

(defvar crmbk-crouton-powerd
  "/usr/local/bin/croutonpowerd -p"
  "Command to poke the powerd daemon to prevent sleep")

;;; Mode magic
;;
;; We want to re-map a bunch of Chromebook keys

(defvar crmbk-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-c") 'delete-frame)
    map)
  "Keymap for minor mode `crmbk-frame-mode'.")

(defvar crmbk-frame-mode-close-hook
  'nil
  "Hook to run when the mode is closed")

(define-minor-mode crmbk-frame-mode
  "Minor mode enabled on buffers when a frame is opened in
Chrome OS's Ash window manager. Call with a negative arg to
shutdown the mode."
  :lighter " Crmbk"
  :init-value nil
  :global t
  :keymap crmbk-frame-mode-map
  (cond
   ((and
     (numberp arg)
     (< arg 0))
     ; clear down mode
    (crmbk-clear-powerd-timer)
    (run-hooks 'crmbk-frame-mode-close-hook))
   (t
    (crmbk-start-powerd-timer))))

;; detection code
(defun crmbk-running-in-host-x11-p ()
  "Return 't if this instance of Emacs is running in crouton under the
host-x11 script"
  (and (getenv "DISPLAY")
       (string-prefix-p "/usr/local/bin/host-x11"
                        (shell-command-to-string "which host-x11"))))

;; powerd related code
(defun crmbk-poke-powerd ()
  "Poke the crouton powerd daemon to prevent sleep"
  (start-process "powerd" 'nil crmbk-crouton-powerd))

(defun crmbk-start-powerd-timer ()
  "Start a periodic timer, poking the powerd"
  (when (not crmbk-powerd-timer)
    (setq crmbk-powerd-timer (run-with-timer 10 10 'crmbk-poke-powerd))))

(defun crmbk-clear-powerd-timer ()
  "Clear the periodic timer"
  (when crmbk-powerd-timer
    (cancel-timer crmbk-powerd-timer)
    (setq crmbk-powerd-timer 'nil)))

;; keyboard re-mapping
;
; While under host-x11 we can call xmodmap to modify the keymaps. I
; use this to re-map the search key to an alternate control while in
; the minor mode.
(defun crmbk-remap-search ()
  "Remap the search key to control"
  (start-process "xmodmap" 'nil 
   "xmodmap" "-e" "remove mod4 = Super_L" "-e" "add control = Super_L")
  (add-hook 'crmbk-frame-mode-close-hook 'crmbk-reset-search))

(defun crmbk-reset-search ()
  "Reset the search key to it's previous setting"
  (interactive)
  (start-process "xmodmap" 'nil
   "xmodmap" "-e" "remove control = Super_L" "-e" "add mod4 = Super_L"))

;; TouchPad handling
;
; We can use xinput to control the state of the touchpad
;

(defvar crmbk-touchpad-id
  'nil
  "The xinput id of the touchpad for later tweaks")

(defun crmbk--find-touchpad ()
  "Find the X input ID of the touchpad"
  (setq crmbk-touchpad-id
        (with-temp-buffer
          (shell-command "xinput list" (current-buffer))
          (goto-char (point-min))
          (re-search-forward "[Tt]ouchpad\\s-+id=\\([0-9]+\\)")
          (match-string 1))))

(defun crmbk-reenable-touchpad ()
  "Re-enable the touchpad"
  (when (stringp crmbk-touchpad-id)
    (start-process "xinput" 'nil
                   "xinput" "set-prop" crmbk-touchpad-id "Tap Enable" "1")))

(defun crmbk-disable-touchpad ()
  "Disable the TouchPad while in crmbk-mode"
  (when (stringp crmbk-touchpad-id)
    (start-process "xinput" 'nil
                   "xinput" "set-prop" crmbk-touchpad-id "Tap Enable" "0")
    (add-hook 'crmbk-frame-mode-close-hook 'crmbk-reenable-touchpad)))

;;
;; Frame handling code
;;
;; Function to handle all new frame creation
(defun crmbk-new-frame-handler (frame)
  "Do any appropriate set-up on new frame creation.
This is intended to be called during after-make-frame-functions"
  (when (frame-parameter frame 'display)
    (set-frame-parameter frame 'fullscreen 'fullboth)
    (crmbk-frame-mode t)))

; We need to know if this frame is the one that
; the new-frame handler set up for
(defun crmbk-delete-frame-handler (frame)
  "Clean-up timers and the like"
  (when (frame-parameter frame 'display)
    (crmbk-frame-mode -1)))

;;
;; Initialise chromebook mode bits.
;;
(when (crmbk-running-in-host-x11-p)
  (crmbk--find-touchpad)
  (add-hook 'after-make-frame-functions 'crmbk-new-frame-handler)
  (add-hook 'delete-frame-functions 'crmbk-delete-frame-handler))

(provide 'chromebook)

;;; chromebook.el ends here
