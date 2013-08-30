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
;;   - advise C-c C-x to (delete-frame)
;;   - periodically poke the power daemon to prevent sleeping
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
  (start-process-shell-command "powerd" 'nil crmbk-crouton-powerd))

(defun crmbk-start-powerd-timer ()
  "Start a periodic timer, poking the powerd"
  (interactive)
  (when (not crmbk-powerd-timer)
    (setq crmbk-powerd-timer (run-with-timer 10 10 'crmbk-poke-powerd))))

(defun crmbk-clear-powerd-timer ()
  "Clear the periodic timer"
  (interactive)
  (when crmbk-powerd-timer
    (cancel-timer crmbk-powerd-timer)
    (setq crmbk-powerd-timer 'nil)))
;;
;; Frame handling code
;;
;; Function to handle all new frame creation
(defun crmbk-new-frame-handler (frame)
  "Do any appropriate set-up on new frame creation.
This is intended to be called during after-make-frame-functions"
  (message "crmbk-new-frame-handler")
  (set-frame-parameter frame 'fullscreen 'fullboth)
  (crmbk-start-powerd-timer)
  ; this really should be frame local...
  (global-set-key (kbd "C-x c") 'delete-frame))

; We need to know if this frame is the one that
; the new-frame handler set up for
(defun crmbk-delete-frame-handler (frame)
  "Clean-up timers and the like"
  (crmbk-clear-powerd-timer)
  ; we should really restore what it was!
  (global-set-key (kbd "C-x c") 'save-buffers-kill-terminal))

;;
;; Initialise chromebook mode bits.
;;
(when (crmbk-running-in-host-x11-p)
  (add-hook 'after-make-frame-functions 'crmbk-new-frame-handler)
  (add-hook 'delete-frame-functions 'crmbk-delete-frame-handler))

(provide 'chromebook)

;;; chromebook.el ends here
