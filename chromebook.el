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

(require 'dbus)

(defvar crmbk-host-dbus-socket
  "unix:path=/var/host/dbus/system_bus_socket"
  "Location of DBUS_SYSTEM_BUS_ADDRESS for host")

(defvar crmbk-powerd-delay-id
  'nil
  "ID assigned to Emacs after a RegisterSuspendDelayRequest")

(defvar crmbk-powerd-listener
  'nil
  "ID of DBUS listener listening for SuspendImminent messages")

(defvar crmbk-current-frame
  'nil
  "Current X11 frame if running")

(defvar crmbk-previous-frame-config
  'nil
  "Frame configuration last time we exited Chromebook mode")

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
    (run-hooks 'crmbk-frame-mode-close-hook))
   (t
    (crmbk-register-with-powerd-dbus))))

;; detection code
(defun crmbk-running-in-host-x11-p ()
  "Return 't if this instance of Emacs is running in crouton under the
host-x11 script"
  (and (getenv "DISPLAY")
       (string-prefix-p "/usr/local/bin/host-x11"
                        (shell-command-to-string "which host-x11"))))

;; powerd related code
;
; Rather than use the crouton scripts we hook directly into the
; power managers DBUS interface. This way we can register Emacs
; as something that needs to prepare when a suspend is imminent.
;
; http://dev.chromium.org/chromium-os/packages/power_manager
; https://chromium.googlesource.com/chromiumos/platform/system_api/+/master/dbus/power_manager/suspend.proto
; 

(defun crmbk-host-dbus-accesible-p ()
  "Can we access the host DBUS?"
  (and (file-exists-p crmbk-host-dbus-socket)
       (require 'dbus 'nil 't)))

(defun crmbk-register-suspend-delay-handler (msg)
  "Async handler for RegisterSuspendDelayRequest"
  (message "crmbk-register-suspend-delay-handler")
  (when crmbk-powerd-delay-id
    (warn "crmbk-powerd-delay-id already set: %d, new msg %s"
          crmbk-powerd-delay-id msg))
  (setq crmbk-powerd-delay-id msg))

;; TODO: finish this off
(defun crmbk-suspend-imminent-hander (msg)
  "Handler for SuspendImminent messages"
  (when crmbk-current-frame
    (message "crmbk-suspend-imminent-hander: deleting frame")
    (delete-frame crmbk-current-frame))
                                        ; once we have removed any live frame we can signal we are done to
                                        ; ChromeOS
  )

;; TODO: get this working properly
(defun crmbk-setup-delay-request ()
  ""
  (dbus-call-method-asynchronously
   :system
   "org.chromium.PowerManager"    ; service
   "/org/chromium/PowerManager"   ; path
   "org.chromium.PowerManager"    ; interface
   "RegisterSuspendDelayRequest"  ; method
   'crmbk-register-suspend-delay-handler
   (list (lsh 1 3) (logior 5 #x80)))
  (when (not crmbk-powerd-listener)
    (setq crmbk-powerd-listener
          (dbus-register-signal
           :system
           "org.chromium.PowerManager"                 ; service
           "/org/chromium/PowerManager"   ; path
           "org.chromium.PowerManager"    ; interface
           "SuspendImminent"
           'crmbk-suspend-imminent-hander))))


(defun crmbk-notify-powerd-user-activity ()
  "Send a notification to powerd that there is user activity. This
is triggered on the post-command-hook"
  (dbus-call-method-asynchronously
            :system
            "org.chromium.PowerManager"                 ; service
            "/org/chromium/PowerManager"   ; path
            "org.chromium.PowerManager"    ; interface
            "HandleUserActivity"           ; method
            'nil))

(defun crmbk-remove-powerd-hooks ()
  "Clean-up any hooks into powerd and it's dbus interface"
  (remove-hook 'post-command-hook 'crmbk-notify-powerd-user-activity))

(defun crmbk-register-with-powerd-dbus ()
  "Request a suspend ID and register a signal handler to service
dbus power notifications"
  ; add hooks to poke powerd and clean-up when done
  (add-hook 'post-command-hook 'crmbk-notify-powerd-user-activity)
  (add-hook 'crmbk-frame-mode-close-hook 'crmbk-remove-powerd-hooks)
  ;(crmbk-setup-delay-request)
  )

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
    (setq crmbk-current-frame frame)
    (when crmbk-previous-frame-config
      (window-state-put crmbk-previous-frame-config
                        (frame-root-window frame)))
    (crmbk-frame-mode t)))

; We need to know if this frame is the one that
; the new-frame handler set up for
(defun crmbk-delete-frame-handler (frame)
  "Clean-up timers and the like"
  (when (frame-parameter frame 'display)
    (setq crmbk-previous-frame-config (window-state-get))
    (when (eq frame crmbk-current-frame)
      (setq crmbk-current-frame 'nil))
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
