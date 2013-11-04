Emacs for Chromebooks
=====================
[![Build Status](https://travis-ci.org/stsquad/emacs-chromebooks.png?branch=master)](https://travis-ci.org/stsquad/emacs-chromebooks)

Chromebooks are web focused laptops that run a very cut down version
of Linux. If you enable Developer mode you can install a set of
scripts called Crouton which allow you to have a fuller Linux
environment in a chroot. Normally people run a secondary X session on
a different console in a sort of parallel setup. However it is
possible for binaries to access the main ChromeOS X session although
with some limitations.

This package is intended to allow running Emacs from such a chroot as
a full-screen window in the main ChromeOS environment. You then bring
up your emacs session by clicking the Emacs logo inside your browser.

Pre-requisites
--------------

You will need:
  * A Chromebook running ChromeOS
  * A [crouton][1] install with emacs in it
  * The latest [Edit with Emacs][2] from git (server and chrome extension)
  * This package

using
-----

In the Chrome extension configuration page enable:

    Allow clicking on Emacs icon to bring Emacs to foreground when no text area in focus

I have the following in my .emacs

```elisp
(when (and (require 'chromebook "chromebook" 't)
           (crmbk-running-in-host-x11-p))
  (set-face-attribute 'default nil :height 250)
  (when (boundp 'edit-server-new-frame-alist)
    (setq edit-server-new-frame-alist '((name . "Edit Server Frame")
					(fullscreen . 'fullboth)))))
```

I start Emacs in my crouton chroot with a command like:

    host-dbus host-x11 emacs --daemon

Then when no edit area is in focus in my Chrome browser I can click
the emacs link and up pops a full frame Emacs. Normal edit-with-emacs
functionality works as well. Dismiss the frame with:

    C-x C-c

There are a couple of hooks that can be used to disable the touchpad
and remap the Search key to an additional control. These both require
the xmodmap utility to be installed:

```
  (add-hook 'crmbk-frame-mode-hook 'crmbk-remap-search)
  (add-hook 'crmbk-frame-mode-hook 'crmbk-disable-touchpad)
```

what works
----------

While the foreground frame is up you have no real access to the rest
of the GUI as the native WM doesn't understand anything but Chrome.
This is why full-screen mode is recommended. The extension will also
poke the power daemon to ensure it doesn't go to sleep while Emacs has
it's attention (as resume gets confused if Emacs is hogging the
frame).

To Do
-----

About the only thing left to do is cleanly handle when ChromeOS
suspends. At the moment if the Emacs window is up when suspend occurs
it may not show-up when the system wakes up. One option is to hook
into the dbus notifications for an imminent suspend and delete the
frame when it occurs. The system already pokes the dbus powerd
interface to indicate activity but hooking into the listener is a
little more involved.

[1]: https://github.com/dnschneid/crouton "Crouton chroot for ChromeOS"
[2]: https://github.com/stsquad/emacs_chrome "Edit with Emacs Chrome Extension"






