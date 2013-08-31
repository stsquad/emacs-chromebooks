emacs-chromebooks
=================

Some integrations for running a crouton powered emacs session on your
Chromebook.

pre-requisites
--------------

You will need:
  * A Chromebook running ChromeOS
  * A crouton install [1] with emacs in it
  * The latest Edit with Emacs [2] from git
  * This package

using
-----

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

host-x11 emacs --daemon

Then when no edit area is in focus in my Chrome browser I can click
the emacs link and up pops a full frame Emacs. Normal edit-with-emacs
functionality works as well. Dismiss the frame with:

C-c x

what works
----------

While the foreground frame is up you have no real access to the rest
of the GUI as the native WM doesn't understand anything but Chrome.
This is why full-screen mode is recommended. The extension will also
poke the power daemon to ensure it doesn't go to sleep while Emacs has
it's attention (as resume gets confused if Emacs is hogging the
frame).

to do
-----

- [ ] Disable touchpad
- [ ] Handle special keys better
- [ ] Remap Search to Ctrl (or at least offer the option)

[1]: https://github.com/dnschneid/crouton
[2]: https://github.com/stsquad/emacs_chrome






