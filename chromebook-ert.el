;;
;; Tests for Chromebook.el
;;

(require 'ert)


(ert-deftest chromebook-compiles ()
  "Tests that chromebook.el compiles cleanly."
  (let ((byte-compile-error-on-warn 't))
    (should (byte-compile-file "chromebook.el"))))
