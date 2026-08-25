;;; mini-posframe-test.el --- Tests for mini-posframe -*- lexical-binding: t; -*-

(require 'ert)
(require 'mini-posframe)

(ert-deftest mini-posframe-display-line-count-counts-newlines ()
  (should (= 3 (mini-posframe--display-line-count "one\ntwo\nthree" 60)))
  (should (= 3 (mini-posframe--display-line-count "one\ntwo\n" 60))))

(ert-deftest mini-posframe-display-line-count-counts-wrapping ()
  (should (= 3 (mini-posframe--display-line-count "123456789" 4)))
  (should (= 3 (mini-posframe--display-line-count "12345\n6" 4))))

(ert-deftest mini-posframe-disable-session-restores-minibuffer ()
  (with-temp-buffer
    (let ((mini-posframe--original-cursor-type 'box)
          (cursor-type nil)
          (mini-posframe--minibuffer-overlay
           (make-overlay (point-min) (point-max))))
      (cl-letf (((symbol-function 'mini-posframe-hide) #'ignore))
        (mini-posframe--disable-session))
      (should mini-posframe--session-disabled)
      (should-not mini-posframe--minibuffer-overlay)
      (should (eq cursor-type 'box)))))

(provide 'mini-posframe-test)
;;; mini-posframe-test.el ends here
