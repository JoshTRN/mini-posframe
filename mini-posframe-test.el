;;; mini-posframe-test.el --- Tests for mini-posframe -*- lexical-binding: t; -*-

(require 'ert)
(require 'mini-posframe)

(ert-deftest mini-posframe-display-line-count-counts-newlines ()
  (should (= 3 (mini-posframe--display-line-count "one\ntwo\nthree" 60)))
  (should (= 3 (mini-posframe--display-line-count "one\ntwo\n" 60))))

(ert-deftest mini-posframe-display-line-count-counts-wrapping ()
  (should (= 3 (mini-posframe--display-line-count "123456789" 4)))
  (should (= 3 (mini-posframe--display-line-count "12345\n6" 4))))

(ert-deftest mini-posframe-display-line-count-prefers-word-boundaries ()
  ;; Character-count wrapping would use two lines here.  Word wrapping
  ;; keeps each four-character word intact and therefore needs three.
  (should (= 3 (mini-posframe--display-line-count "1234 1234 1234" 8))))

(ert-deftest mini-posframe-prepare-buffer-enables-visual-line-wrapping ()
  (with-temp-buffer
    (setq-local truncate-lines t
                word-wrap nil)
    (mini-posframe--prepare-buffer)
    (should-not truncate-lines)
    (should word-wrap)))

(ert-deftest mini-posframe-refresh-lets-posframe-fit-rendered-height ()
  (with-temp-buffer
    (insert "a line long enough to wrap in a scaled posframe")
    (goto-char (point-max))
    (let ((mini-posframe-width 20)
          (mini-posframe-height 1)
          (mini-posframe-max-height 10)
          show-arguments)
      (cl-letf (((symbol-function 'mini-posframe-active-p) (lambda () t))
                ((symbol-function 'active-minibuffer-window)
                 (lambda () (selected-window)))
                ((symbol-function 'posframe-show)
                 (lambda (&rest arguments)
                   (setq show-arguments arguments))))
        (mini-posframe-refresh))
      (should show-arguments)
      (should (= 20 (plist-get (cdr show-arguments) :width)))
      (should (= 1 (plist-get (cdr show-arguments) :min-height)))
      (should (= 10 (plist-get (cdr show-arguments) :max-height)))
      (should-not (plist-member (cdr show-arguments) :height))
      (should (plist-member (cdr show-arguments) :lines-truncate))
      (should-not (plist-get (cdr show-arguments) :lines-truncate)))))

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
