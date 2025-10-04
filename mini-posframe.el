;;; mini-posframe.el --- Minibuffer mirrored in posframe -*- lexical-binding: t; -*-

;; Author: JoshTRN <your-email@example.com>
;; URL: https://github.com/JoshTRN/mini-posframe
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (posframe "1.0.0"))
;; Keywords: convenience, minibuffer
;; License: GPL-3+

;;; Commentary:

;; mini-posframe mirrors the minibuffer in a child frame using posframe.
;; It is designed for Spacemacs + Evil, but can be customized to work in
;; other setups. Provides customizable position handlers, font scaling,
;; border styling, and optional integration with helm-posframe.
;;
;; Usage:
;;   (require 'mini-posframe)
;;   (mini-posframe-mode 1)

;;; Code:

(require 'posframe)
(require 'subr-x) ;; string-empty-p, when-let, etc.

;; ───────────────────────────────────────────────────────────────────
;; Customization group
;; ───────────────────────────────────────────────────────────────────

(defgroup mini-posframe nil
  "Display minibuffer content inside a posframe."
  :group 'convenience
  :prefix "mini-posframe-")

;; ───────────────────────────────────────────────────────────────────
;; Faces
;; ───────────────────────────────────────────────────────────────────

(defface mini-posframe-face
  '((t (:inherit default :height 1.2)))
  "Face for mini-posframe text."
  :group 'mini-posframe)

(defface mini-posframe-border-face
  '((t (:inherit default :background "#4c78cc")))
  "Face for mini-posframe border."
  :group 'mini-posframe)

;; ───────────────────────────────────────────────────────────────────
;; User options
;; ───────────────────────────────────────────────────────────────────

(defcustom mini-posframe-width 60
  "The width of mini-posframe (in characters)."
  :group 'mini-posframe
  :type 'integer)

(defcustom mini-posframe-height 1
  "The height of mini-posframe (in lines).
When set to 1, the posframe’s height is determined dynamically
based on its content."
  :group 'mini-posframe
  :type 'integer)

(defcustom mini-posframe-font-size 1.4
  "Relative font size factor for mini-posframe.
The default font family is taken from the current frame’s `default' face,
and the height is scaled by this factor."
  :group 'mini-posframe
  :type 'number)

(defcustom mini-posframe-border-width 3
  "Width of the mini-posframe border, in pixels.
This value is passed through to `posframe-show' as
`:internal-border-width'."
  :group 'mini-posframe
  :type 'integer)

(defcustom mini-posframe-parameters
  '((undecorated . nil))
  "Frame parameters passed to `posframe-show' for mini-posframe.
Customize this alist to control borders, fringes, decorations, etc."
  :group 'mini-posframe
  :type 'alist)

(defcustom mini-posframe-vertical-fringe 0.10
  "Fraction of the parent frame height to offset vertically.
E.g. 0.10 means 10% from top or bottom."
  :group 'mini-posframe
  :type 'number)

(defcustom mini-posframe-horizontal-fringe 0.05
  "Fraction of the parent frame width to offset horizontally.
E.g. 0.05 means 5% from left or right."
  :group 'mini-posframe
  :type 'number)

(defcustom mini-posframe-position 'top-center
  "Where to display the mini-posframe.
One of: `top-center', `center', `bottom-center',
`top-left', `left', `bottom-left', `top-right', `right', `bottom-right'."
  :group 'mini-posframe
  :type '(choice (const :tag "Top center" top-center)
                 (const :tag "Center" center)
                 (const :tag "Bottom center" bottom-center)
                 (const :tag "Top left" top-left)
                 (const :tag "Left" left)
                 (const :tag "Bottom left" bottom-left)
                 (const :tag "Top right" top-right)
                 (const :tag "Right" right)
                 (const :tag "Bottom right" bottom-right)))

(defcustom mini-posframe-background-face nil
  "Face used for mini-posframe background.
When nil, use `solaire-default-face' if `solaire-mode' is active and
the face exists; otherwise fall back to `default'."
  :group 'mini-posframe
  :type '(choice (const :tag "Auto (solaire/default)" nil) face))

;; ───────────────────────────────────────────────────────────────────
;; Internal vars
;; ───────────────────────────────────────────────────────────────────

(defvar mini-posframe-buffer " *mini-posframe*"
  "Buffer used for mini-posframe display.")

(defvar mini-posframe-last-message nil
  "Last status message to display in the mini-posframe (e.g., Evil search).")

;; ───────────────────────────────────────────────────────────────────
;; Position handlers
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe-poshandler-top-center (info)
  "Horizontally centered, vertical fringe from top."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (x (/ (- pfw fw) 2))
         (y (max 0 (floor (* pfh mini-posframe-vertical-fringe)))))
    (cons x y)))

(defun mini-posframe-poshandler-center (info)
  "Center both horizontally and vertically."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (fh  (plist-get info :posframe-height))
         (x (/ (- pfw fw) 2))
         (y (/ (- pfh fh) 2)))
    (cons x y)))

(defun mini-posframe-poshandler-bottom-center (info)
  "Horizontally centered, vertical fringe from bottom."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (fh  (plist-get info :posframe-height))
         (x (/ (- pfw fw) 2))
         (y (max 0 (- (floor (* pfh (- 1 mini-posframe-vertical-fringe))) fh))))
    (cons x y)))

(defun mini-posframe-poshandler-top-left (info)
  "Offset from top and left fringes."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (x (floor (* pfw mini-posframe-horizontal-fringe)))
         (y (floor (* pfh mini-posframe-vertical-fringe))))
    (cons x y)))

(defun mini-posframe-poshandler-top-right (info)
  "Offset from top and right fringes."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (x (max 0 (- (floor (* pfw (- 1 mini-posframe-horizontal-fringe))) fw)))
         (y (floor (* pfh mini-posframe-vertical-fringe))))
    (cons x y)))

(defun mini-posframe-poshandler-bottom-left (info)
  "Offset from bottom and left fringes."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fh  (plist-get info :posframe-height))
         (x (floor (* pfw mini-posframe-horizontal-fringe)))
         (y (max 0 (- (floor (* pfh (- 1 mini-posframe-vertical-fringe))) fh))))
    (cons x y)))

(defun mini-posframe-poshandler-bottom-right (info)
  "Offset from bottom and right fringes."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (fh  (plist-get info :posframe-height))
         (x (max 0 (- (floor (* pfw (- 1 mini-posframe-horizontal-fringe))) fw)))
         (y (max 0 (- (floor (* pfh (- 1 mini-posframe-vertical-fringe))) fh))))
    (cons x y)))

(defun mini-posframe-poshandler-left (info)
  "Vertically centered, offset from left fringe."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fh  (plist-get info :posframe-height))
         (x (floor (* pfw mini-posframe-horizontal-fringe)))
         (y (/ (- pfh fh) 2)))
    (cons x y)))

(defun mini-posframe-poshandler-right (info)
  "Vertically centered, offset from right fringe."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (fh  (plist-get info :posframe-height))
         (x (max 0 (- (floor (* pfw (- 1 mini-posframe-horizontal-fringe))) fw)))
         (y (/ (- pfh fh) 2)))
    (cons x y)))

(defun mini-posframe-poshandler-dispatch (info)
  "Dispatch to the appropriate poshandler based on `mini-posframe-position'."
  (pcase mini-posframe-position
    ('top-center     (mini-posframe-poshandler-top-center info))
    ('center         (mini-posframe-poshandler-center info))
    ('bottom-center  (mini-posframe-poshandler-bottom-center info))
    ('top-left       (mini-posframe-poshandler-top-left info))
    ('left           (mini-posframe-poshandler-left info))
    ('bottom-left    (mini-posframe-poshandler-bottom-left info))
    ('top-right      (mini-posframe-poshandler-top-right info))
    ('right          (mini-posframe-poshandler-right info))
    ('bottom-right   (mini-posframe-poshandler-bottom-right info))
    (_               (mini-posframe-poshandler-bottom-center info))))

(defcustom mini-posframe-poshandler #'mini-posframe-poshandler-dispatch
  "The poshandler function used by mini-posframe."
  :group 'mini-posframe
  :type 'function)

;; ───────────────────────────────────────────────────────────────────
;; Evil search message capture (optional)
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe-capture-evil-search-message (orig-fn fmt &rest args)
  "Advice around `message' to capture search status like \"[Search failed]\"."
  (if (null fmt)
      (apply orig-fn fmt args)
    (let ((msg (apply #'format fmt args)))
      (when (and msg (string-match-p "\\[Search" msg))
        (setq mini-posframe-last-message msg))
      (apply orig-fn fmt args))))

(advice-add 'message :around #'mini-posframe-capture-evil-search-message)

;; ───────────────────────────────────────────────────────────────────
;; Core logic
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe--resolve-background-face ()
  "Return the face to use for mini-posframe background."
  (or mini-posframe-background-face
      (if (and (boundp 'solaire-mode)
               solaire-mode
               (facep 'solaire-default-face))
          'solaire-default-face
        'default)))

(defun mini-posframe-active-p ()
  "Return non-nil if mini-posframe should run."
  (and (active-minibuffer-window)
       (window-live-p (active-minibuffer-window))
       (minibufferp (window-buffer (active-minibuffer-window)))
       (not (bound-and-true-p helm-alive-p))))

(defun mini-posframe-refresh ()
  "Safely mirror minibuffer in a posframe, with fake cursor tracking."
  (condition-case _
      (when (mini-posframe-active-p)
        (let* ((win (active-minibuffer-window))
               (buf (and (window-live-p win) (window-buffer win))))
          (when buf
            (with-current-buffer buf
              (let* ((raw (ignore-errors
                            (buffer-substring (point-min) (point-max))))
                     (status-msg (when mini-posframe-last-message
                                   (propertize mini-posframe-last-message 'face 'error)))
                     (max-width mini-posframe-width))
                (when (and raw (stringp raw))
                  (let* ((text (if (> (length raw) max-width)
                                   (substring raw (- (length raw) max-width))
                                 raw))
                         ;; fake cursor insertion
                         (cursor-idx (max 0 (min (length text)
                                                 (- (point) (point-min)))))
                         (before (if (> cursor-idx 0) (substring text 0 cursor-idx) ""))
                         (cursor-char (if (< cursor-idx (length text))
                                          (substring text cursor-idx (1+ cursor-idx))
                                        " "))
                         (after (if (< cursor-idx (length text))
                                    (substring text (1+ cursor-idx))
                                  ""))
                         (cursor (propertize cursor-char 'face '(:inverse-video t)))
                         (with-cursor (concat before cursor after))
                         (full (if status-msg (concat with-cursor "  " status-msg) with-cursor))
                         (bg (face-background (mini-posframe--resolve-background-face) nil t)))
                    (save-window-excursion
                      (posframe-show mini-posframe-buffer
                                     :string (if (string-empty-p full) " " full)
                                     :poshandler mini-posframe-poshandler
                                     :width mini-posframe-width
                                     :height mini-posframe-height
                                     :face 'mini-posframe-face
                                     :foreground-color (face-attribute 'mini-posframe-face :foreground nil t)
                                     :background-color bg
                                     :internal-border-width mini-posframe-border-width
                                     :internal-border-color (face-attribute 'mini-posframe-border-face :background nil t)
                                     :font (format "%s-%d"
                                                   (face-attribute 'default :family)
                                                   (round (* mini-posframe-font-size
                                                             (/ (face-attribute 'default :height)
                                                                10.0))))
                                     :override-parameters mini-posframe-parameters)))))))))
    (error
     (mini-posframe-hide))))

(defun mini-posframe-hide ()
  "Hide mini-posframe and reset state."
  (ignore-errors (posframe-delete mini-posframe-buffer))
  (setq mini-posframe-last-message nil))

(defun mini-posframe-hide-minibuffer-maybe ()
  "Make real minibuffer invisible (input still works)."
  (when (mini-posframe-active-p)
    (let* ((bg (face-background (mini-posframe--resolve-background-face) nil t))
           (ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face `(:foreground ,bg :background ,bg))
      (setq-local cursor-type nil))))

;; ───────────────────────────────────────────────────────────────────
;; Session management & mode
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe-session-start ()
  "Enable posframe refresh only for this minibuffer session."
  (when mini-posframe-mode
    ;; Hooks are buffer-local to minibuffer
    (add-hook 'post-command-hook #'mini-posframe-refresh nil t)
    (add-hook 'post-command-hook #'mini-posframe-hide-minibuffer-maybe nil t)))

(defun mini-posframe-session-end ()
  "Disable posframe refresh after minibuffer session ends."
  (mini-posframe-hide)
  (remove-hook 'post-command-hook #'mini-posframe-refresh t)
  (remove-hook 'post-command-hook #'mini-posframe-hide-minibuffer-maybe t))

;;;###autoload
(define-minor-mode mini-posframe-mode
  "Global minor mode to show minibuffer input in a posframe."
  :global t
  (if mini-posframe-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'mini-posframe-session-start)
        (add-hook 'minibuffer-exit-hook  #'mini-posframe-session-end)
        (add-hook 'post-command-hook     #'mini-posframe-auto-cleanup)
        (message "mini-posframe: enabled"))
    (remove-hook 'minibuffer-setup-hook #'mini-posframe-session-start)
    (remove-hook 'minibuffer-exit-hook  #'mini-posframe-session-end)
    (remove-hook 'post-command-hook     #'mini-posframe-auto-cleanup)
    (mini-posframe-hide)
    (message "mini-posframe: disabled")))

;; ───────────────────────────────────────────────────────────────────
;; Reset & cleanup
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe-reset (&optional reenable)
  "Forcefully reset mini-posframe mode.
Disables the mode, removes hooks, deletes posframe, and resets state.
With optional REENABLE non-nil, turn the mode back on afterwards."
  (interactive "P")
  (mini-posframe-mode -1)
  (mini-posframe-hide)
  (message "[mini-posframe] Hard reset complete")
  (when reenable
    (mini-posframe-mode 1)
    (message "[mini-posframe] Mode re-enabled after reset")))

(defun mini-posframe-auto-cleanup ()
  "Force cleanup if minibuffer hooks are active but minibuffer is gone.
Also remove this function from `post-command-hook' until next session."
  (unless (active-minibuffer-window)
    (mini-posframe-hide)
    ;; Remove any minibuffer-local hooks that might have leaked
    (remove-hook 'post-command-hook #'mini-posframe-refresh t)
    (remove-hook 'post-command-hook #'mini-posframe-hide-minibuffer-maybe t)
    ;; Remove this global watcher until next minibuffer session
    (remove-hook 'post-command-hook #'mini-posframe-auto-cleanup)
    (message "[mini-posframe] Auto-cleaned and disabled global watcher")))

;; ───────────────────────────────────────────────────────────────────
;; Debug helper
;; ───────────────────────────────────────────────────────────────────

(defun mini-posframe-debug-exit ()
  "Log when the minibuffer exit hook fires (for debugging)."
  (message "[mini-posframe DEBUG] minibuffer-exit-hook fired in %S"
           (current-buffer)))
(add-hook 'minibuffer-exit-hook #'mini-posframe-debug-exit)

;; ───────────────────────────────────────────────────────────────────
;; Helm-posframe compatibility (optional)
;; ───────────────────────────────────────────────────────────────────

;; Helm-posframe compatibility (optional)
(when (featurep 'helm-posframe)
  (defun mini-posframe-helm-ff-delete-char-backward-advice (orig-fn &rest args)
    "Around advice for `helm-ff-delete-char-backward-with-subkeys'."
    (let ((cmd (apply orig-fn args)))
      (when (commandp cmd)
        (command-execute cmd))
      cmd))

  (defun mini-posframe-helm-setup-advice ()
    "Enable helm-posframe-specific advice."
    (advice-add 'helm-ff-delete-char-backward-with-subkeys
                :around #'mini-posframe-helm-ff-delete-char-backward-advice))

  (defun mini-posframe-helm-remove-advice ()
    "Remove helm-posframe-specific advice."
    (advice-remove 'helm-ff-delete-char-backward-with-subkeys
                   #'mini-posframe-helm-ff-delete-char-backward-advice))

  ;; Attach to helm-posframe lifecycle
  (advice-add 'helm-posframe-enable :after #'mini-posframe-helm-setup-advice)
  (advice-add 'helm-posframe-disable :after #'mini-posframe-helm-remove-advice))

(provide 'mini-posframe)
;;; mini-posframe.el ends here
