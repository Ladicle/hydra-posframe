;;; hydra-posframe.el --- Display hydra diagnostics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
;; URL: https://github.com/Ladicle/hydra-posframe
;; Keywords: convenience, languages, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (hydra "0.14.0") (posframe "1.1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display hydra message at point using a posframe.
;; Check out the README for more information.

;;; Code:
(require 'hydra)
(require 'posframe)

(defgroup hydra-posframe nil
  "Using posframe to show hydra"
  :group 'hydra
  :prefix "hydra-posframe")

(defcustom hydra-posframe-parameters nil
  "The frame parameters used by hydra-posframe."
  :type 'string
  :group 'hydra-posframe)

(defcustom hydra-posframe-border-width 1
  "The border width used by hydra-posframe.
When 0, no border is showed."
  :group 'hydra-posframe
  :type 'number)

(defcustom hydra-posframe-poshandler 'posframe-poshandler-frame-center
  "The poshandler used by hydra-posframe."
  :group 'hydra-posframe
  :type 'function)

(defcustom hydra-posframe-font nil
  "The font used by hydra-posframe.
When nil, Using current frame's font as fallback."
  :group 'hydra-posframe
  :type 'string)

(defface hydra-posframe-face
  '((t :inherit default))
  "The background and foreground color of the posframe.
`background' and `foreground` are used in this face."
  :group 'hydra-posframe)

(defface hydra-posframe-border-face
  '((t (:background "gray50")))
  "The border color of the posframe.
Only `background` is used in this face."
  :group 'hydra-posframe)

(defvar hydra-posframe-buffer " *hydra-posframe-buffer*"
  "The posframe-buffer used by hydra-posframe.")

(defun hydra-posframe-hide-window ()
  "Hide the hydra posframe"
  (posframe-hide hydra-posframe-buffer))

(defun hydra-posframe-show-window (str)
  "Show hydra hints on the posframe"
  (posframe-show
   hydra-posframe-buffer
   :font hydra-posframe-font
   :poshandler hydra-posframe-poshandler
   :foreground-color (face-foreground 'hydra-posframe-face nil t)
   :background-color (face-background 'hydra-posframe-face nil t)
   :border-width hydra-posframe-border-width
   :border-color (face-attribute 'hydra-posframe-border-face :background)
   :string (concat str "\n")
   :override-parameters hydra-posframe-parameters)
  (let ((current-frame
         (buffer-local-value 'posframe--frame
                             (get-buffer hydra-posframe-buffer))))
    (redirect-frame-focus current-frame
                          (frame-parent current-frame))))

;;;###autoload
(define-minor-mode hydra-posframe-mode
  "Display hydra via posframe."
  :init-value nil
  :global t
  :require 'hydra-posframe
  :group 'hydra-posframe
  (let ((hydra-posframe-list (list 'hydra-posframe
                                   #'hydra-posframe-show-window
                                   #'hydra-posframe-hide-window)))
    (if hydra-posframe-mode
        (progn
          (add-to-list 'hydra-hint-display-alist hydra-posframe-list)
          (setq hydra-hint-display-type 'hydra-posframe))
      (progn
        (setq hydra-hint-display-alist
              (delete hydra-posframe-list hydra-hint-display-alist))
        (setq hydra-hint-display-type 'lv)))))

;;;###autoload
(defun hydra-posframe-enable ()
  "Enable hydra-posframe."
  (interactive)
  (hydra-posframe-mode 1)
  (message "hydra-posframe: suggest use `hydra-posframe-mode` instead."))

(provide 'hydra-posframe)
;;; hydra-posframe.el ends here
