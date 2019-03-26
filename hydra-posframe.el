;;; hydra-posframe.el --- Display hydra diagnostics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
;; URL: https://github.com/Ladicle/hydra-posframe
;; Keywords: convenience, languages, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (hydra "0.14.0") (posframe "0.4.3"))

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

(defun hydra-posframe-keyboard-quit ()
  "Quitting function similar to `keyboard-quit'."
  (interactive)
  (hydra-disable)
  (cancel-timer hydra-timeout-timer)
  (cancel-timer hydra-message-timer)
  (setq hydra-curr-map nil)
  (unless (and hydra--ignore
               (null hydra--work-around-dedicated))
    (posframe-hide hydra-posframe-buffer))
  nil)

(defun hydra-posframe-show-hint (hint caller)
  (let ((verbosity (plist-get (cdr (assoc caller hydra-props-alist))
                              :verbosity)))
    (cond ((eq verbosity 0))
          ((eq verbosity 1)
           (message (eval hint)))
          (t
           (when hydra-is-helpful
	     (posframe-show
              hydra-posframe-buffer
              :poshandler 'posframe-poshandler-frame-center
              :foreground-color (face-foreground 'hydra-posframe-face nil t)
              :background-color (face-background 'hydra-posframe-face nil t)
	      :internal-border-width hydra-posframe-border-width
	      :internal-border-color (face-attribute 'hydra-posframe-border-face :background)
              :string (eval hint)
	      :override-parameters hydra-posframe-parameters)
             (let ((current-frame
                    (buffer-local-value 'posframe--frame
                                        (get-buffer hydra-posframe-buffer))))
	       (redirect-frame-focus current-frame
                                     (frame-parent current-frame))))))))

;;;###autoload
(defun hydra-posframe-enable ()
  "Enable hydra-posframe."
  (interactive)
  (require 'hydra)
  (advice-add 'hydra-keyboard-quit :override #'hydra-posframe-keyboard-quit)
  (advice-add 'hydra-show-hint :override #'hydra-posframe-show-hint)
  (message "hydra-posframe is enabled, disabling it need to reboot emacs."))

(provide 'hydra-posframe)
;;; hydra-posframe.el ends here
