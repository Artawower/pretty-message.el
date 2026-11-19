;;; pretty-message.el --- Decorate echo-area messages with icons and faces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Artur Iaroshenko

;; Author: Artur Iaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/pretty-message.el
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;; Keywords: convenience, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `pretty-message-mode' is a global minor mode that restyles messages
;; shown in the echo area.  Each message is classified as an error, a
;; success or plain information by matching it against
;; `pretty-message-error-regexps' and `pretty-message-success-regexps',
;; then gets a prefix icon and a face for its class.
;;
;; Only the echo-area display changes: the *Messages* buffer keeps the
;; original text.
;;
;; Values printed by `eval-expression', `eval-last-sexp' and
;; `eval-defun' are decorated too, so evaluating (message "TEXT") shows
;; the decorated TEXT instead of the raw "TEXT" return value.
;;
;; Usage:
;;
;;   (require 'pretty-message)
;;   (pretty-message-mode 1)
;;
;; The mode works through `set-message-functions' (Emacs 29.1+), so it
;; has no effect if `set-message-function' was changed from its default
;; `set-message-functions'.

;;; Code:

(defgroup pretty-message nil
  "Decorate echo-area messages."
  :group 'convenience
  :prefix "pretty-message-")

;;;; Classification

(defcustom pretty-message-error-regexps
  '("is undefined"
    "Error:"
    "error:"
    "Wrong type argument"
    "Invalid function"
    "void-function"
    "failed")
  "Regexps that mark a message as an error.
A message is an error if any of them matches anywhere in it.
Matching is case-sensitive."
  :type '(repeat regexp))

(defcustom pretty-message-success-regexps
  '("\\`Wrote "
    "\\`Saved "
    "\\`Done")
  "Regexps that mark a message as a success.
Checked only when no regexp in `pretty-message-error-regexps'
matches.  Matching is case-sensitive."
  :type '(repeat regexp))

;;;; Prefixes

(defcustom pretty-message-error-prefix " ✗  "
  "String inserted before error messages."
  :type 'string)

(defcustom pretty-message-success-prefix " ✓  "
  "String inserted before success messages."
  :type 'string)

(defcustom pretty-message-info-prefix " ❯❯  "
  "String inserted before messages that are neither errors nor successes."
  :type 'string)

;;;; Faces

(defface pretty-message-error '((t :inherit error))
  "Face for the text of error messages.")

(defface pretty-message-success '((t :inherit success))
  "Face for the text of success messages.")

(defface pretty-message-info '((t :inherit font-lock-string-face))
  "Face for the text of informational messages.")

(defface pretty-message-error-prefix '((t :inherit pretty-message-error))
  "Face for `pretty-message-error-prefix'.")

(defface pretty-message-success-prefix '((t :inherit font-lock-keyword-face))
  "Face for `pretty-message-success-prefix'.")

(defface pretty-message-info-prefix '((t :inherit font-lock-keyword-face))
  "Face for `pretty-message-info-prefix'.")

;;;; Implementation

(defvar pretty-message--last nil
  "Text of the last message decorated by `pretty-message-decorate'.")

(defvar pretty-message--eval-commands
  '(eval-expression eval-last-sexp eval-defun)
  "Commands whose printed value is decorated by `pretty-message-mode'.")

(defun pretty-message--match-any-p (regexps string)
  "Return non-nil if any regexp in REGEXPS matches STRING."
  (let ((case-fold-search nil))
    (seq-some (lambda (re) (string-match-p re string)) regexps)))

(defun pretty-message--classify (msg)
  "Return the class of MSG: `error', `success' or `info'."
  (cond ((pretty-message--match-any-p pretty-message-error-regexps msg) 'error)
        ((pretty-message--match-any-p pretty-message-success-regexps msg) 'success)
        (t 'info)))

(defun pretty-message-decorate (msg)
  "Return MSG with a prefix and faces according to its class.
Return nil for empty or already decorated messages, which tells
the option `set-message-functions' to pass MSG on unchanged.

Faces are appended, so faces already set by the caller of
`message' take precedence."
  (when (and (stringp msg)
             (> (length msg) 0)
             ;; Code that saves `current-message' and shows it again
             ;; would otherwise get a second prefix.
             (not (get-text-property 0 'pretty-message msg)))
    (let* ((class (pretty-message--classify msg))
           (prefix (pcase class
                     ('error pretty-message-error-prefix)
                     ('success pretty-message-success-prefix)
                     (_ pretty-message-info-prefix)))
           (prefix-face (pcase class
                          ('error 'pretty-message-error-prefix)
                          ('success 'pretty-message-success-prefix)
                          (_ 'pretty-message-info-prefix)))
           (face (pcase class
                   ('error 'pretty-message-error)
                   ('success 'pretty-message-success)
                   (_ 'pretty-message-info)))
           (body (copy-sequence msg)))
      (setq pretty-message--last msg)
      (add-face-text-property 0 (length body) face t body)
      (propertize (concat (propertize prefix 'face prefix-face) body)
                  'pretty-message t))))

(defun pretty-message--redisplay-eval-result (orig &rest args)
  "Call ORIG with ARGS, then decorate the value it printed.
Eval commands print their value with `prin1' straight into the echo
area, which skips the option `set-message-functions' and hides a decorated
message.  When the printed value is just the last message string,
show that message again, so (message \"TEXT\") displays as TEXT;
otherwise decorate the printed value itself."
  (let ((pretty-message--last nil))
    (prog1 (apply orig args)
      (let ((shown (current-message)))
        (when (and (> (length shown) 0)
                   (not (get-text-property 0 'pretty-message shown)))
          ;; The value is already in *Messages*; don't log it twice.
          (let ((message-log-max nil))
            (message "%s"
                     (if (and pretty-message--last
                              (equal shown (prin1-to-string pretty-message--last)))
                         pretty-message--last
                       shown))))))))

;;;###autoload
(define-minor-mode pretty-message-mode
  "Decorate echo-area messages with a prefix icon and faces.
See `pretty-message-decorate' for how messages are styled."
  :global t
  :group 'pretty-message
  (if pretty-message-mode
      (progn
        ;; Run late so `inhibit-message', which must be first, still
        ;; sees the original text.
        (add-hook 'set-message-functions #'pretty-message-decorate 90)
        (dolist (command pretty-message--eval-commands)
          (advice-add command :around #'pretty-message--redisplay-eval-result)))
    (remove-hook 'set-message-functions #'pretty-message-decorate)
    (dolist (command pretty-message--eval-commands)
      (advice-remove command #'pretty-message--redisplay-eval-result))))

(provide 'pretty-message)
;;; pretty-message.el ends here
