;;; jump-char.el --- navigation by char

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: navigation by char
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Jan  9 22:41:43 2012 (+0800)
;; Version: 0.1
;; Last-Updated: Sat Jan 14 18:39:16 2012 (+0800)
;;           By: Le Wang
;;     Update #: 69
;; URL: https://github.com/lewang/jump-char
;; Keywords:
;; Compatibility: 23+

;;; Installation:

;;
;;   (require 'jump-char)
;;
;;   (global-set-key [(meta m)] 'jump-char-forward)
;;   (global-set-key [(shift meta m)] 'jump-char-backward)
;;

;; But what about `back-to-indentation' (bound to M-m by default)?  You should
;; customize C-a to toggle between indentation and beginning of line like a
;; civilized human being.

;;; Commentary:

;; Navigate by char.  The best way to "get" it is to try it.
;;
;; Interface:
;;
;;   <char>   :: move to the next match in the current direction.
;;   ;        :: next match forward (towards end of buffer)
;;   ,        :: next match backward (towards beginning of buffer)
;;   C-c C-c  :: invoke ace-jump-mode if available
;;
;; Any other key stops jump-char and edits as normal.
;;
;; The behaviour is strongly modeled after `iy-go-to-char' with the following
;; differences:
;;
;;   * point always stays before match
;;
;;   * point during search is same as after exiting
;;
;;   * lazy highlighting courtesy of isearch
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
:;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))


(provide 'jump-char)

(require 'ace-jump-mode nil t)

(defvar jump-char-isearch-map
  (let ((map (make-sparse-keymap))
        (exception-list '(isearch-abort isearch-describe-key))
        isearch-commands)
    (flet ((remap (key def)
                  (if (symbolp def)
                      (setq isearch-commands (cons def isearch-commands))
                    (when (keymapp def)
                      (map-keymap 'remap def)))))
      (map-keymap 'remap isearch-mode-map))
    (setq isearch-commands (delete-dups isearch-commands))
    (dolist (cmd isearch-commands)
      (unless (memq cmd exception-list)
        (define-key map `[remap ,cmd] #'jump-char-process-char)))
    (set-keymap-parent map isearch-mode-map)
    (define-key map (kbd ";") #'jump-char-repeat-forward)
    (define-key map (kbd ",") #'jump-char-repeat-backward)
    (when (featurep 'ace-jump-mode)
      (define-key map (kbd "C-c C-c") #'jump-char-switch-to-ace))
    map))

(defvar jump-char-store (make-hash-table :test 'eq :size 5))
(defvar jump-char-lazy-highlight-face lazy-highlight-face)
(defvar jump-char-initial-char nil)

(defsubst jump-char-equal (l r)
  (and (not (null l))
       (not (null r))
       (char-equal l r)))

(defsubst jump-char-printing-p (event-v)
  (when (eq (length event-v) 1)
    (let ((event (aref event-v 0)))
      (and (characterp event)
           (>= event ?\s)
           (<= event (max-char))))))

(defun jump-char-cleanup ()
  (maphash (lambda (key value)
             (if (functionp key)
                 (fset key value)
               (set key value)))
           jump-char-store)
  (remove-hook 'isearch-update-post-hook 'jump-char-isearch-update-func)
  (remove-hook 'isearch-mode-end-hook 'jump-char-cleanup))

(defun jump-char-isearch-update-func ()
  (when (and isearch-forward
             isearch-success
             (not (zerop (length isearch-string)))
             (jump-char-equal (aref isearch-string 0) (char-before)))
    (forward-char -1)))

(defun jump-char-isearch-message-prefix (&optional _c-q-hack ellipsis nonincremental)
  (let ((msg (funcall (gethash 'isearch-message-prefix jump-char-store) _c-q-hack ellipsis nonincremental)))
    (setq msg (replace-regexp-in-string "\\`\\(.*?\\)I-search" "\\1jump-char" msg))
    (propertize msg
                'face 'minibuffer-prompt)))


(defun jump-char-repeat-forward ()
  "keep point at beginning of match"
  (interactive)
  (if (and (zerop (length isearch-string))
           (jump-char-printing-p (this-command-keys-vector)))
      (jump-char-process-char)
    (if isearch-forward
        (skip-chars-forward (string jump-char-initial-char))
      (when isearch-success
        (goto-char isearch-other-end)))
    (isearch-repeat-forward)))

(defun jump-char-repeat-backward ()
  (interactive)
  (if (and (zerop (length isearch-string))
           (jump-char-printing-p (this-command-keys-vector)))
      (jump-char-process-char)
    (isearch-repeat-backward)))

(defun jump-char-switch-to-ace ()
  (interactive)
  (let ((search-nonincremental-instead nil))
    (isearch-exit))
  (if (null jump-char-initial-char)
      (call-interactively 'ace-jump-char-mode)
    (ace-jump-char-mode jump-char-initial-char)))

(defun jump-char-process-char (&optional arg)
  (interactive "P")
  (let* ((keylist (listify-key-sequence (this-command-keys-vector)))
         (command-only-key-v (this-single-command-keys))
         (this-key-global-cmd (let ((isearch-mode 0))
                     (key-binding command-only-key-v nil t)))
         (this-key-is-global-jump-char (car (memq this-key-global-cmd
                                                '(jump-char-forward jump-char-backward)))))
    ;; (message "this-key-is-global-jump-char %s this-key-global-cmd %s" this-key-is-global-jump-char this-key-global-cmd)
    (cond ((and this-key-is-global-jump-char
                (zerop (length isearch-string)))
           (setq isearch-string (string jump-char-initial-char))
           (funcall (if (eq this-key-global-cmd 'jump-char-forward)
                        'jump-char-repeat-forward
                      'jump-char-repeat-backward)))
          ((and (jump-char-printing-p command-only-key-v))
           (if (zerop (length isearch-string))
               (progn
                 (isearch-printing-char)
                 (setq jump-char-initial-char last-command-event))
             (when (eq last-command-event jump-char-initial-char)
               (funcall (if isearch-forward 'jump-char-repeat-forward 'jump-char-repeat-backward)))))
          (t
           (apply 'isearch-unread keylist)
           (setq prefix-arg arg)
           (let ((search-nonincremental-instead nil))
             (isearch-exit))))))

;;;###autoload
(defun jump-char-forward ()
  "
; next

, previous

search_char next

press current binding for `jump-char-forward' / `jump-char-backward' to reuse
last input.
"
  (interactive)
  (let ((backward (when (eq this-command 'jump-char-backward)
                    (setq backward t))))
    (puthash 'isearch-mode-map isearch-mode-map jump-char-store)
    (puthash 'lazy-highlight-face lazy-highlight-face jump-char-store)
    (puthash 'isearch-message-prefix (symbol-function 'isearch-message-prefix) jump-char-store)
    (add-hook 'isearch-mode-end-hook 'jump-char-cleanup)
    (add-hook 'isearch-update-post-hook 'jump-char-isearch-update-func)
    (setq isearch-mode-map jump-char-isearch-map)
    (setq lazy-highlight-face jump-char-lazy-highlight-face)
    (fset 'isearch-message-prefix (symbol-function 'jump-char-isearch-message-prefix))
    (funcall (if backward
                 'isearch-backward
               'isearch-forward)
             nil t)))

;;;###autoload
(defalias 'jump-char-backward 'jump-char-forward)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jump-char.el ends here
