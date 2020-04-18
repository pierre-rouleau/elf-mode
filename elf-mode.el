;;; elf-mode.el --- Show symbols in binaries -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel
;; Copyright (C) 2020 Michael Krasnyk

;; Author: Oleh Krehel <ohwoeowho@gmail.com>, Michael Krasnyk <michael.krasnyk@gmail.com>
;; URL: https://github.com/oxidase/elf-mode
;; Package-Requires: ((emacs "25"))
;; Version: 1.0
;; Keywords: elf readelf convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Toggle `elf-mode' to show the symbols that the binary uses instead
;; of the actual binary contents.
;;

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar elf-mode-buffer-initial-type 'dynamic)

(defvar-local elf-mode-buffer-type elf-mode-buffer-initial-type)

(defvar-local elf-mode-buffer-types
  '((arch-specific   . ((key . "A") (command . ("readelf" "-W" "--arch-specific"))))
    (archive-index   . ((key . "c") (command . ("readelf" "-W" "--archive-index"))))
    (dynamic         . ((key . "d") (command . ("readelf" "-W" "--dynamic"))))
    (headers         . ((key . "e") (command . ("readelf" "-W" "--headers"))))
    (section-groups  . ((key . "G") (command . ("readelf" "-W" "--section-groups"))))
    (header          . ((key . "h") (command . ("readelf" "-W" "--file-header"))))
    (histogram       . ((key . "I") (command . ("readelf" "-W" "--histogram"))))
    (program-headers . ((key . "l") (command . ("readelf" "-W" "--program-headers"))))
    (notes           . ((key . "n") (command . ("readelf" "-W" "--notes"))))
    (relocs          . ((key . "r") (command . ("readelf" "-W" "--relocs"))))
    (section-headers . ((key . "S") (command . ("readelf" "-W" "--section-headers"))))
    (symbols         . ((key . "s") (command . ("readelf" "-W" "--symbols"))))
    (unwind          . ((key . "u") (command . ("readelf" "-W" "--unwind"))))
    (version-info    . ((key . "V") (command . ("readelf" "-W" "--version-info"))))
    (dyn-syms        . ((key . "x") (command . ("readelf" "-W" "--dyn-syms"))))
))

(defvar-local elf-mode-disassemble-command
  "gdb -n -q -batch -ex 'file %s' -ex 'disassemble/rs %s'")

(defun elf-mode-buffer-file-name ()
  (cond
   ((and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p (buffer-file-name)))
    (tramp-handle-file-local-copy (buffer-file-name)))
   (t (buffer-file-name))))

(defun elf-add-func-refs ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "FUNC" nil t)
      (dotimes (_i 3)
        (skip-chars-forward " \t")
        (search-forward " "))
      (let ((beg (point))
            (end (progn (forward-word) (point))))
        (make-button
         beg end
         'action #'elf-mode-disassemble
         'mouse-action #'elf-mode-disassemble)))))

(defun elf-revert-buffer ()
  (interactive)
  (when (eq 'elf-mode major-mode)
    (let* ((state (cdr (assoc elf-mode-buffer-type elf-mode-buffer-types)))
           (command (cdr (assoc 'command state)))
           (stdout (current-buffer))
           (stderr (generate-new-buffer "*readelf stderr*"))
           (file-name (elf-mode-buffer-file-name)))
      (setf (buffer-string) "")

      (make-process
       :name "readelf"
       :buffer nil
       :stderr stderr
       :noquery t
       :command (append command `(,file-name))
       :filter
       (lambda (proc _msg)
         (when (buffer-live-p stdout)
           (with-current-buffer stdout
             (setq-local inhibit-read-only t)
             (goto-char (point-max))
             (insert _msg))))
       :sentinel
       (lambda (proc event)
         (with-current-buffer stdout
           (when (string= event "finished\n")
             (cond
              ((eq elf-mode-buffer-type 'symbols) (elf-add-func-refs))))
           (goto-char (point-min))
           (set-buffer-modified-p nil)
           (read-only-mode))
         (with-current-buffer stderr
           (let ((err (string-trim (buffer-string))))
             (unless (string= "" err)
               (message "elf-mode: %s\n%s" event err))))
         (kill-buffer stderr))))))

(defun elf-mode-set-disassemble-command (s)
  (interactive "sDisassemble command: ")
  (unless (string-empty-p s)
    (setq elf-mode-disassemble-command s)))

(defun elf-mode-disassemble (o)
  (interactive)
  (let* ((symbol (buffer-substring (overlay-start o) (overlay-end o)))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (command (format elf-mode-disassemble-command (elf-mode-buffer-file-name) symbol)))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (flush-lines "^[[:space:]]*$" (point-min) (point-max))
      (set-buffer-modified-p nil)
      (asm-mode)
      ;;(gdb-disassembly-mode)
      (setq-local asm-comment-char ?#)
      (setq-local tab-width 8)
      (read-only-mode))))


(defconst elf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st))

(setq elf-mode-map (make-keymap))
(suppress-keymap elf-mode-map)

(defmacro elf-mode-create-key-binding (arg)
  `(defun ,(intern (concat "elf-mode-" arg)) ()
     "Macro-generated key-binding function that changes buffer type"
     (interactive)
     (setq elf-mode-buffer-type (intern ,arg))
     (elf-revert-buffer)))

(mapcar
 (lambda (x)
   (let* ((key (cdr (assoc 'key x)))
          (state-name (car x)))
     (eval (macroexpand-1 `(elf-mode-create-key-binding ,(symbol-name state-name))))
     (define-key elf-mode-map key (intern (concat "elf-mode-" (symbol-name state-name))))
     ))
 elf-mode-buffer-types)


;;;###autoload
(define-derived-mode elf-mode special-mode "Elf"
  "TODO"
  :syntax-table elf-mode-syntax-table
  (buffer-disable-undo)
  (elf-revert-buffer))

;; TODO
;; ;; (require 's)
;; (defun debug-buffer-advice (fun arg)
;;   ;(when (s-contains? "treemacs-persist" arg)
;;                                         ;(backtrace);)
;;   (print fun)
;;   (print arg)
;;   (funcall fun arg))
;; (advice-add #'get-buffer-create :around #'debug-buffer-advice)

;;;###autoload
(add-to-list 'magic-mode-alist '("^\177ELF" . elf-mode))

(add-to-list 'magic-mode-alist '("^!<arch>\012/       " . elf-mode))

(provide 'elf-mode)
;;; elf-mode.el ends here
