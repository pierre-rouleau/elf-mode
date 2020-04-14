;;; elf-mode.el --- Show symbols in binaries -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel
;; Copyright (C) 2020 Michael Krasnyk

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/elf-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: matching

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
;; Use `elf-setup-default' to make `elf-mode' get called
;; automatically.

;;; Code:


(defvar elf-mode-buffer-initial-type 'dynamic)

(defvar-local elf-mode-buffer-type elf-mode-buffer-initial-type)

(defvar-local elf-mode-buffer-types
  '((arch-specific   . ((key . "A") (command . "readelf --arch-specific -W %s")))
    (archive-index   . ((key . "c") (command . "readelf --archive-index -W %s")))
    (dynamic         . ((key . "d") (command . "readelf --dynamic -W %s")))
    (headers         . ((key . "e") (command . "readelf --headers -W %s")))
    (section-groups  . ((key . "G") (command . "readelf --section-groups -W %s")))
    (header          . ((key . "h") (command . "readelf --file-header -W %s")))
    (histogram       . ((key . "I") (command . "readelf --histogram -W %s")))
    (program-headers . ((key . "l") (command . "readelf --program-headers -W %s")))
    (notes           . ((key . "n") (command . "readelf --notes -W %s")))
    (relocs          . ((key . "r") (command . "readelf --relocs -W %s")))
    (section-headers . ((key . "S") (command . "readelf --section-headers -W %s")))
    (symbols         . ((key . "s") (command . "readelf --symbols -W %s")))
    (unwind          . ((key . "u") (command . "readelf --unwind -W %s")))
    (version-info    . ((key . "V") (command . "readelf --version-info -W %s")))
    (dyn-syms        . ((key . "x") (command . "readelf --dyn-syms -W %s")))
))

(defvar-local elf-mode-disassemble-command
  "gdb -n -q -batch -ex 'file %s' -ex 'disassemble/rs %s'")

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
    (save-excursion
      (let* ((state (cdr (assoc elf-mode-buffer-type elf-mode-buffer-types)))
             (command (cdr (assoc 'command state)))
             (inhibit-read-only t)
             (file-name
              (cond
               ((tramp-tramp-file-p (buffer-file-name)) (tramp-handle-file-local-copy (buffer-file-name)))
               (t (buffer-file-name)))))
        (erase-buffer)
        (insert
         (shell-command-to-string
          (format command file-name)))
        (set-buffer-modified-p nil)))
      ;;
      (cond
       ((eq elf-mode-buffer-type 'symbols) (elf-add-func-refs)))
      ;;
      (read-only-mode)))

(defun elf-mode-set-disassemble-command (s)
  (interactive "sDisassemble command: ")
  (unless (string-empty-p s)
    (setq elf-mode-disassemble-command s)))

(defun elf-mode-disassemble (o)
  (interactive)
  (let* ((symbol (buffer-substring (overlay-start o) (overlay-end o)))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (command (format elf-mode-disassemble-command (buffer-file-name) symbol)))
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

;;;###autoload
(add-to-list 'magic-mode-alist '("^\177ELF" . elf-mode))

(add-to-list 'magic-mode-alist '("^!<arch>\012/       " . elf-mode))

(provide 'elf-mode)
;;; elf-mode.el ends here
