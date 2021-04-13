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
;; References:
;;    https://en.wikipedia.org/wiki/Executable_and_Linkable_Format#Specifications

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar elf-mode-buffer-initial-type 'dynamic)

(defface elf-mode-disassemble-hex
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the disassembled hex values.")

(defface elf-mode-disassemble-opcode
  '((((class color) (background light)) :foreground "blue"))
  "Face for the disassembled opcode values.")

(defvar-local elf-mode-buffer-type elf-mode-buffer-initial-type)

(defcustom elf-mode-gdb-alist `(("00b7" . "aarch64-linux-gnu-gdb")
                                ("0028" . "arm-none-eabi-gdb"))
  "The gdb binary used by used by the corresponding platform.

Each element has the form (E_MACHINE . GDB).
 E_MACHINE is a elf file header e_machine value, as a string in lower case.
 GDB pecifies the gdb command for corresponding platform."
  :type '(alist :key-type (string :tag "e_machine id")
                :value-type (string :tag "gdb executable")))
(defvar-local elf-mode-gdb-executable "gdb")

(defvar-local elf-mode-buffer-types
  '((arch-specific   . ((key . "A") (command . ("readelf" "-W" "--arch-specific"))))
    (archive-index   . ((key . "c") (command . ("readelf" "-W" "--archive-index"))))
    (dynamic         . ((key . "d") (command . ("readelf" "-W" "--dynamic"))))
    (headers         . ((key . "e") (command . ("readelf" "-W" "--headers"))))
    (section-groups  . ((key . "G") (command . ("readelf" "-W" "--section-groups"))))
    (header          . ((key . "h") (command . ("readelf" "-W" "--file-header"))))
    (histogram       . ((key . "I") (command . ("readelf" "-W" "--histogram"))))
    (program-headers . ((key . "l") (command . ("readelf" "-W" "--program-headers"))))
    (md5sum          . ((key . "m") (command . ("md5sum"))))
    (notes           . ((key . "n") (command . ("readelf" "-W" "--notes"))))
    (relocs          . ((key . "r") (command . ("readelf" "-W" "--relocs"))))
    (section-headers . ((key . "S") (command . ("readelf" "-W" "--section-headers"))))
    (symbols         . ((key . "s") (command . ("readelf" "-W" "--symbols"))))
    (unwind          . ((key . "u") (command . ("readelf" "-W" "--unwind"))))
    (version-info    . ((key . "V") (command . ("readelf" "-W" "--version-info"))))
    (dyn-syms        . ((key . "x") (command . ("readelf" "-W" "--dyn-syms"))))
    (strings         . ((key . "z") (command . ("strings"))))
))

(defvar-local elf-mode-disassemble-command
  "%s -n -q -batch -ex 'file %s' -ex 'disassemble/rs %s'")

(defvar-local elf-mode-binary-command
  "dd status=none bs=1 skip=%s count=%s if=%s")

(defun elf-mode-buffer-file-name ()
  (cond
   ((and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p (buffer-file-name)))
    (tramp-handle-file-local-copy (buffer-file-name)))
   (t (buffer-file-name))))

(defun elf-add-func-refs ()
  (goto-char (point-min))
  (while (re-search-forward "FUNC[[:space:]]+\\([[:alnum:]]+[[:space:]]+\\)\\{3\\}\\(\\sw+\\)" nil t)
    (let* ((name (match-string 2))
           (ol (make-button
                (match-beginning 2) (match-end 2)
                'help-echo (format "disassemble %s" name)
                'action #'elf-mode-disassemble
                'mouse-action #'elf-mode-disassemble)))
      (overlay-put ol 'symbol name))))

(defun elf-add-sections-refs ()
  (goto-char (point-min))
  (while (re-search-forward "^ *\\[ *[[:digit:]]+] +\\([^ ]+\\) +[^ ]+ +[^ ]+ +\\([^ ]+\\) +\\([^ ]+\\)" nil t)
    (when (string-prefix-p "." (match-string 1))
      (let* ((name (match-string 1))
             (offset (string-to-number (match-string 2) 16))
             (size (string-to-number (match-string 3) 16))
             (ol (make-button
                  (match-beginning 1) (match-end 1)
                  'help-echo (format "hexl %s" (match-string 1))
                  'action #'elf-mode-binary
                  'mouse-action #'elf-mode-binary)))
        (overlay-put ol 'section name)
        (overlay-put ol 'offset offset)
        (overlay-put ol 'size size)))))

(defun elf-revert-buffer ()
  (interactive)
  (when (eq 'elf-mode major-mode)
    (let* ((state (cdr (assoc elf-mode-buffer-type elf-mode-buffer-types)))
           (command (cdr (assoc 'command state)))
           (stdout (current-buffer))
           (inhibit-read-only t)
           (stderr (generate-new-buffer "*readelf stderr*"))
           (file-name (elf-mode-buffer-file-name)))
      (setf (buffer-string) "")
      (let* ((get_e_machine_command
              (format "hexdump -e '1/2 \"%s\"' -s 0x12 -n 2 %s" "%04x" file-name))
             (e_machine (string-trim (shell-command-to-string get_e_machine_command)))
             (gdb (assoc e_machine elf-mode-gdb-alist)))
        (if gdb (setq-local elf-mode-gdb-executable (cdr gdb))))

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
              ((or (eq elf-mode-buffer-type 'headers)
                   (eq elf-mode-buffer-type 'section-headers))
               (elf-add-sections-refs))
              ((or (eq elf-mode-buffer-type 'dyn-syms)
                   (eq elf-mode-buffer-type 'symbols))
               (elf-add-func-refs))))
           (goto-char (point-min))
           (set-buffer-modified-p nil)
           (read-only-mode))
         (with-current-buffer stderr
           (let ((err (string-trim (buffer-string))))
             (unless (string= "" err)
               (message "elf-mode: %s\n%s" event err))))
         (kill-buffer stderr))))))

(defun elf-mode-disassemble (overlay)
  (let* ((symbol (overlay-get overlay 'symbol))
         (buffer-name (format "%s(%s)" (buffer-name) symbol))
         (command (format elf-mode-disassemble-command elf-mode-gdb-executable
                          (elf-mode-buffer-file-name) symbol)))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (flush-lines "^[[:space:]]*$" (point-min) (point-max))
      (set-buffer-modified-p nil)
      (asm-mode)
      (while (re-search-forward "\\(<\\+[[:digit:]]+>:\\)\t\\([a-f0-9 ]+\\)\t\\([a-zA-Z0-9]+\\)" nil t)
        (let* ((offset (concat (match-string 1) (make-string (+ 1 (max 0 (- 8 (length (match-string 1))))) ? )))
               (hex (concat (match-string 2) (make-string (+ 1 (max 0 (- 32 (length (match-string 2))))) ? )))
               (opcode (match-string 3))
               (beg1 (match-beginning 1))
               (beg2 (+ beg1 (length offset)))
               (beg3 (+ beg2 (length hex)))
               (end3 (+ beg3 (length opcode))))
          (replace-match (concat offset hex opcode))
          (put-text-property beg2 beg3 'font-lock-face 'elf-mode-disassemble-hex)
          (put-text-property beg3 end3 'font-lock-face 'elf-mode-disassemble-opcode)))
      (setq-local asm-comment-char ?#)
      (read-only-mode))))

(defun elf-mode-binary (overlay)
  (let* ((section (overlay-get overlay 'section))
         (offset (overlay-get overlay 'offset))
         (size (overlay-get overlay 'size))
         (buffer-name (format "%s(%s)" (buffer-name) section))
         (command (format elf-mode-binary-command offset size (elf-mode-buffer-file-name))))
    (with-current-buffer (pop-to-buffer buffer-name)
      (shell-command command (current-buffer))
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil)
      (hexl-mode)
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
