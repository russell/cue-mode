;;; cuelang-mode.el --- CUE Lang Major Mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: convenience, tools

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

;;

;;; Code:


(defvar cuelang--identifier-regexp
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regular expression matching a Cuelang identifier.")

(defvar cuelang-font-lock-keywords
  (let ((builtin-regex (regexp-opt '("package" "import" "for" "in" "if" "let") 'words))
        (constant-regex (regexp-opt '("false" "null" "true") 'words))
        ;; All builtin functions (see https://cuelang.org/docs/references/spec/#builtin-functions)
        (standard-functions-regex (regexp-opt '("len" "close" "and" "or" "div" "mod" "quo" "rem")))

    )
  (list
   `(,builtin-regex . font-lock-builtin-face)
   `(,constant-regex . font-lock-constant-face)
   `(,(concat "#" cuelang--identifier-regexp "+:?") . font-lock-type-face)
   `(,(concat cuelang--identifier-regexp "+:") . font-lock-keyword-face)
   ))
"Minimal highlighting for ‘cuelang-mode’.")

(defun cuelang-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eql (char-after string-start)
                              (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))


(defvar cuelang-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (cuelang-syntax-stringify))))))

(defconst cuelang-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments. Cuelang supports /* */ and // as comment delimiters
    (modify-syntax-entry ?/ ". 124" table)
    ;; Additionally, Cuelang supports # as a comment delimiter
    (modify-syntax-entry ?\n ">" table)
    ;; ", ', ,""" and ''' are quotations in Cuelang.
    ;; both """ and ''' are handled by cuelang--syntax-propertize-function
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `cuelang-mode'.")


(define-derived-mode cuelang-mode prog-mode "CUE Lang Mode"
  :syntax-table cuelang-mode-syntax-table
  (setq-local font-lock-defaults '(cuelang-font-lock-keywords ;; keywords
                                   nil  ;; keywords-only
                                   nil  ;; case-fold
                                   nil  ;; syntax-alist
                                   nil  ;; syntax-begin
                                   ))

  (setq-local syntax-propertize-function
              python-syntax-propertize-function)

  ;; cue lang uses tabs for indent by default
  (setq indent-tabs-mode t))

(provide 'cuelang-mode)
;;; cuelang-mode.el ends here
