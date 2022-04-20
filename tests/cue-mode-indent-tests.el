;;; cue-mode-indent-tests.el -- CUE Indent Tests

;; Copyright (C) 2021-2022  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: languages

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

(require 'cue-mode)
(require 'ert)

(defmacro cue--should-indent (from to)
  "Assert that we indent text FROM producing text TO in `cue-mode'."
  `(with-temp-buffer
     (let ((cue-indent-offset 4))
       (cue-mode)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(ert-deftest cue--test-indent-if ()
  "We should not indent top level fields."
  (cue--should-indent
   "
foo: \"hello\"
    bar: 42"
   "
foo: \"hello\"
bar: 42"))

(ert-deftest cue--test-indent-nested ()
  "We should indent nested hashes."
  (cue--should-indent
   "
value: {
	hello: \"world\"
	struct: {
		a: \"a\"
		b: \"b\"
	}
}"
   "
value: {
	hello: \"world\"
	struct: {
		a: \"a\"
		b: \"b\"
	}
}"
   ))


(ert-deftest cue--test-indent-schemas ()
  "We should indent nested parts of schemas."
  (cue--should-indent
   "
import (
	\"list\"
)

#Schema: {
	hello: string
	life:  int
	nums: [...int]
}

#Constrained: #Schema & {
	hello: =~\"[a-z]+\"
	life:  >0
	nums:  list.MaxItems(10)
}"
   "
import (
	\"list\"
)

#Schema: {
	hello: string
	life:  int
	nums: [...int]
}

#Constrained: #Schema & {
	hello: =~\"[a-z]+\"
	life:  >0
	nums:  list.MaxItems(10)
}"))
