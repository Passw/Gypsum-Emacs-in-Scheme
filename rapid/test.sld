;; Copyright (C) 2017 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;> A Scheme API for test suites compatible to SRFI 64.

(define-library (rapid test)
  (export test-begin
	  test-end
	  test-assert
	  test-eqv
	  test-eq
	  test-equal
	  test-approximate
	  test-error
	  test-apply
	  test-with-runner
	  test-match-nth
	  test-match-all
	  test-match-any
	  test-match-name
	  test-skip
	  test-expect-fail
	  test-read-eval-string
	  test-runner-group-path
	  test-group
	  test-group-with-cleanup
	  test-result-ref
	  test-result-set!
	  test-result-clear
	  test-result-remove
	  test-result-kind
	  test-passed?
	  test-runner?
	  test-runner-reset
	  test-runner-null
	  test-runner-simple
	  test-runner-current
	  test-runner-factory
	  test-runner-get
	  test-runner-create
	  test-runner-test-name
	  test-runner-pass-count
	  test-runner-fail-count
	  test-runner-xpass-count
	  test-runner-xfail-count
	  test-runner-skip-count
	  test-runner-group-stack
	  test-runner-on-test-begin test-runner-on-test-begin!
	  test-runner-on-test-end test-runner-on-test-end!
	  test-runner-on-group-begin test-runner-on-group-begin!
	  test-runner-on-group-end test-runner-on-group-end!
	  test-runner-on-final test-runner-on-final!
	  test-runner-on-bad-count test-runner-on-bad-count!
	  test-runner-on-bad-end-name test-runner-on-bad-end-name!
	  test-result-alist
	  test-runner-aux-value test-runner-aux-value!
	  test-on-test-end-simple
	  test-on-group-begin-simple
	  test-on-group-end-simple
	  test-on-bad-count-simple
	  test-on-bad-end-name-simple
	  test-on-final-simple)
    (import (scheme base)
	    (scheme case-lambda)
	    (scheme eval)
	    (scheme read)
	    (scheme repl)
	    (scheme write))
    (include "test.scm"))


;; Local Variables:
;; eval: (put 'test-assert 'scheme-indent-function 1)
;; eval: (put 'test-eqv 'scheme-indent-function 1)
;; eval: (put 'test-equal 'scheme-indent-function 1)
;; eval: (put 'test-group 'scheme-indent-function 1)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(test-assert\\)\\>" 1 font-lock-keyword-face)
;;                                 ("(\\(test-eqv\\)\\>" 1 font-lock-keyword-face)
;;                                 ("(\\(test-group\\)\\>" 1 font-lock-keyword-face)
;;                                 ("(\\(test-equal\\)\\>" 1 font-lock-keyword-face)))
;; End:
