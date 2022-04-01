all: test

test:
	emacs -batch -l ert -l svgo-test.el -f ert-run-tests-batch-and-exit

.PHONY:	all test
