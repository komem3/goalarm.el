help:           ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'

build: ## Build lisp files.
	 emacs -batch -f batch-byte-compile ./goalarm.el

test: build ## Test lisp files.
	emacs -batch -l ert -l goalarm-test.el -f ert-run-tests-batch-and-exit
