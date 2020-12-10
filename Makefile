help:           ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'

init: ## Init Cask.
	go get github.com/komem3/goalarm/cmd/goalarm && \
	cask install

build: ## Build lisp files.
	 emacs -batch -f batch-byte-compile ./goalarm.el

test: build test_runner ## Test lisp files.

test_runner: ## run test
	cask exec ert-runner
