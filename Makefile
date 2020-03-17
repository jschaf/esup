include default.mk

%.elc: %.el
	@printf "Compiling $<\n"
	@$(RUNEMACS) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

## Public targets

.PHONY: .title
.title:
	@echo Esup $(VERSION)

.PHONY: init
init: Cask
	@$(CASK) install

.PHONY: test
test:
	@$(CASK) exec buttercup $(TESTFLAGS)

.PHONY: build
build: $(OBJS)

.PHONY: clean
clean:
	$(info Remove all byte compiled Elisp files...)
	@$(CASK) clean-elc

.PHONY: help
help: .title
	@echo 'Run "make init" first to install and update all local dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  help:     Show this help and exit'
	@echo '  init:     Initialize the project (has to be launched first)'
	@echo '  build:    Byte compile Esup package'
	@echo '  test:     Run the non-interactive unit test suite'
	@echo '  clean:    Remove all byte compiled Elisp files as well as build'
	@echo '            artifacts'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	@echo ''
	@echo 'You need $(CASK) to develop and test Esup.'
	@echo 'See http://cask.readthedocs.io/ for more.'
	@echo ''
