## Sane defaults

SHELL := $(shell which bash)
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS ?= emacs
CASK ?= cask

EMACSFLAGS ?=
TESTFLAGS ?= --reporter ert+duration

PKGDIR =
VERSION = undefined

.DEFAULT_GOAL = build

## File lists

SRCS = esup-child.el esup.el
OBJS = $(SRCS:.el=.elc)

## Internal variables

EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS =

## Program availability

HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
PKGDIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)
RUNEMACS = $(CASK) exec $(EMACSBATCH)
VERSION = $(shell $(CASK) version)
endif

# TODO(serghei): Add byte compile test
# --eval '(setq byte-compile-error-on-warn t)'
# See: https://github.com/jschaf/esup/issues/68
%.elc: %.el $(PKGDIR)
	$(RUNEMACS) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	@touch $(PKGDIR)

## Public targets

.PHONY: .title
.title:
	$(info Esup $(VERSION))

.PHONY: init
init: $(PKGDIR)

.PHONY: test
test:
	$(CASK) exec ert-runner $(TESTFLAGS)

.PHONY: build
build: $(OBJS)

.PHONY: clean
clean:
	$(CASK) clean-elc

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
