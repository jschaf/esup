# Run “make build” by default
.DEFAULT_GOAL = build

EMACS  ?= emacs
CASK   ?= cask

EMACSFLAGS ?=
TESTFLAGS  ?= -L .

EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS   =

# Program availability
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' esup.el)"

# File lists
SRCS = esup-child.el esup.el
OBJS = $(SRCS:.el=.elc)
