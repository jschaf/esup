# Copyright (C) 2014, 2015, 2016, 2017, 2018, 2019, 2020 Joe Schafero
#
# This file is NOT part of GNU Emacs.
#
# License
#
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

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
