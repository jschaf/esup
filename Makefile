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
	@echo 'See https://cask.readthedocs.io/ for more.'
	@echo ''
