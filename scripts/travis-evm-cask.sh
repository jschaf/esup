#!/bin/sh

# This script will setup Evm (Emacs Version Manager) and Cask on
# Travis to use for Emacs Lisp testing.

export PATH="/home/travis/.evm/bin:$PATH"
export PATH="/home/travis/.cask/bin:$PATH"

git clone https://github.com/rejeep/evm.git /home/travis/.evm
evm config path /tmp
evm install emacs-25.3-travis --use --skip

curl -fsSkL https://raw.github.com/cask/cask/master/go | python
