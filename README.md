# ESUP - Emacs Start Up Profiler

[![MELPA][melpa badge]][melpa link]
[![MELPA Stable][melpa-s badge]][melpa-s link]
[![Build Status][actions badge]][actions link]

Benchmark Emacs Startup time without ever leaving your Emacs.

<p align="center">
  <img src="./esup-screenshot.png" width="512" height="504"/>
</p>

## Installation

Known to work with GNU Emacs 25.1 and later.  Esup may work with older versions
of Emacs, but this is NOT guaranteed.  Bug reports for problems related to using
Esup with older versions of Emacs will most like not be addressed.

The master of all the material is the Git repository at
https://github.com/jschaf/esup .

NOTE: The `master` branch will always contain the latest _unstable_ version.  If
you wish to check older versions or formal, tagged release, please switch to the
relevant [tag][esup tags].

### Using MELPA

Add MELPA or MELPA Stable to the list of repositories to access this mode.
MELPA tracks this Git repository and updates relatively soon after each commit
or formal release.  For more detail on setting up see [MELPA Getting
Started][melpa help].

For those who want only formal, tagged releases use MELPA Stable:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
```

For those who want rolling releases as they happen use MELPA:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

After initializing packaging system you can install Esup using preferred way:

#### `package-list-packages`

Use <kbd>M-x package-refresh-contents</kbd> and <kbd>M-x
package-list-packages</kbd> to get to the package listing and install `esup`
from there.

#### Manual

You can install `esup` manually by adding following to your init file:

``` emacs-lisp
(unless (package-installed-p 'esup)
    (package-refresh-contents)
    (package-install 'esup))
```

#### Cask

Add following to your [Cask][cask] file:

``` emacs-lisp
(source melpa)

(depends-on "esup")
```

#### `use-package`

Add following to your init file:

``` emacs-lisp
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)
```

### El-Get

Esup is included in the El-Get repository.  To install Esup using El-Get:

<kbd>M-x el-get-install RET esup RET</kbd>

Another way is to create a recipe file `esup.rcp` as follows:

``` emacs-lisp
(:name esup
 :website "https://github.com/jschaf/esup"
 :description "Emacs Start Up Profiler"
 :type github
 :pkgname "jschaf/esup")
```

and add it to a directory present in `el-get-recipe-path`.  Then, use <kbd>M-x
el-get-install RET esup</kbd> or add:

``` emacs-lisp
(el-get-bundle esup)
```

to your init file.

### Manual Install


Download Esup and place the download directory on your `load-path` like so:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/path/to/esup")
```

And add _either_ of the two following lines to your initialization file.  The
first only loads Esup when necessary, the 2nd always during startup of GNU
Emacs.

``` emacs-lisp
(autoload 'esup "esup" "Emacs Start Up Profiler." nil)
;; OR
(require 'esup)
```

## Usage

To start Esup, run <kbd>M-x esup</kbd>, and watch the magic happen.

### Profile a custom file with esup

By default, Esup will profile `user-init-file`.  To profile a custom file, call
`esup` with a prefix argument.  That is, <kbd>C-u M-x esup</kbd>.

## Developing

Patches are always welcome.  To submit a patch, use something like the following
workflow.

- Clone the project:
``` sh
git clone https://github.com/jschaf/esup.git
cd esup
git checkout -b MY-NEW-FIX
```

- Implement your fix

- Ensure that all elisp code is lint-clean with Flycheck

- Test your fix with [Cask][cask]

- Test your fixes with the Emacs Regression Test runner
```
make test
```

- Create a pull request with the normal GitHub user interface

[actions badge]: https://github.com/jschaf/esup/workflows/build/badge.svg
[actions link]: https://github.com/jschaf/esup/actions
[melpa badge]: https://melpa.org/packages/esup-badge.svg
[melpa link]: https://melpa.org/#/esup
[melpa-s badge]: https://stable.melpa.org/packages/esup-badge.svg
[melpa-s link]: https://stable.melpa.org/#/esup
[esup tags]: https://github.com/jschaf/esup/tags
[melpa help]: https://melpa.org/#/getting-started
[cask]: https://cask.github.io
