ESUP - Emacs Start Up Profiler
==============================

Benchmark Emacs Startup time without ever leaving your Emacs.

Installation
============

## With Package (ELPA, MELPA, MARMALADE): ##

First tell `package.el` to use one of the package repository:

```lisp
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
```

**or**

```lisp
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
```

Then install esup:

<kbd>M-x install-package RET esup RET</kbd>

## El-Get ##

Use the following recipe with el-get.

```emacs-lisp
(:name esup
  :website "https://github.com/jschaf/esup"
  :description "Emacs Start Up Profiler"
  :type "github"
  :branch "master"
  :pkgname "jschaf/esup")
```

Then install esup:

<kbd>M-x el-get-install RET esup RET</kbd>

## Manually ##

Download `esup.el` and place the download directory on your
`load-path` like so:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/path/to/esup")
```

Usage
=====

Auto-load the starting function `esup`.
    
```emacs-lisp
(autoload 'esup "esup" "Emacs Start Up Profiler." nil)
```

To start Esup, run <kbd>M-x esup<kbd>, and watch the magic happen.
