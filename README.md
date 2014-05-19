# emacs-git-grep

## Abstract

`emacs-git-grep.el` is a interface of git grep with helm.


## Requirements

* git
* helm

## Installation

Please clone this repository.

```
git clone https://github.com/ganmacs/emacs-git-grep.git
```

Open your .emacs or init.el.

```
(add-to-list 'load-path "~/emacs.d/emacs-git-grep")
```

And then just require as normal.

```
(require 'eamcs-git-grep)
```

## example settings

```
(require 'eamcs-git-grep)
(global-set-key (kbd "C-c g") 'emacs-git-grep)
```
