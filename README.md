# .emacs.d
Landscape for my zen garden.

This installs the list of packages outlined in `pkg.el`
using the default package manager:
[Package.el](http://wikemacs.org/wiki/Package.el).

The automatic install requires an internet connection. Your first time
starting emacs will take a while because it will install everything
that first go. After that it'll be buttery, though.

## Usage

Here's what you do
```bash
$ git clone https://github.com/bkc39/.emacs.d.git $HOME/.emacs.d
$ emacs
```
And thats it!

## Requirements

I've only ever tested this on my machine running Mac OS X and Emacs 24
(installed from [Homebrew](http://brew.sh/)). I think it should work
on Linux and any version of Emacs with `package.el`.
