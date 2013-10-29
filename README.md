# Shut up!

If you have an Emacs shell script, you most likely don't want output
like this:

```
Avoid "Loading vc-git..." messages
```

This package does it's best at shutting Emacs up!

## Installation

Add `shut-up` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "shut-up")
```

## Usage

Simply require it:

```lisp
(require 'shut-up)
```
