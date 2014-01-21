# Shut up!

If you have an Emacs shell script, you most likely don't want output
like this:

```
Loading vc-git...
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

This changes Emacs settings to reduce the output.  To silence individual
functions, use the `shut-up` macro:

```lisp
(let (output)
  (shut-up
    (message "Foo")
    (setq output (shut-up-current-output)))
  (message "This was the last message: %s" output))
```
