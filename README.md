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

Use the `shut-up` macro to silence function calls:

```lisp
(let (output)
  (shut-up
    (message "Foo")
    (setq output (shut-up-current-output)))
  (message "This was the last message: %s" output))
```

In non-interactive sessions, you can also use `shut-up-silence-emacs` to change
some global Emacs settings to reduce output:

```lisp
(when noninteractive
  (shut-up-silence-emacs))
```
