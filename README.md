# wilt.el

An Emacs extension for calculating **W**hitespace **I**ntegrated over
**L**ines of **T**ext (WILT) on your code.

WILT is a somewhat whimsical code metric invented by Rob
Smallshire. In essence, it uses leading whitespace as a measure of
code complexity. Surprisingly, WILT correlates very well with other,
more "sophisticated" metrics like cyclomatic complexity, but WILT has
the benefit of being *far* easier to calculate.

This extension provides the `wilt-mode` minor mode for emacs. This
mode will occassionally recalculate the WILT for a buffer and display
it in the status line.

![Screenshot](https://raw.github.com/sixty-north/emacs-wilt/master/screenshot.png)

## Installation and setup

This simplest way to install `wilt-mode` is through
[MELPA](https://melpa.org/). If MELPA is one of your package archives,
install using `package`:

```
M-x package-install wilt
```

To use this extension, first require the extension:

```
(require 'wilt)
```

Then go to the buffer in which you want to calculate WILT and run:

```
M-x wilt-mode
```

## Customization

`wilt-mode` can be customized in a number of ways through the standard
Emacs customization machinery.

### Changing the mode-line display

You can control how WILT is displayed in your mode-line through the
`wilt-mode-line-template` variable. This is a string that is used as a
formatting template. When display is necessary, the current WILT value
(a floating point number) is passed to this template, and the
interpolated result is displayed.

For example, to display WILT like `W=1.23`, you would use a value of
`"W=%.2f"`.

### Controlling when WILT is recalculated

By default WILT is calculated when you enter the mode, when a newline
is entered, and when the buffer is saved. You can change this behavior
by customizing the `wilt-update-conditions` variable. This is simply a
list of atoms which indicate the conditions under which WILT should
ber recalculated.
