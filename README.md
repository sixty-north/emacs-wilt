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
