My .emacs.d configs!
=============


In my ~.emacs:
=========

    (add-to-list 'load-path "~/.emacs.d")
    (load "bastos")

In my ~/.emacs.d/local.el:
===============

    (setq twittering-username "login")
    (setq twittering-password "password")

js2.el
======

You have to compile the js2.el:

    emacs --batch --eval '(byte-compile-file "js2.el")'

more on: http://code.google.com/p/js2-mode/issues/detail?id=68