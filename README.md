Cletris
=======

[![Build Status](https://travis-ci.org/nlamirault/cletris.png)](https://travis-ci.org/nlamirault/cletris)

This is the classic [Tetris](http://en.wikipedia.org/wiki/Tetris) game in Common Lisp.

## Required softwares

You will need :

* [ASDF](http://www.cliki.net/asdf)
* [PAL](http://common-lisp.net/project/pal)

## Installation

In /usr/share/common-lisp/systems, create a symbolic link
to the system definition file (cletris.asd) or
add the ernestine directory to *central-directory* of ASDF :

    CL-USER> (push "/directory/cletris/" asdf:*central-registry*)
    CL-USER> (asdf:operate 'asdf:load-op 'cletris)

## Launch game

    CL-USER> (cletris:cletris)


## Commands

Available commands :

* s : Start a new game
* Left : Move the current block to the left
* Right : Move the current block to the right
* Up : Rotate the current block
* Down : Move the current block down
* Space : Move the current block all the way down.
* p : Pause game
* v : View score (when user not playing a game).
* q : Quit Cletris


## Screenshots

![0.3](www/cletris-0.3.png)


## Changelog

A changelog is available [here](ChangeLog.md).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>
