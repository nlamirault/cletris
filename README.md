Cletris
=======

[![Build Status](http://img.shields.io/travis/nlamirault/cletris.svg)](https://travis-ci.org/nlamirault/cletris)

[![License MIT][badge-license]][LICENSE]

Master :
* [![Circle CI](https://circleci.com/gh/nlamirault/cletris/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/cletris/tree/master)

Develop:
* [![Circle CI](https://circleci.com/gh/nlamirault/cletris/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/cletris/tree/develop)

This is the classic [Tetris](http://en.wikipedia.org/wiki/Tetris) game in Common Lisp.

## Required softwares

You will need :

* [SBCL](http://www.sbcl.org)
* [Quicklisp](http://www.quicklisp.org)

and dependencies :

    $ sudo apt-get install libsdl1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev

## Installation

* Install tools and dependencies :

        $ make init
        $ make deps

* Make binary :

        $ make binary

* Launch game :

        $ roswell/cletris

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


## Development

* Install [roswell][] to setup the Common Lisp environment and install your
Common Lisp implementation using it.:

        $ make init
        $ ros install sbcl

* Install dependencies :

        $ make deps

* Launch unit tests :

        $ make test


## Support / Contribute

See [here](CONTRIBUTING.md)


## Changelog

A changelog is available [here](ChangeLog.md).

## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>


[cletris]: https://github.com/nlamirault/cletris
[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/cletris/blob/master/LICENSE

[Issue tracker]: https://github.com/nlamirault/cletris/issues
