# assumpta [![Hackage version](https://img.shields.io/hackage/v/assumpta-ci.svg?label=Hackage)](https://hackage.haskell.org/package/assumpta-ci) [![Linux Build Status](https://img.shields.io/travis/com/phlummox/assumpta.svg?label=Linux%20build)](https://travis-ci.com/phlummox/assumpta) [![phlummox](https://circleci.com/gh/phlummox/assumpta.svg?style=svg)](https://circleci.com/gh/phlummox/assumpta)

An SMTP client library. It provides functionality for sending SMTP
commands (including `STARTTLS`) to a server and checking for expected
responses. Also for just sending an email.

## Installation

`assumpta` can be installed in the standard way using `stack`
or `cabal` (e.g. `stack install assumpta`).

## Usage

See the `simple-client-session-text.hs` program in the 
`examples` directory, or, say, the
`Network.Mail.Assumpta.ByteString` module
 for a examples of usage.


