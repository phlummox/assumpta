
# TODO

## Add 'period-escaping' functions for ByteStrings

The Text and String modules have utility functions for
escaping periods at the start of lines, the
ByteString one does not, as yet.
The 'stringsearch' package will do search-and-replace on
ByteStrings if we don't want to write our own impl.

## more tests

-   Test that package builds cleanly from an sdist
    source distribution tgz.
-   Test that Haddock documentation builds cleanly

## build sdist from CI

-  add Releases to GitHub

