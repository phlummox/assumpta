# Development notes and tips

I build and test assumpta and assumpta-core
using [`stack`](https://docs.haskellstack.org/en/stable/README/),
plus CI services (Travis CI and CircleCI).

CircleCI in general is much more irritating, doesn't natively support
'test matrixes', and has obviously had less thought put into its design;
but it's faster, seems to do a better job of cacheing build artifacts,
and lets you use any Docker image you like to run tests in (unlike Travis CI,
which only has a fixed set of supported environments).

If anyone wants to suggest things which might make the source tree
more friendly to cabal-users, feel free to let me know.

## Tests

### Compile-examples

The test suite `compile-examples` (which checks that the
programs in the `examples` directory can be compiled)
requires stack to run, and will
fail if a `STACK_RESOLVER` environment variable isn't found (or if
it can't find `stack` on the `$PATH`). It also is normally disabled,
unless the `stack-based-tests` flag is enabled.

So it is best run like this:

```bash
$ export STACK_RESOLVER=lts-12.26
$ stack --resolver="${STACK_RESOLVER}" test --flag assumpta:stack-based-tests
```

The aim of the test is just to ensure that the examples
are in sync with the API, so I haven't attempted to
get the test working in any other environment.



