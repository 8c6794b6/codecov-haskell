codecov-haskell
===============

[![Build
Status](http://img.shields.io/travis/8c6794b6/codecov-haskell/master.svg?logo=travis)](https://travis-ci.org/8c6794b6/codecov-haskell)
[![codecov](https://codecov.io/gh/8c6794b6/codecov-haskell/branch/master/graph/badge.svg)](https://codecov.io/gh/8c6794b6/codecov-haskell)
[![BSD3
License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Version on
Hackage](http://img.shields.io/hackage/v/codecov-haskell.svg)](http://hackage.haskell.org/package/codecov-haskell)

codecov-haskell converts and sends Haskell projects hpc code coverage
to [codecov.io](http://codecov.io/).

At the moment, [Travis CI](https://travis-ci.org) has been tested, but
codecov-haskell should be compatible with other CI services in the
future.

<!-- At the moment, [Travis CI](https://travis-ci.org), [Circle -->
<!-- CI](https://circleci.com) and [Jenkins CI](https://jenkins-ci.org) -->
<!-- have been tested, but codecov-haskell should be compatible with other -->
<!-- CI services in the near future. -->

codecov-haskell is still under development and any contributions are welcome!

# Usage

## Travis CI

Below is a sample configuration for a project `.travis.yml` using
`cabal-install`:

```yaml
language: c

os: linux

git:
  depth: 3

cache:
  directories:
    - $HOME/.cabal/packages
    - $HOME/.cabal/store

jobs:
  include:
    - addons:
        apt: {sources: [hvr-ghc], packages: [cabal-install-3.0,ghc-8.8.2]}

install:
  - export PATH=/opt/ghc/bin:$PATH
  - travis_retry cabal v2-update

script:
  - cabal v2-configure --enable-tests --enable-coverage
  - cabal v2-build
  - cabal v2-test

after_script:
  # Building and installing codecov-haskell from git HEAD
  - git clone --depth 3 https://github.com/8c6794b6/codecov-haskell
  - ( cd codecov-haskell && cabal v2-install )
  - export PATH=$HOME/.cabal/bin:$PATH
  - which codecov-haskell

  # Variables for .tix and .mix arguments
  - VANILLA=$(find ./dist-newstyle -name 'vanilla' | head -1)
  - PKG=$(echo $VANILLA | cut -d/ -f6)
  - TIX=$VANILLA/tix/$PKG/$PKG.tix
  - MIX_DIR=$VANILLA/mix/$PKG
  - codecov-haskell --tix=$TIX --mix-dir=$MIX_DIR ccovhs-example-test
```

<!-- If your build fails during the test phase with an error message -->
<!-- starting by "hpc:", just replace the `cabal test` command by -->
<!-- `run-cabal-test`, as in the following example: -->

<!-- ```yaml -->
<!-- before_install: -->
<!--   - cabal install codecov-haskell -->
<!-- script: -->
<!--   - cabal configure --enable-tests --enable-library-coverage && cabal build -->
<!--   - run-cabal-test [options] [cabal-test-options] -->
<!-- after_script: -->
<!--   - codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- This will prevent the build to fail because of hpc related reasons, -->
<!-- which are usually not fatal and should not affect the coverage -->
<!-- data. Details are available in the next section. -->

<!-- You may also experience some issues related to your project -->
<!-- dependencies, which can be solved by using the -->
<!-- `--avoid-reinstalls`/`--force-reinstalls` flags.</br> Another way to -->
<!-- solve problems related dependencies is to install codecov-haskell in a -->
<!-- sandbox, as in the example below: -->

<!-- ```yaml -->
<!-- after_script: -->
<!--   - cabal sandbox init && cabal install codecov-haskell -->
<!--   - .cabal-sandbox/bin/codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- ## Circle CI -->

<!-- In your test section of your `circle.yml` add the following: -->
<!-- ```yaml -->
<!-- test: -->
<!--   pre: -->
<!--     - cabal install codecov-haskell -->
<!--     - cabal configure --enable-tests --enable-library-coverage -->
<!--     - cabal build -->
<!--   override: -->
<!--     - cabal test -->
<!--   post: -->
<!--     - codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- If your build fails during the test phase with an error message -->
<!-- starting by "hpc:", just replace the `cabal test` command by -->
<!-- `run-cabal-test`, as in the following example: -->

<!-- ```yaml -->
<!-- test: -->
<!--   pre: -->
<!--     - cabal install codecov-haskell -->
<!--     - cabal configure --enable-tests --enable-library-coverage -->
<!--     - cabal build -->
<!--   override: -->
<!--     - run-cabal-test -->
<!--   post: -->
<!--     - codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- ## Jenkins CI -->

<!-- In your build script add the following commands: -->

<!-- ```bash -->
<!-- cabal install codecov-haskell -->
<!-- cabal configure --enable-tests --enable-library-coverage && cabal build && cabal test -->
<!-- codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- If your build fails during the test phase with an error message -->
<!-- starting by "hpc:", just replace the `cabal test` command by -->
<!-- `run-cabal-test`, as in the following example: -->

<!-- ```bash -->
<!-- cabal install codecov-haskell -->
<!-- cabal configure --enable-tests --enable-library-coverage && cabal build -->
<!-- run-cabal-test [options] [cabal-test-options] -->
<!-- codecov-haskell [options] [test-suite-names] -->
<!-- ``` -->

<!-- ## The run-cabal-test command -->

<!-- Under certain conditions related to the project structure and the -->
<!-- version of hpc, `cabal test` may output an error message and exit with -->
<!-- the error code `1`, which would result in a build failure.<br/> -->

<!-- To prevent this from happening, codecov-haskell provides the -->
<!-- `run-cabal-test` command which runs `cabal test` and returns with `0` -->
<!-- if the following regular expression never matches any line of the -->
<!-- output: -->

<!-- ```perl -->
<!-- /^Test suite .*: FAIL$/ -->
<!-- ``` -->

<!-- Below are some of the conditions under which you will likely need to -->
<!-- use `run-cabal-test`: -->

<!-- - when using GHC 7.6 (hpc 0.6 known issue) -->
<!-- - when using GHC 7.8 with multiple test suites covering the same module(s) -->

<!-- ### Options -->

<!-- The `--cabal-name` option can be used to specify a custom executable -->
<!-- name instead of the default `cabal` when calling `cabal test`.<br/> -->
<!-- Below is an example which can be useful for projects with a Travis -->
<!-- configuration based on -->
<!-- [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis): -->

<!-- ```yaml -->
<!-- run-cabal-test --cabal-name=cabal-1.20 -->
<!-- ``` -->

## The codecov-haskell command

This command parses the hpc generated output, converts its to Codecov
json format and finally sends it to codecov.io over http.<br/>

<!-- Multiple test suites can be specified, in which case the coverage -->
<!-- report will be made of the merged coverage data generated by the -->
<!-- specified test suites.<br/> For example, if your test suite are named -->
<!-- `test1` and `test2`, use the command as follows: -->

<!-- ```yaml -->
<!-- codecov-haskell test1 test2 -->
<!-- ``` -->

### Options

#### --exclude-dir

The `--exclude-dir` option allows to exclude source files located under a given directory from the coverage report.<br/>
You can exclude source files located under the `test/` directory by using this option as in the following example:

```yaml
codecov-haskell --exclude-dir=test [test-suite-names]
```

You can specify multiple excluded folders by using the following example syntax:

```yaml
codecov-haskell --exclude-dir=test1 --exclude-dir=test2 [test-suite-names]
```

#### --display-report

This boolean option prints the raw json coverage report to be sent to
codecov.io.

#### --dont-send

This boolean option prevents codecov-haskell from sending the coverage
report to codecov.io.  This option can be used together with
`--display-report` for testing purpose.<br/> For example, you can try
various combinations of the other options and confirm the difference
in the resulting report outputs.

#### --print-response

This boolean option prints the raw json response received after
posting the coverage report to codecov.io.

# Limitations

## Total coverage

Because of the way hpc works, coverage data is only generated for
modules that are referenced directly or indirectly by the test suites.
As a result, the total package coverage computed by Codecov may be
higher than what it really is.  An option will be added soon in order
to allow specifying source folders to include in the total coverage
computation.

## Expression level coverage

Even though hpc supports expression level coverage, this version of
codecov-haskell does not support it yet, but this feature will be
implemented soon.  Meanwhile, the hpc coverage information is
converted into a line based report, in which a line can be: fully
covered (green), partially covered (yellow) and not covered (red).

# Contributing

codecov-haskell is still under development and any contributions are welcome!

# License

BSD3 ([tl;dr](https://tldrlegal.com/license/bsd-3-clause-license-(revised)))

# Notes

- HPC publication: http://ittc.ku.edu/~andygill/papers/Hpc07.pdf
