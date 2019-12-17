[![Build Status](https://secure.travis-ci.org/doublep/emake.png)](http://travis-ci.org/doublep/emake)


# Emake

Emake is an Emacs-based build system, targeted solely at Elisp
projects.  It is an alternative to Cask.  Unlike Cask, Emake itself is
fully written in Elisp and its configuration files are also Elisp
programs.  If you are familiar with Java world, Cask can be seen as a
parallel to Maven — it uses project *description*, while Emake is sort
of a parallel to Gradle — its configuration is a *program* on its own.


## Brief overview

Emake features:

* Emake configuration is Elisp.  It can change many defaults or even
  define additional commands and options.
* Built-in support for regression/unit testing.
* There are *four* levels of configuration — you can customize most
  aspects of Emake for your project needs and personal preferences.
* You can use local dependencies, even those that don’t use Emake
  (though some restrictions do apply, of course).  This is similar to
  Cask linking, but with more flexibility.
* Emake is fast.

Drawbacks:

* Emake doesn’t run the project being tested/built in a separate
  process, so it is not as pure as Cask.  However, Emacs packages
  won’t live in a sterile world anyway: typical user setup will
  include dozens of other packages.
* Emake depends much more on Emacs internals.  It is more likely to
  break with future Emacs versions than Cask.
* Emake is a recent development and is not widely used, so there can
  be bugs.  However, Emake contains a reasonably large regression test
  collection, so it is not completely untested.


## Requirements

Emake runs on Emacs 24.4 and up [*].  On earlier Emacs versions it
will be overly verbose, but this is rather an Emacs problem.

Linux or other POSIX-like system is currently required.  However,
since there is only a small shell script that is really OS-dependent,
porting to other systems should not be difficult (volunteers welcome).

Emake intentionally has no dependencies, at least currently: otherwise
your project would also see them, which could in theory lead to some
problems.

[*] Emake could reasonably be backported to work on Emacs 24.1 and up
    if anyone interested has access to such old versions.


## Installation

There are several ways to install Emake.

1. Bootstrapping from Melpa.

   FIXME: Not available from Melpa yet.

2. Installing from sources.

   * Clone the source tree from GitHub.

   * In the cloned working directory execute:

         $ ./install.sh DIRECTORY

     Here `DIRECTORY` is the location of `emake` executable should be
     put.  It should be in `$PATH` environment variable, or else you
     will need to specify full path each time you invoke Emake.  You
     probably have sth. like `~/bin` in your `$PATH` already, which
     would be a good value for `DIRECTORY`.  You could even install in
     e.g. `/usr/local/bin` — but make sure you have permissions first.

3. Mostly for developing Emake itself.

   * Clone the source tree from GitHub.

   * Set variable `EMAKE_LOCAL` to the full path of the working
     directory.

   * Make sure executable `emake` is available.  Either follow any of
     the first way to install Emake, or symlink/copy file `bin/emake`
     from the cloned directory to somewhere on your `$PATH`.

   Now each time Emake is executed, it will use the source at
   `$EMAKE_LOCAL`.  You can even modify it and see how that affects
   Emake immediately.


## Getting started

Emake comes with built-in help.  Just run:

    $ emake help

This will list all the commands Emake supports.  To see detailed
description of any of those, type:

    $ emake help COMMAND

In the help you can also see lots of options — both global and
specific to certain commands.  Many common things are possible just
out of the box, but later we will discuss how to define additional
commands and options or change defaults for the existing.

Two most important global options to remember are `--trace` (`-t`) and
`--debug` (`-d`).  With the first one, Emake prints lots of additional
information about what it is doing to stdout.  With the second, Emake
prints stacktraces for most errors.  With these options you will be
often able to figure out what’s going wrong without requesting any
external assistance.

Emake mostly follows GNU conventions in its command line.  Perhaps the
only exception is that global options must be specified before command
name and command-specific options — after it.


## Initializing a project

When Emake starts up, it configures itself for the project in the
directory where it is run from.  This is done by loading Elisp file
called `Emake` (without extension!) in the current directory.  This
file is similar to Make’s `Makefile` or Cask’s `Cask`.  But even more
so to Gradle’s `build.gradle`: because it is a program.  File `Emake`
is not strictly required, but nearly all projects will have one.

You can create the file in your project manually, but it is easier to
just let Emake itself do it for you, especially the first time:

    $ emake init

If you let the initializer do its work, it will create file `Emake`
already prepared to download project dependencies.  If you answer “no”
to its question (or execute as `emake init --non-interactive`), just
edit the created file and uncomment some of the calls to
`emake-use-package-archive` there as appropriate.  These forms
instruct Emake to use specific package archives to download project
dependencies.

After this step, Emake is ready to work with your project.

### Setup procedure in details

Now that we have created file `Emake`, it makes sense to go over the
full startup process:

* Load file `~/.emake/config`
* Load file `Emake` in the current directory
* Load file `Emake-local` in the current directory
* Execute setup forms specified on the command line

None of these Elisp files and forms are required.  They are also not
restricted in what they do.  However, their *intended* usage is
different.

File `~/.emake/config` is *user-specific*.  It is meant mostly for
customizing Emake to your personal preferences.  For example, if you
hate coloring of Emake output, add form `(setf emake-coloring-mode
nil)` to it.  Then every Emake process started for any project will
default to using uncolored output.

File `Emake` is *project-specific*.  It is the only configuration file
that should be added to project’s VCS (Git, Mercurial, etc.).  Typical
usage of this file is to define in which package archives to look up
dependencies.  It is also the place to define project-specific
builders and commands, for example to build project documentation from
source.

File `Emake-local` is *working directory* or *user/project-specific*.
Unlike `Emake`, it *should not* be added to VCS: it is meant to be
created by each developer (should he want to do so) to customize how
Emake behaves in this specific directory.  The most common use is to
define local dependencies.  A good practice is to instruct your VSC to
ignore this file, e.g. list it in `.gitignore` for Git.

Finally, it is possible to specify some (short) setup forms on the
command line using `--setup` (`-S`) option.  This is not supposed to
be used often, mostly in cases where you run Emake on a use-once
project checkout, e.g. on a continuous integration server.


## Project dependencies

Emake picks up project dependencies from package declaration,
i.e. usually from `Package-Requires` header in the project’s main
`.el` file.  You don’t need to declare these dependencies second time
in `Emake` and keep track that they remain in sync.

However, you do need to tell Emake how to *find* these dependencies.
Like Cask, by default it doesn’t use any package archives.  To tell it
to use an archive, call function `emake-use-package-archive` in
`Emake` (you have such forms already in place if you have used `emake
init`).  For example:

    (emake-use-package-archive 'melpa-stable)

Emake knows about three “standard” archives, which should cover most
of your needs: `gnu`, `melpa-stable` and `melpa-unstable`.  Note that
`http://melpa.org` is called `melpa-unstable`.  This is to emphasize
that you shouldn’t use it if `melpa-stable` is enough, because you
wouldn’t want your tests fail only because a dependency in an unstable
version has a bug.

Emacs 25 and up supports package archive priorities.  Emake utilizes
this to assign the standard archives it knows about priorities 300,
200 and 100 in the order they are listed above.

If dependencies for your project are only available from some other
archive, you can still use the same function.  Just substite the
symbolic archive name with a cons cell of name and URL as strings:

    (emake-use-package-archive '("myarchive" . "http://my.archive.com/packages/"))

You don’t need to perform any additional steps to have Emake actually
install the dependencies: any command that needs them will make sure
they are installed first.  However, if you need to check if package
archives have been specified correctly and all dependencies can be
looked up without problems, you can explicitly use command `prepare`.

### Local dependencies

Imagine you are developing more than one project at once and they
depend on each other.  You’d typically want to test the changes you
make in one of them from another right away.  If you are familiar with
Cask, this is solved by linking projects in it.

Emake provides a more flexible approach to this problem called *local
dependencies*.  Let’s assume you develop project `foo` in directory
`~/foo` and also a library called `barlib` in `~/barlib`.  And `foo`
uses the library.  To have Emake use your local copy of `barlib`
instead of downloading it e.g. from Melpa, add the following form in
file `~/foo/Emake-local`:

    (emake-use-local-dependency "~/barlib")

Note that the form *must not* be added to `Emake`: other developers
who check out your project probably don‘t even have a local copy of
`barlib` or maybe have it in some other place.  In other words, this
should really remain your own private setting and go to `Emake-local`.

Local dependencies have *loading modes*, just as the project’s package
itself.  Those will be discussed later.

### Additional dependencies

It is possible to register additional dependencies for use only by
certain Emake commands.  Perhaps the most useful is to make certain
packages available for testing purposes.  For example, if your project
doesn’t depend on package `foo` on its own, but your test files do,
add the following form to `Emake` file:

    (emake-add-extra-dependencies 'test 'foo)

Additional dependencies are looked up in the same way as normal ones.
So, you need to make sure that all of them are available from the
package archives you instructed Emake to use.

The following commands make use of additional dependencies: `build`,
`emacs`, `eval`, `exec` and `test`.  Commands you define yourself can
also take advantage of this mechanism, see function
`emake-load-project-dependencies`.

### Examining dependencies

Sometimes it is useful to check what a project depends on, especially
if it is not your project, just something you have checked out.  There
are two commands for this in Emake.

First is `dependencies` (can be shortened to `deps`).  It lists
*direct* dependencies of the project being built.  By default, it
omits any built-in packages, most importantly `emacs`.  If you want to
check those too, add option `-b` (`--list-built-ins`).

Second is `dependecy-tree` (short alias: `dtree`).  It prints a tree
of project direct dependencies, direct dependencies of those, and so
on — recursively.  Like with the first command, use option `-b` if you
want to see built-ins in the tree.

Both commands can also list additional dependencies if instructed:
just specify set name(s) on the command line, e.g.:

    $ emake dependencies test

You can also check which archives Emake uses to look up dependencies
for this particular project with the following command:

    $ emake archives

### Upgrading dependencies

Emake will install project dependencies automatically, but it will
never upgrade them, at least if you don’t change your project to
require a newer version.  However, you can always explicitly ask Emake
to upgrade the installed dependencies:

    $ emake upgrade

First, package archive contents will be refetched, so that Emake knows
about newly available versions.  Next, this command upgrades (or
installs, if necessary) all project dependencies and all additional
dependencies you might have registered (see above).  If you don’t want
to upgrade everything, you can explicitly list names of the packages
that should be upgraded:

    $ emake upgrade dash ht

You can also check what Emake would upgrade without actually upgrading
anything:

    $ emake upgrade --dry-run


## Loading modes

In Emake the project’s package and its local dependencies have
*loading modes*.  This affects exactly how the package (that of the
project or of its local dependency) becomes loadable by Emacs.

Default loading mode is called `as-is`.  It means the directory where
project (or local dependency) is located is simply added to Emacs
varible `load-path` and normal Emacs loading should be able to find
required features from there on.  This is the fastest mode, since it
requires no preparation and in most cases is basically what you want
during development.

However, users won’t have your project loaded like that.  To emulate
the way that most of the people will use it, you can use loading mode
`packaged`.  In this mode, Emake will first package your project (or
local dependency), then install and activate it using Emacs’ packaging
system.  This is quite a bit slower than `as-is`, because it involves
several preparation steps.  However, this is (almost) exactly the way
normal users will use your project after e.g. installing it from
Melpa.  For this reason, this mode is recommended for continuous
integration and other forms of automated testing.

Other modes include `byte-compiled` and `source`.  In these modes
loading is performed just as in `as-is` mode, but before that Emake
either byte-compiles everything or, vice-versa, removes `.elc` files.

So, after discussing the loading modes, let’s have a look at how
exactly you tell Emake which one to use.

For the project itself, this is done from the command line using
global option `--loading` (or `-m`) with its argument being the name
of the mode.  Since this is supposed to be used quite frequently,
there are also shortcut options to select specific modes: `--as-is`
(or `-a`), `--packaged` (`-p`), `--source` (`-s`) or `--byte-compiled`
(`-b`).  For example, the following command will run unit-tests in the
project, having it loaded as a Emacs package:

    $ emake -p test

Remember, that as everything in Emake, this can be customized.
E.g. if you want to run your project byte-compiled by default, add
this to your `Emake-local`:

    (setf emake-project-loading-mode 'byte-compiled)

For local dependencies the mode can be chosen when calling
`emake-use-local-dependency`.  For example:

    (emake-use-local-dependency "~/barlib" 'packaged)

As mentioned above, loading mode defaults to `as-is`.

There a few other loading modes useful only for certain packages.  You
can always ask Emake for a full list:

    $ emake --list-modes


## Build system

Emake comes with quite a sofisticated build system.  While by default
it only knows how to build packages, byte-compile `.el` files and make
`.info` from `.texi`, you can extend it with custom *builders* that
can do anything you want.  For example, generate resource files that
should be included in the final package.

### Targets

Build system is based on *targets*.  Targets come in two kinds: *real*
and *virtual*.  First type of targets corresponds to files — not
necessarily existing, but they will be generated when such targets are
built.  Targets of the second type always have names that begin with
“:” (like keywords in Elisp).  Most import virtual target is called
`:default` — this is what Emake will build if you don’t request
anything explicitly.

FIXME

#### Target sets

Emake groups all targets into *sets*.  Normally, there are only two
sets called `main` and `test`, but you can define more if you need
(see variable `emake-filesets').

FIXME

### Cleaning

FIXME

### Building packages

FIXME

### Byte-compiling

FIXME


## Testing

Emake has built-in support for running regression/unit tests of your
project.  Currently, Emake supports only ERT.  Other frameworks will
also be supported in the future; leave a feature request in the issue
tracker if you are interested.

Simply executing

    $ emake test

will run all your tests.  By default, all tests are expected to be in
files named `test.el`, `tests.el`, `*-test.el`, `*-tests.el` or in
`test` or `tests` subdirectories of the project root.  But you can
always change the value of `emake-test-fileset` variable in the
project’s `Emake` as appropriate.

By default, the command runs all available tests.  However, during
development you often need to run one or a few tests only — when you
hunt a specific bug, for example.  Emake provides two ways to select
which tests to run.

First is by using a *selector*:

    $ emake test foo-test-15

will run only the test with that specific name.  It is of course
possible to select more than one test by specifying multiple
selectors: they are combined with ‘or’ operation.  You can use any
selector supported by the testing framework here, see its (i.e. read:
“ERT’s”) documentation.

The second way is to avoid loading (and executing) certain test files
altogether.  This can be achieved with `--file` (`-f`) option:

    $ emake test -f foo.el

will execute tests only in file `foo.el` and not in e.g. `bar.el`.
You don’t need to specify directory (e.g. `test/foo.el`); for reasons
why, see explanation of Emake filesets below.

Both ways of selecting tests can be used together.  In this case they
are combined with ‘and’ operation: only tests that match selector and
which are defined in a loaded file are run.

How exactly tests are executed depends on *test runner*.  If you
dislike the default behavior of Emake, you can choose a different test
runner using `--runner` (`-r`) option of `test` command; see the list
of available test runners with their descriptions using
`--list-runners` option.  If you always use a different test runner,
it is a good idea to set it as the default in file `~/.emake/config`.
Finally, you can even write your own runner.

### Reusing previous test results

ERT provides a few selectors that operate on tests’ last results.
Even though different Emake executions will run in different Emacs
processes, you can still use these selectors: Emake stores and then
loads last results of test execution as needed.

For example, execute all tests until some fails (`-s` is a shortcut
for `--stop-on-unexpected`):

    $ emake test -s

If any fails, you might want to fix it and rerun again, to see if the
fix helped.  The easiest way is:

    $ emake test :failed

For more information, see documentation on ERT selectors — other
“special” selectors (e.g. `:new` or `:unexpected`) also work.

### Testing command line simplifications

When variable `emake-test-dwim` (“do what I mean”) is non-nil (as by
default), Emake supports a few simplifications of the command line to
make testing even more streamlined.

* Any selector that ends in `.el` is instead treated as a file
  pattern.  For example:

      $ emake test foo.el

  will work as if you specified `-f` before `foo.el`.

* For ERT: any symbol selector that doesn’t match a test name is
  instead treated as regular expression (i.e. as a string).  For
  example:

      $ emake test foo

  will run all tests with names that contain `foo`.  You can achieve
  the same result with ‘strict’ command line (see also ERT selector
  documentation) like this:

      $ emake test \"foo\"

If you dislike these simplifications, set `emake-test-dwim` to nil in
`~/.emake/config`.


## Quickly evaluating expressions

It is often useful to evaluate Elisp expressions in context of the
project you develop — and probably using functions from the project.
There are two commands for this in Emake: `eval` and `exec`.  The only
difference between them is that `exec` doesn’t print results to
stdout, i.e. it assumes that the forms you evaluate produce some
detectable side-effects.  Because of this similarity, we’ll consider
only `eval` here.

The basic usage should be obvious:

    $ emake eval "(+ 1 2)"

Of course, evaluating `(+ 1 2)` form is not terribly useful.  Usually
you’ll want to use at least one function or variable from the project.
However, for that you need your project not only to be in `load-path`
(which Emake guarantees), but also `require`d.  Luckily, you don’t
have to repeat `(require 'my-package)` all the time on the command
line, as Emake does this too, so normally you can just run it like
this:

    $ emake eval "(my-package-function)"

What Emake actually does is requiring all features listed in variable
`emake-eval-required-features`.  If value of that variable is symbol
`:default`, value of `emake-default-required-features` is taken
instead.  And finally, when value of the latter is symbol
`:project-name`, only one feature with the same name as that of the
project is required.  In 95% of the cases this is exactly what you
need.  However, if the main feature of the project has a different
name, you can always change the value of one of the mentioned
variables in file `Emake`.

It can also make sense to change the variable’s value in `Emake-local`
if you want certain features to always be available for quick testing.


## Running Emacs

Sometimes you want to run Emacs with *just* your project installed and
see how it works without any customization.  You can achieve this in
Emake easily:

    $ emake emacs

This will spawn a separate Emacs that doesn’t read any initialization
scripts and doesn’t have access to your usual set of installed
packages, but instead has access to the project being built with Emake
— and its dependencies, of course.  Similar as with `eval` and `exec`
commands, features listed in variable `emake-emacs-required-features`
are required automatically.

You can also pass any Emacs options through the command line.  For
example, this will visit file `foo.bar`, which is useful if your
project is a mode for `.bar` files:

    $ emake emacs foo.bar

See `emacs --help` for what you can specify on the command line.

When issued as shown above, command `emacs` will pass the rest of the
command line to Emacs, but also add a few things on its own.  First,
it adds everything from the list `emake-emacs-default-command-line`,
which disables `~/.emacs` loading and similar things.  Second, it adds
`--eval` arguments to require the features as described above.  And
only after that comes the actual command line you specified.

Occasionally you might not want this behavior.  In this case, prepend
`--` to the command line — then Emake will pass everything after it to
the spawned Emacs as-is.  Remember that you will likely need to pass
at least `-q` (`--no-init-file`) option to Emacs, otherwise it will
probably fail on your `~/.emacs` since it will not see your usual
packages.  To illustrate:

    $ emake emacs -- -q foo.bar


## Executing on different Emacs versions

Since Emake itself is a Elisp program, version of Emacs you use can
affect any aspect of execution — even before it gets to running
something out of your project.  Therefore, inside its “cache”
directory called `.emake`, the utility creates a subdirectory named
after Emacs version it is executed on.  If it is run with a different
Emacs, it will not use dependencies or previous test results, but
rather install or recompute them from scratch.

Normally, Emake uses command `emacs` that is supposed to be resolvable
through `PATH` environment variable.  However, you can always tell it
to use a different Emacs version by setting either `EMAKE_EMACS` or
just `EMACS` in the environment, e.g.:

    $ EMACS=emacs25 emake eval emacs-version

This is especially useful for testing your project with different
Emacs versions.

Remember, however, that Emake cannot separate byte-compiled files
(`.elc`) from sources.  From documentation of
`byte-compile-dest-file-function':

    Note that the assumption that the source and compiled files are
    found in the same directory is hard-coded in various places in
    Emacs.

Therefore, if you use byte-compilation and switch Emacs versions,
don’t forget to clean the directory.


## Filesets

Filesets are lists of rules that determine a collection of files
inside given root directory, usually the project directory.  Similar
concepts are present in most build systems, version control systems
and some other programs.  Filesets in Emake are inspired by Git.

Important examples of filesets are variables `emake-main-fileset`,
`emake-test-fileset` and `emake-standard-excludes`.  Default values of
all three are *simple filesets*, but are not actually restricted to
those: when customizing for your project you can use any valid fileset
as a value for any of these variables.  However, for most cases simple
filesets are all that you really need.

### Simple filesets

From Lisp point of view, a simple fileset is a list of strings.  A
single-string list can also be replaced with that string.  The most
important filesets are `emake-main-fileset` and `emake-test-fileset`.
Using them you can define which `.el` files are to be packaged and
which contain tests.  Default values should be good enough for most
projects, but you can always change them in file `Emake` if needed.

Each rule is a string that matches file path — or its part — relative
to the root directory.  Path elements must be separated with a slash
(`/`) regardless of your OS, to be machine-independent.  A rule may
contain glob wildcards (`*` and `?`) with the usual meaning and also
double-star wildcard (`**`) that must be its own path element.  It
stands for any number (including zero) of nested subdirectories.
Example:

    foo/**/bar-*.el

matches `foo/bar-1.el` and `foo/x/y/bar-baz.el`.

If a rule starts with an exclamation mark (`!`), it is an *exclusion*
rule.  Files that match it (after the mark is stripped) are excluded
from the result.  Other (“normal”) rules are called *inclusion* rules.

Typically, a rule must match any part of a file path (below the root,
of course).  However, if a rule starts with `/` or `./` it is called
*anchored* and must match beginning of a file path.  For example, rule
`./README` matches file `README` in the root directory, but not in any
of its subdirectories.

If a rule matches a directory, it also matches all of the files the
directory contains (with arbitrary nesting level).  For example, rule
`test` also matches file `test/foo/bar.el`.

A rule that ends in a slash directly matches only directories.  But,
in accordance to the previous paragraph, also all files within such
directories.  So, there is a subtle difference: a rule `test/` won’t
match a file named `test`, but will match any file within a directory
named `test`.

Finally, note a difference with Git concerning inclusions/exclusions
and subdirectories.  Git manual says: *“It is not possible to
re-include a file if a parent directory of that file is excluded.”*
Emake filesets have no such exceptions.

### Composite filesets

Emake also supports composite filesets.  They are built using common
set/logic operations and can be nested, i.e. one composite fileset can
include another.  There are currently three types:

* `(:and ELEMENT...)`

  A file matches an `:and` fileset if and only if it matches *every*
  of its `ELEMENT` filesets.

* `(:or ELEMENT...)`

  A file matches an `:or` fileset if and only if it matches *at least
  one* of its `ELEMENT` filesets.

* `(:not NEGATED)`

  A file matches a `:not` fileset when it *doesn’t match* its
  `NEGATED` fileset and vice versa.

### Evaluated filesets

Finally, some parts of filesets — but not elements of simple filesets!
— can be evaluated.  An evaluated element can be a variable name (a
symbol) or a form.  When matching, such element will be evaluated
*once*, before `emake-find-files` or `emake-filter-files` start actual
work.

Result of evaluating such an expression can be an evaluated fileset in
turn — Emake will keep evaluating elements until results finally
consist of only simple and composite filesets.  To prevent accidental
infinite loops, there is a limit of `emake-fileset-max-iterations` on
how many times sequential evaluations can yield symbols or forms.

Example of an evaluated fileset can be seen from return value of
`emake-standard-fileset` function.  E.g.:

    (emake-standard-fileset 'main)
    => (:and emake-main-fileset (:not emake-standard-excludes))

As the result contains references to two variables, they will be
evaluated in turn — and so on, until everything is resolved.


## Extending Emake

FIXME

### Hooks

FIXME

### Adding commands

FIXME

### Adding command and global options

FIXME
