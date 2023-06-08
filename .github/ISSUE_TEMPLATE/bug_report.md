---
name: Bug report
about: Report a bug in Eldev you've discovered
---

Before creating a bug report, please do the following:

* Upgrade to the latest version ($ eldev upgrade-self) and see if the bug is still present.

* Run Eldev with global options -dt; this may give you enough information to understand what's going on and, sometimes, uncover that the bug is actually not in Eldev itself.

* Run Eldev's Doctor ($ eldev doctor) and study its suggestions.

* Check if a similar issue has already been reported here.

**Please remove this line and everything above it in your final report!**


**Describe the bug**

A clear and concise description of what the bug is.

**Steps to reproduce**

1. Download the project from ...
2. In its directory, execute `eldev ...`
3. See error

**Expected behavior**

It may be not obvious what is expected for someone unfamiliar with your project, so please write this explicitly.

**Actual behavior**

This might be machine-specific, so please write explicitly what *you* see.

**Environment**

- Eldev: [e.g. 1.4.1; run `eldev version` to find it]
- Emacs: [e.g. 28.2; run `eldev version emacs` or `eldev eval "(emacs-version)"` to find out]
- OS: [e.g. Linux, macOS, Windows etc.]

**Additional context**

Any additional information you think might be relevant.  In particular, a link to your project if not specified above.
