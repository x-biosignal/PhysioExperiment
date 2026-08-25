# Security Policy

## Supported versions

The Physio ecosystem is developed as a set of coordinated R packages. Security
fixes are applied to the **latest released version** of each affected package on
the [x-biosignal r-universe](https://x-biosignal.r-universe.dev). Older versions
are not maintained; please update before reporting.

## Reporting a vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, report them privately by email to:

**mail.to.matsui@gmail.com**

Please include, as far as you can:

- the affected package(s) and version(s),
- a description of the vulnerability and its potential impact,
- steps to reproduce (a minimal example is ideal), and
- any suggested mitigation.

## What to expect

- **Acknowledgement:** we aim to acknowledge your report within 5 working days.
- **Assessment:** we will investigate and keep you informed of progress.
- **Fix and disclosure:** once a fix is available, we will release it and, with
  your agreement, credit your report in the release notes. We prefer coordinated
  disclosure and ask that you give us reasonable time to release a fix before any
  public disclosure.

## Scope

These packages process scientific/physiological signal data and can read a range
of file formats. Reports of particular interest include unsafe handling of
untrusted input files, unsafe deserialization, code execution via crafted
inputs, and path-traversal or resource-exhaustion issues in I/O routines.
