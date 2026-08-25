# Deprecation & Lifecycle Policy

The Physio ecosystem communicates the stability of its public API using the
[lifecycle](https://lifecycle.r-lib.org/) conventions and badges. This document
defines what each stage means and the process (and minimum window) for retiring
functionality.

## Lifecycle stages

We use the standard `lifecycle` stages:

| Stage | Meaning |
|-------|---------|
| **experimental** | New; the interface may change without a deprecation cycle while we learn the right design. Safe to try, not yet safe to depend on. |
| **stable** | The interface is settled. Breaking changes go through a full deprecation cycle. |
| **superseded** | A newer preferred alternative exists; the function is not going away but will only receive critical fixes. |
| **deprecated** | Scheduled for removal; still works but warns. A replacement is documented. |

Each stage renders as a [lifecycle badge](https://lifecycle.r-lib.org/articles/stages.html)
in the package documentation (added to a function's roxygen `@description` via
`` `r lifecycle::badge("experimental")` ``).

Once removed, calling a function raises a "defunct" error (via
`lifecycle::deprecate_stop()`) for at least one further release before the
function is deleted entirely.

## Deprecation process

When a stable function or argument must be retired:

1. **Announce** — mark it `deprecated` (badge in the roxygen `@description`) and
   emit a warning at call time using the ecosystem helper
   `PhysioCore::deprecate_physio()` (a thin wrapper over
   `lifecycle::deprecate_warn()` / `deprecate_stop()`). The warning names the
   version, the removed item, and the replacement.
2. **Document** — record the change in the package `NEWS.md`, and point users to
   the replacement with `@seealso` and the `with =` argument of the helper.
3. **Warn window** — keep the deprecated item working (warning only) for a
   **minimum of two minor releases** (or six months, whichever is longer).
4. **Defunct** — after the warn window, switch the item to error (`deprecate_stop`)
   for at least one release.
5. **Remove** — delete the item in a subsequent release, noted in `NEWS.md`.

Breaking changes are only introduced through this process; experimental items are
exempt (they may change directly, but such changes are still noted in `NEWS.md`).

## Using the helper

```r
old_function <- function(x) {
  PhysioCore::deprecate_physio(
    when = "0.3.0",
    what = "old_function()",
    with = "new_function()"
  )
  new_function(x)
}
```

Set `severity = "stop"` to raise a defunct error instead of a warning once the
warn window has elapsed.

## Versioning

Packages follow semantic-versioning intent: patch releases fix bugs, minor
releases add backward-compatible features (and start deprecation cycles), and a
major release may complete removals announced through the process above.
