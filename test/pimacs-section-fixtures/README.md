# Section folding fixtures

Each `.txt` file contains configuration followed by `==========` and the
expected visible buffer. `[-]` means expanded; `[+]` means collapsed. Hidden
body text and descendants are omitted.

Autohide counts the newest eligible leaves across the entire buffer.
A leaf has no children; parents do not consume slots. Leaves selected by
the initial-hide filter never count, even when manually expanded.
Retained leaves keep their ancestors from collapsing because of age, while
initial and manual folding settings still apply. Excluded sections and their
ancestors stay expanded unless manually collapsed. Excluding a parent does
not exclude its children.

Fixtures use the shared Build / Logs / Deploy demo buffer by default. They
start with fresh settings and point outside the sections. Tests that need
tool metadata select a named buffer with `pimacs-section-fixture-buffer`.

A file can repeat the config / buffer pair. Later configurations apply to
the same buffer, preserving earlier folding choices and content updates.
Unchanged settings carry over to the next step.

Additional fixture settings:

- `pimacs-section-fixture-point`: a section path for point-protection cases.
- `pimacs-section-fixture-visibility`: section paths paired with visibility
  states, such as `'(((build) . :hide))`.
- `pimacs-section-fixture-result`: add a result beneath Unit Tests.
- `pimacs-section-fixture-messages`: append top-level message types.
- `pimacs-section-fixture-append`: section paths paired with text to append.
- `pimacs-section-fixture-replace`: section path, tool name, and replacement
  body for a tool result.

Paths use section types in the demo buffer, or heading strings in tool
buffers. Content and visibility actions apply once per configuration block.

The runner also checks that manual visibility states are preserved,
predicates visit each section once per reconciliation, initial filters are
not reapplied to existing sections, and repeating reconciliation makes no
visibility changes.

Run these fixtures with:

```sh
make test MATCH=pimacs-section-autohide-fixtures
```
