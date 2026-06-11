# Security Policy

## Reporting a vulnerability

Please report suspected security issues privately through the repository's
GitHub security reporting flow if available, or by opening a GitHub issue that
asks for a private contact path without publishing exploit details.

Include:

- affected API surface
- minimal input that demonstrates the issue
- expected behavior
- observed behavior
- version or commit tested

## Scope

Security-sensitive issues include:

- crashes or uncaught exceptions reachable from public APIs
- excessive CPU or memory behavior on crafted input
- acceptance of inputs that should be rejected by the documented IDNA/UTS #46
  behavior
- generated-table or Unicode-version mismatches that affect runtime decisions

Diagnostics wording differences are normally not security issues unless they
hide or contradict the runtime decision.

## Verification

Maintainers should verify fixes with:

```sh
dune build @runtest --force
dune build @test/evidence --force
dune build @test/conformance --force
dune build @test/bench --force
```

The evidence, conformance, and benchmark gates require the local Unicode data
files.
