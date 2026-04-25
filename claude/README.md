# claude — stow package

User-level Claude Code agent definitions. Stowed into `~/.claude/agents/`.

## Install

```
cd ~/.dotfiles && stow claude
```

## Agents

- `code-reviewer` — post-edit review for correctness, security, perf, maintainability. Read-only.
- `rust-perf-reviewer` — Rust-specific perf review focused on hot paths. Read-only.
- `test-writer` — writes unit / integration / property-based tests. Has Edit/Write access.
