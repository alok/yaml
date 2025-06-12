# Agents Guide

This repository uses AI agents (LLMs) to assist with porting the Haskell `yaml` library to Lean 4.

## Purpose
1. **Automated Planning & Execution** – break the porting effort into atomic, verifiable tasks.
2. **Documentation & Memory** – persist design decisions and context for future agent runs.
3. **Repeatability** – keep a single source of truth (`agents.md`) and link it under multiple common names so different tooling can discover it easily.

## Key Files & Links
- `agents.md` – this document. **Canonical**.
- `llms.txt` – symlink → `agents.md` (legacy filename for some tools).
- `agent.md` – symlink → `agents.md` (short alias).
- `.cursorrules` – high-level goals and progress checklist. See the reference line inside that file for a quick jump to this guide.

## Workflow for Agents
1. Read `.cursorrules` to understand the immediate goal list.
2. Consult `agents.md` (this file) for detailed instructions & architecture decisions.
3. Make atomic commits on focused changes. Use Git worktrees for parallel branches.
4. Update `CHANGELOG.md` and progress checklists after each merge.

## Porting Roadmap Snapshot (2025-06-11)
*(See current issues/PRs for live status)*
- Phase 0: Scaffolding – ✅ cloned Haskell repo as worktree.
- Phase 1: Data model & core types – ⏳ in progress.
  • Basic `Value`, `Event`, `ParseException` implemented.
- Phase 2: Parser/emitter FFI – 🔜.

### Recent progress (2025-06-12)
• Lean side emitter (`objToEvents`, `objToStream`) implemented.  
• Minimal recursive-descent parser with scalar classification (null/boolean/number) added.  
• Unit-test executable `leanyaml_tests` performs round-trip checks and is wired into `lake`.

Checklist additions

- [x] Basic scalar classification (bool/null/number) in parser
- [x] Round-trip smoke tests (`Test/Main.lean`)

Upcoming
* Extend numeric parsing to exponent/scientific once `Scientific` util is available in stdlib.
* Anchor/alias handling.

## Contact
Ping @alokbeniwal for clarifications or major architectural decisions.

Port progress checklist (2025-06-11):
[ ] Data.Yaml.Internal – value types and exceptions (stubs) ✅
[ ] Parsing machinery
[ ] Warning logic
[ ] StringStyle helpers
[ ] Encoder objToEvents
[ ] Event model ✅ 