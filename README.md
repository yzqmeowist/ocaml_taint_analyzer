# OCaml Taint Analyzer for JavaScript

## Overview

The project is a static taint analysis tool written in OCaml. Its goal is to automatically detect potential **DOM-based XSS (Cross-Site Scripting)** vulnerabilities in JavaScript/TypeScript projects.

The system is initially designed as the following pipeline with four stages, with clear boundaries
between external tools and the OCaml program.

1. Stage 1 - Pre-processing: The target project in TypeScript is compiled to JavaScript
   by `tsc` and then bundled using `esbuild`, which resolves all local import statements and
   transform the problem between files into a simpler one in a single file.
2. Stage 2 - Parsing: A mature JavaScript parser reads the bundled file and produces a
   complete, concrete JSON AST. The parsing process or commands should be integrated
   into the OCaml code if necessary.
3. Stage 3 - Transformation: The OCaml program ingests the JSON and recursively walks
   the JSON tree. It will transform only the nodes corresponding to my minimal subset ADT
   and maps all others to a placeholder which will not interference the analysis.
4. Stage 4: Analysis: The analysis engine written in OCaml traverses the lightweight ADT,
   tracks variables tainted by known sources and flags any flow into a known sink

## Prerequisites

Please ensure that you have installed the following toolchains:

1. **Node.js & npm:**

   ```
   node -v
   npm -v
   ```
2. **OCaml & Dune:**

   ```
   # macOS
   brew install opam
   # Linux
   sudo apt install opam

   opam init
   eval $(opam env)
   opam install dune
   ```
3. **OCaml dependencies:**

   ```
   opam install yojson
   ```

## Quick Start

1. In the project root directory, install Node.js dependencies (for parser and bundler):

```
npm install
```

2. Enter `analyzer` directory and build the project:

```
cd analyzer
dune build
```

3. execute the analyzer and it will analyze the files under `../target-project/app`:

```
dune exec taint_analyzer
```

## Expected Output

```
Starting Taint Analysis...    
Target Directory: ../target-project/app/
Found 1 files to analyze.

Analyzing file: ../target-project/app/page.tsx
---------------------------------------------------
ast.json generated with source maps embedded.
FOUND 1 VULNERABILITIES!

===================================================
FINAL REPORT
===================================================
Total Vulnerable Files: 1

File: ../target-project/app/page.tsx
     Sink: innerHTML
     Message: Vulnerability at ../target-project/app/page.tsx:17

Analysis Complete!
```
