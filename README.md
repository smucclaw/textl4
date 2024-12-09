# λ4

A legal DSL that is, at its core, a functional expression language, but that also aims, in the long run, to

* incorporate solver / automated reasoning capabilities, so that you can ask questions like, what needs to happen for me to be able to make this insurance claim (probably coming soon)

* offer a 'low-code', intuitive GUI with an AI (+ PL / program synthesis / formal methods methods)-powered Copilot (on the long-term roadmap)

## How to build

This will be simpler soon enough, once I've set up a Nix environment etc. But in the meantime:

You basically have to set up two things:

1. The Langium frontend and VSCode extension: see the detailed instructions in the "To build / generate the Langium frontend parser" and "To build the VSCode extension" sections below

2. The Haskell codebase: Use GHC 9.6.6 and `cabal`.

When building the Haskell codebase, if the `Grisette` dependencies complains about not having Z3 on your PATH, you may need to add it to your PATH.

Before running the VSCode extension, make sure to have an `.env` that looks like the following in your root `lam4` dir

```.env
REMOTE_DECISION_SERVICE_URL=<url --- use an empty string if you are not from CCLAW>
UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS=<use "update" if you want it to update remote decision service with new program on save; use "no_update" if not>
DECISION_SERVICE_REQUEST_MAKER_CMD=<for CCLAW: use "l4-oia">
DEMO_OIA_DATA_MODEL_XML_PATH=<for CCLAW: use path to projectDataModel.xml>
COMPILED_SIMALA_OUTPUT_DIR="generated/simala"
NLG_EN_OUTPUT_DIR="generated/nlg_en"
NLG_EN_OUTPUT_FILENAME="nlg_en_output.json"
GF_PORTABLE_GRAMMAR_FORMAT_FILENAME="Lam4.pgf"
```

## How to compile Lam4 files

Use the `lam4-cli`.

EG, from the root `lam4` dir:

> `cabal run lam4-cli -- examples/arithmetic.l4`

If you want to use the VSCode extension, make sure to `cabal install lam4-cli --overwrite-policy=always` as well.

## What is Lam4?

### Vs. main branch L4

This is a kind of rational reconstruction of the 'MathLang' dialect of L4. That is, it is, at its core, a functional expression language; but it aims to extend the MathLang work by incorporating solver / automated reasoning capabilities. It also differs from Natural L4 in offering a textual format with a more constrained surface syntax (with a 'no-code,' intuitive, AI-augmented GUI for non-programmers on the roadmap).

### Goals and Principles, in the mid to longer term

The goal is to get something that

* is more *modular*, and hence *maintainable*, with less technical debt, to enable rapid prototyping and iteration
* is more *extensible* -- e.g., with things like metadata
* has a *textual* format (potentially in addition to a visual one, if time/energy permits), and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)
* de-emphasizes complicated parsing; aims to shifts more of the burden instead to UI/UX, AI, program synthesis, etc
* emphasizes *usability*, *ease of learning*, and *ergonomics*, in particular, support for *tooling* and other modern conveniences to make working with L4 less onerous and more intuitive. This is worth emphasizing: the current discourse on L4 usability tends to conflate (i) how easy / intuitive it is to learn it with (ii) how approachable it looks on the surface. In particular, the current approach seems to be to try to disguise the complexity inherent in formalizing law, in a way that can be misleading. By contrast, the Lam4 approach aims to avoid misleading simplification, and instead tries to engender genuine understanding.
* allows you not only to *evaluate expressions* (via a 'MathLang'-esque expression sub-language), but also to deploy *formal-methods-powered techniques* (e.g., to explore or understand the contract better, to check that your formalization of the contract does indeed, or even to synthesize or repair L4 specifications).
* has natural language rendering; see [documentation on GF grammar](lam4-backend/gf-grammar/README.md) for details.

## Understanding the grammar / working with it

If you don't need to use record field access or predicate declarations, you can paste the grammar into the playground (https://langium.org/playground/) to see the generated parser in action.

### To build / generate the Langium frontend parser

First install the dependencies.

```bash
cd lam4-frontend
npm install
```

Then build the frontend, again from the `lam4-frontend` dir:

```bash
npm run langium:generate
npm run build
```

You must re-build the frontend whenever the Langium grammar or lam4-frontend code changes.

You can also

* Run `npm run watch` to have the TypeScript compiler run automatically after every change of the source files.

* Run `npm run langium:watch` to have the Langium generator run automatically after every change of the grammar declaration.


### To build the VSCode extension

To **generate and install a VSCode extension**, after you have `npm install`-ed:

* at the command line (the first `rm` instruction tries to remove previously-generated Lam4 extensions), from `lam4-frontend`: `rm lam4-*.vsix; npx vsce package; code --install-extension lam4-*.vsix`

* You can also try pressing `F5` from within VSCode

To remove a VSCode extension at the command line: `code --list-extensions` to list extensions and get the name of the lam4 extension, then `code --uninstall-extension <name of the lam4 extension>`

When things update...

* You can update your VSCode extension from the command line using the commands mentioned earlier. Yongming has a personal script for this that can be adapted (`yongminghan` is the publisher). If the changes don't show up, try restarting the extension host from VSCode:

```bash
code --uninstall-extension yongminghan.lam4;
rm lam4-0.0.1.vsix;
vsce package;
code --install-extension lam4*.vsix
```

* You can relaunch the extension from the debug toolbar.

* You can also reload (`Ctrl+R` or `Cmd+R` on Mac) the VS Code window with your extension to load your changes.


### Working with the Langium grammar

After building:

* To see what the surface Langium grammar looks like, you can **generate a railroad diagram from VSCode**, using the Langium VSCode extension.
* To print to std out and save to disk a **human-readable version of the surface Langium grammar syntax** (i.e., a version without the metadata): from `lam4-frontend`, do `node ./bin/cli toMinimalAst <lam4program filename>.l4`
* To do that for a version that includes source text metadata: `node ./bin/cli toAstWithSrcMetadata <lam4program filename>.l4`.

## Noteworthy TODOs

### Backlog

#### Typechecker, evaluator, backend related

The type checker is currently implemented in Typescript, but in the medium term I want to just move that to the Haskell backend.

I also want to have some kind of JSON RPC going with the Haskell backend, so that we'll be able to, e.g., parse the program, run the evaluator, and see nice feedback in a webview, all within the IDE.


#### Parser

1. Try cutting down on symbols like braces if this is something that would confuse non-technical users

When time permits -- e.g., shortly after the first end-to-end system is done --- we should try cutting down on symbols like braces if that will confuse non-professional programmers. EG, we could instead go for something that uses indentation.

This is already possible with Langium; I just haven't done it because it requires a bit more effort and would complicate the codebase more than I would like at this stage.
