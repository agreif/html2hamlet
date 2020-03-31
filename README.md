# html2hamlet

HTML to Hamlet converter

### Prerequisites:
You must have `git` installed
You must have haskell installed.

> **Tip:** Using Stack to install Haskell is recommended

## How to use it:

Download the repository with:
`git clone https://github.com/tanakh/html2hamlet`

Go into the downloaded code directory:
`cd html2hamlet`

### The cabal way

Build all the dependencies:
`cabal new-build`

Run the tool:
`cabal new-run html2hamlet test.html`

The file `test.hamlet` will be produced.

### The stack way

Build and install all the dependencies:
`stack build html2hamlet`
`stack install html2hamlet`

Run the tool:
`html2hamlet test.html`

The file `test.hamlet` will be produced.
