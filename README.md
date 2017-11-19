# lucid
The λucid typesetting system.

## Based on TeX
This way we aren't reinventing the wheel.  We get mathematical typesetting for free, etc.

## Has a Linker
This is a big one.  It allows you to use multiple files to create documents, and to reuse files in multiple documents easily.  Each document can be self contained and complete. (ie will compile on its own to a document).

### Format of Input Files
The format of input files consists of a header called the preamble where all definitions and declarations are made.  Then comes the body where the actual text to be typeset lives.  All of the global styling information lives in the preamble.  This makes it possible to merge two preambles when compiling a multi-file project, and to rationally override the parent documents' style, or defer to them.  Styling can be held in a separate file as well, and included in the same way.  A style file is just a file with an empty body.

## Has Namespacing
Identifiers live in packages which act as namespaces.

