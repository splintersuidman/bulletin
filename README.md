# Bulletin

Automate assembling regular bulletins with contributions from various sources using various formats.

## Usage

A bulletin specification is a TOML file looking something like this:
```toml
title = "Bulletin 0"
date = 1970-01-01
file = "Bulletin-0.pdf"
template = "template.typ"

[extra]
deadline = "1 January 2000"

[[contribution]]
author = "John Doe"
title = "My first bulletin contribution"
date = 1969-07-20
file = "./bulletin0/John.docx"

[[contribution]]
author = "Jane Doe"
title = "A Markdown contribution"
date = 1969-12-31
file = "./bulletin0/Jane Doe - Markdown.md"
```

The top-level `title` and `date` variables specify the title and date of the bulletin.
The `file` variable specifies the output file to which the compiled bulletin is written.
The `template` variable specifies the Pandoc template file to use.

In the `extra` table, extra variables can be specified that will be passed to the template compiler.
In the above example, the value of the `deadline` variable can be used in the template as `$extra.deadline$`.

The rest of the file specifies two contributions.
Each has an author, title and date.
The `file` variable specifies the contribution's file.
Currently, the supported file formats are: Word (`.docx`), OpenDocument text (`.odt`), Markdown (`.md`), and Typst (`.typ`).

## Build instructions

Currently, the only supported build system is Nix.
You can build or run the program by cloning the Git repository and running `nix build` or `nix run`.
You can also build or run the program as follows:
```sh
nix build github:splintersuidman/bulletin
nix run github:splintersuidman/bulletin
```
