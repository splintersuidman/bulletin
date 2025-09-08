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
author = "@splintersuidman"
title = "Bulletin"
date = 2025-08-27
url = "https://raw.githubusercontent.com/splintersuidman/bulletin/refs/heads/master/README.md"
format = "markdown"

[[contribution]]
author = "Jane Doe"
title = "A Word contribution"
date = 1969-12-31
file = "./bulletin0/Jane Doe.docx"
```

The top-level `title` and `date` variables specify the title and date of the bulletin.
The `file` variable specifies the output file to which the compiled bulletin is written.
The `template` variable specifies the Pandoc template file to use.

In the `extra` table, extra variables can be specified that will be passed to the template compiler.
In the above example, the value of the `deadline` variable can be used in the template as `$extra.deadline$`.

The rest of the file specifies two contributions.
Each has an author, title and date.
The source of the contribution is specified by either (1) a file path in the `file` variable, (2) a URL in the `url` variable, or (3) a Google Docs document ID in the `google-docs` variable.
Optionally, the source format is specified by the `format` variable; otherwise, the format is inferred based on the file extension (and in the case of a URL, based on the file extension at the end of the URL if there is any, *not* on the response MIME type).

The format can be any of Pandoc's [supported input formats](https://pandoc.org/MANUAL.html#option--from).
When the source is Google Docs, it should additionally correspond to an [export format supported by Google Docs](https://developers.google.com/workspace/drive/api/guides/ref-export-formats).
By default, Google Docs exports will go through Word's `docx` format.
In the case of Markdown, one can and should use `format = "markdown"`, which is supported by both Pandoc and Google Docs.

## Build instructions

Currently, the only supported build system is Nix.
You can build or run the program by cloning the Git repository and running `nix build` or `nix run`.
You can also build or run the program as follows:
```sh
nix build github:splintersuidman/bulletin
nix run github:splintersuidman/bulletin
```
