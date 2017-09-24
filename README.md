PLUS
====

PLUS is an [Emacs](https://www.gnu.org/software/emacs/) configuration for researchers, which is inspired by
    [Aaron Bedra's emacs.d](https://github.com/abedra/emacs.d) and [Bin Chen's emacs.d](https://github.com/redguardtoo/emacs.d).

## Directory structure
- `~/.emacs.d/init.el` is the configuration entrance.
- `~/.emacs.d/custom.el` includes user's personal configurations (`custom.org` also works).
- `~/.emacs.d/sitelisp/` collects the packages that are NOT maintained by package manager `ELPA`.
- `~/.emacs.d/elpa/` and the other things will be auto-maintained by `ELPA` or the other packages.

## Modules
**Golang, R, Python, Julia, LaTeX, markdown, orgmode, pandoc, gnuplot, Scala and Kotlin are supported.**

# Installation
1. Backup the current `Emacs` configuration, e.g., the file `~/.emacs` or the folder `~/.emacs.d/`.
1. Uninstall all systematic `Emacs` plugins. All plugins will be placed at `~/.emacs.d` from now on.
1. Clone `PLUS`

    `cd ~; git clone https://github.com/ubtc/plus.git ~/.emacs.d; cd -`

1. Run the following command to fetch the latest packages from remote package repositories:

    `emacs -nw --batch -l ~/.emacs.d/init.el -f package-refresh-contents`

## Third party tools installation (OPTIONAL)
External applications are optional, and can be installed through OS package managers, i.e.,
- [apt-cyg](https://github.com/transcode-open/apt-cyg) for `Cygwin`,
- [homebrew](https://github.com/mxcl/homebrew) for `Mac`, and
- various `Linux` package managers (`apt` for `Ubuntu`, `yum` for `Redhat`, `pacman` for `Arch`, `emerge` for `Gentoo`, etc.)

### Golang
- needed by `golang-mode`
- install through OS package manager

### Python (and JEDI, Flake8, importmagic)
- needed by `python-mode`
- install through OS package manager or `pip/pip3`

### R
- needed by `ess`
- install through OS package manager

### Scala (and sbt)
- needed by `ensime-mode/scala-mode-hook`
- install through OS package manager

### Kotlin
- needed by `kotlin-mode`
- install through OS package manager or through `sdkman!`

### Julia
- needed by `julia-mode` and `julia-shell`
- install through OS package manager

### pandoc
- needed by `pandoc-mode`
- install through OS package manager

### AUCTeX or XeTeX or LaTeX
- needed by `auctex` (for writing and formatting `TeX` files in `Emacs`)
- install through OS package manager (`auctex` need to be downloaded manually)

### BibTeX
- needed by `ebib`
- install through OS package manager

### gnuplot
- needed by `gnuplot-mode`
- install through OS package manager

### zip and unzip
- needed by `org-mode` to export `org` file to `odt` file
- install through OS package manager

### Aspell or hunspell
- needed by `flyspell`
- install through OS package manager. `aspell` usually is a better choice for programmers, whose dictionary has been set to `en_US`.

## That's all, have fun!
