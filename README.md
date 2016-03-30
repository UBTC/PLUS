PULSE
=====


m.w.'s [Emacs](https://www.gnu.org/software/emacs/) configuration, which was inspired by
    [Aaron Bedra's emacs.d](https://github.com/abedra/emacs.d) and [Bin Chen's emacs.d](https://github.com/redguardtoo/emacs.d).


## Directory structure
- `~/.emacs.d/init.el` is the configuration entrance.
- `~/.emacs.d/custom.el` includes some extra configurations (`custom.org` also works).
- `~/.emacs.d/site-lisp/` collects the packages that are NOT maintained by package manager `elpa`.
- `~/.emacs.d/elpa/` and the other things will be auto-maintained by `elpa` or the other packages.


## Modules
**Golang, Python, Julia, LaTeX, orgmode, markdown, pandoc, R and gnuplot are supported**


# Installation
1. Backup the current `Emacs` configuration, e.g., the file `~/.emacs` or the folder `~/.emacs.d/`.
1. Uninstall all systematic `Emacs` plugins. All plugins will be placed at `~/.emacs.d` from now on.
1. Run command

    `cd ~; git clone https://github.com/ubtc/pulse.git ~/.emacs.d`

1. Run the following command to fetch the latest packages from remote package repositories:

    `emacs -nw --batch -l ~/.emacs.d/init.el -f package-refresh-contents`

1. That's all.


## Third party tools installation (OPTIONAL)
External applications are optional, and can be installed through OS package managers, i.e.,
- [apt-cyg](https://github.com/transcode-open/apt-cyg) for `Cygwin`,
- [homebrew](https://github.com/mxcl/homebrew) for `Mac`, and
- various `Linux` package managers (`apt-get` for `Ubuntu`, `yum` for `Redhat`, `pacman` for `Arch`, `emerge` for `Gentoo`, etc.)

### golang platform
- needed by `golang-mode`
- install through OS package manager

### python platform (python, jedi, flake8 and importmagic)
- needed by `python-mode`
- install through OS package manager and/or `pip/pip3`

### R platform
- needed by `ESS`
- install through OS package manager

### julia platform
- needed by `julia-mode` and `julia-shell`
- install through OS package manager

### pandoc platform
- needed by `pandoc-mode`
- install through OS package manager

### auctex, and xetex/latex
- needed by `auctex` (for writing and formatting `TeX` files in `Emacs`)
- install through OS package manager (`auctex` need to be downloaded manually)

### bibtex
- needed by `ebib`
- install through OS package manager

### gnuplot
- needed by `gnuplot-mode`
- install through OS package manager

### zip and unzip
- needed by `org-mode` to export `org` file to `odt` file
- install through OS package manager

### aspell/hunspell
- needed by `flyspell`
- install through OS package manager. `aspell` usually is a better choice for programmers, whose dictionary has been set to `en_US`.

## That's all, have fun!
