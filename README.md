PULSE
=====

my [Emacs](https://www.gnu.org/software/emacs/) configuration.


## Directory structure
- `~/.emacs.d/init.el` is the configuration entrance and the most important file.
- `~/.emacs.d/custom.el` includes some extra configurations (`custom.org` also works).
- `~/.emacs.d/elpa/` will be auto-maintained by the package manager elpa.
- `~/.emacs.d/site-lisp/` collects the packages that are not maintained by elpa.
- `~/.emacs.d/snippets` collects [snippets](https://github.com/CestDiego/.emacs.d/tree/master/snippets) for [Yasnippet](https://github.com/capitaomorte/yasnippet).


## Modules
**Julia, Haskell, Golang, Python, R, LaTeX, Markdown, Pandoc and gnuplot are supported.**


# Installation
1. Please backup your current Emacs configuration, e.g., the file "~/.emacs" or the folder "~/.emacs.d/".
1. Uninstall any system Emacs plugins. All plugins will be placed at "~/.emacs.d" from now on.
1. Run command

    `cd ~; git clone https://github.com/ubtc/pulse.git ~/.emacs.d`

1. Run the following command to fetch the latest packages from remote package repositories:

    `emacs -nw --batch -l ~/.emacs.d/init.el -f package-refresh-contents`

1. That's all.


## Third party tools installation (OPTIONAL)
You can install those tools to your OS, if you need them. If some of these optional tools are not installed, please ignore the related error messages.

OS package manager usually is a convenient way to install the packages you need. Here, OS package manager includes, but not limited to,
- [apt-cyg](https://github.com/transcode-open/apt-cyg) at Cygwin,
- [homebrew](https://github.com/mxcl/homebrew) at Mac, and
- any package manager at Linux (apt-get at Ubuntu, yum at Redhat, pacman at Arch, emerge at Gentoo ...)

### julia platform
- needed by `julia-mode`
- install through OS package manager

### golang platform
- needed by `go-mode`
- install through OS package manager

### pandoc
- needed by `pandoc-mode`
- install through OS package manager

### python platform
- needed by `elpy`
- install through OS package manager

### R platform
- needed by `ESS`
- install through OS package manager

### haskell platform
- needed by `haskell-mode`
- install through OS package manager

### gnuplot
- needed by `gnuplot`
- install through OS package manager

### auctex, and xetex/latex
- needed by `auctex` (for writing and formatting TeX files in Emacs)
- install through OS package manager [auctex need to be downloaded manually]

### bibtex
- needed by `ebib`
- install through OS package manager

### zip and unzip
- needed by `org-mode` to export org file to odt file
- install through OS package manager

### aspell/hunspell
- needed by `flyspell`
- install through OS package manager. aspell usually is a better choice for programmers, whose dictionary has been set to "en_US".

## That's all, have fun!
