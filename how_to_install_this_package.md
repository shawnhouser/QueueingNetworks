# Quick steps

# **TODO** Does Installing from github require gt?

* If you do not want to build, but use it run `library(devtools)`
* and then `install_github("angyalkavalcsics/QueueingNetworks/QinR")`
** If you want to build this package run `R CMD build QinR` in the root directory.
** Run `install.packages('<name of generated file>')` in an R environment to install.
* In any script or environment, load functions with `library(QinR)`
* You can now use the package

# Installing this package

## Installing from github

Installing this package with github is the most straightforward way to use it. This method ensures that you use the most up-to-date version of the package so new functionality can be added and bugs fixed.

To start, ensure that you have the package `devtools` installed on your system (this can be a pain to install on some operating systems, but from our experience, RStudio has this preinstalled on MacOSX and Windows).

After you know devtools is installed, load it with `library(devtools)`.

After it is loaded, run `install_github("angyalkavalcsics/QueueingNetworks/QinR")` to download the library

Congratulations. The package is **installed**. To use the package, jump to the "Loading this package" section.

## Installing from source

Know that it is much quicker to install with github. You can still install from source, but only do so because you can not install `devtools` or you have another *good* reason.

Before you build this package from source, know that it requires the (`gt`)[https://www.rdocumentation.org/packages/gt/] package for its graph-drawing capabilities.

Start by obtaining a copy of the source (best done with the command `git clone https://github.com/angyalkavalcsics/QueueingNetworks`).

Once you have the source code downloaded, navigate to it's root directory and run `R CMD build QinR`. This will compile the source-code into a `.tar.gz` file called `QinR_<version>.tar.gz` where `<version>` is the current version of the package.

Take note of this filename and run `install.packages('<name of generated file>')`, where, self-explanatory, `<name of generated file>` is the name of the generated file.

Congratulations. The package is **installed**. To use the package, jump to the "Loading this package" section.

# Loading this package

After the package is downloaded, you can run `library(QinR)` at any point, anywhere, in the future, to gain access to the functions within.