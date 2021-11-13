This is a quick lesson on how to apply the correct formatting-styles to files after editing them. The purpose of this is so all source files have a uniform style and makes it easier to read and peruse the source code.

# Choice of Style

We are using (styler)[https://styler.r-lib.org/]. There was no great thought put into this.

# Installing the styler

Install styler with `install.packages("styler")` (or `remotes::install_github("r-lib/styler")` if you want to install it from github).

# Using the styler

Style the files with the RStudio addin or from an R environment (at the root directory) by running `library(styler)` and then `style_pkg("QinR")`.

If you are getting errors like `unexpected symbol` and `unexpected numeric constant` this is because styler is trying to style the code--in the examples--as it styles everything else. You have to make sure that the example code is valid R code.