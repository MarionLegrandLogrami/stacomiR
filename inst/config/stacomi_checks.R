setwd("C:/workspace/stacomir/pkg/stacomir")

res <- lintr::lint_package() 
cat(unlist(res) , file = "C:/Users/cedric.briand/Documents/projets/stacomi/stacomir/lintr.txt", sep = "\n")
# Some things that goodpractice::gp() will flag might be necessary features of your package, but in general it’s a nice list to read.
# Note: you can only run goodpractice::gp() once R CMD check passes.
goodpractice::gp()


devtools::release() # include devtools::spell_check()

devtools::spell_check()

#The formatR package, by Yihui Xie, makes it easier to clean up poorly formatted code. It can’t do everything, but it can quickly get your code from terrible to pretty good. Make sure to read the notes on the website before using it. It’s as easy as:
    
 install.packages("formatR")
formatR::tidy_dir("R")
#A complementary approach is to use a code linter. Rather than automatically fixing problems, a linter just warns you about them. The lintr package by Jim Hester checks for compliance with this style guide and lets you know where you’ve missed something. Compared to formatR, it picks up more potential problems (because it doesn’t need to also fix them), but you will still see false positives.

install.packages("lintr")
lintr::lint_package()

devtools::use_readme_rmd()
devtools::release() # include devtools::spell_check()

# BOF, ne marche pas avec testthat, ne marche pas sur les \dontrun, ... , a voir
install.packages('formatR')
library(formatR)
tidy_source("C:/workspace/stacomir/pkg/stacomir/R/create_generic.R",file="clipboard")
