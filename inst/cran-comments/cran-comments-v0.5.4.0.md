This updates the existing stacomiR package on CRAN.

# Notes
There is one warning concerning the size of the vignette pdf when built on R-Forge
* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'stacomir.pdf' from 467Kb to 215Kb
  consider running tools::compactPDF(gs_quality = "ebook") on these files
But since the vignette is generated on the fly in R-forge, I don't think there is anything
I can do about that.

# New development

* the removal of calls to the database named "test", and use of dplyr for internal calculations. 
* the update of tests
* the development of a vignette to present the package

#Testing Environments

My Windows machine.
Win Builder -- current and development.
R-forge.


