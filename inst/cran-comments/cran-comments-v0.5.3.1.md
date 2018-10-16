This updates the existing stacomiR package on CRAN.

# Notes

* The package is intended to work with a database, in previous development is was relying on a "test" 
database for some calculations made with the sqldf package. All reference to a test database have been removed, and replaced with calls to dplyr for internal calculations. 

* Updated testhat tests, those are now in a tests subdirectory as recommended by testhat developper. Most tests are relying on the database backend and a skip has been added to those tests.

* developped vignette for package 


#Testing Environments

My Windows machine.
Win Builder -- current
R-forge.


