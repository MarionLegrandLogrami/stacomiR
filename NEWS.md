# stacomiR 0.5.4.0

* removed reference to test database, and use of dplyr for internal calculations. 
Trying to launch as test database with postgres failed on distant computers for tests.... (@marion #484-502)

* Updated testhat tests (@marion  @cedric #502-506)

* developped vignette for package (@marion #480-484)

# stacomiR 0.5.3.2

* changed stacomiR-package.md according to Kurt Hornik (CRAN) demand to use macro in share (@cedric #481)

* bug in interface report_dc corrected (@cedric #489)

* Problem with ceilPOSIXt now named only ceil in package Hmisc. (@cedric #489)


# stacomiR 0.5.3.1

* added and edited readme.Rmd file (@cedric and @marion #447-#452)

* compilation of messages, fixes bug for for some year in report\_mig\_mult (@cedric #444)

* Editing translation messages (@cedric #446)

* fix bug for setasqualitative method in report\_sea__age (@cedric #443)

* updated documentation (@cedric #440)

* Modification in glass eel graph (@cedric #441)

* changes to the summary-report\_mig\_interannual-method (@cedric # 439)

* change to the presentation page in R-forge (@timothée #425 #436-#438)

# stacomiR 0.5.3

* Langage with poedit (@cedric and @timothee).

* suppress all assignments to .globalEnv.

* adapt documentation and package to pass with zero warning and zero note.

# stacomiR 0.5.2 (R-Forge)

* Development of the command line interface (@cedric)

* Adaptation to the database change to adapt French SANDRE directives (@cedric)

* added 4 new report (silver eel, sea age, report\_mig\_mult, report_annual) and adapted all old script to use in both command line and graphical interface. (@ cedric and @marion)

# stacomiR 0.5.1 (R-Forge)

* tests with testthat (@cedric)

# stacomiR 0.5.0 (R-Forge)

* Roxygen documentation (@cedric)

# stacomiR 0.4 (R-Forge)

* classes adapted to the new database format

# stacomiR 0.3 (R-Forge)

* Adapting to one schema per user in the postgres database 

* initial development of all classes following version 0.3 of the database
 