% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_year.R
\docType{methods}
\name{choice,ref_year-method}
\alias{choice,ref_year-method}
\title{choice method for ref_year referential}
\usage{
\S4method{choice}{ref_year}(object, nomassign = "ref_year",
  funoutlabel = gettext("Year selected\\n", domain = "R-stacomiR"),
  titleFrame = gettext("Year choice", domain = "R-stacomiR"),
  preselect = 1)
}
\arguments{
\item{object}{An object of class \link{ref_year-class}}

\item{nomassign}{The name to be assigned in envir_stacomi}

\item{funoutlabel}{The label that appears in funout}

\item{titleFrame}{Title for the frame}

\item{preselect}{The number of the year selected in the gdroplist (integer)}
}
\description{
Allows the selection of year and the assignment in environment envir_stacomi
}
\examples{
 
\dontrun{
object=new("ref_year")
object<-charge(object)
win=gwindow(title="test ref_year")
group=ggroup(container=win,horizontal=FALSE)
choice(object,nomassign="ref_year",funoutlabel="essai",titleFrame="essai ref_year",preselect=1)
dispose(win)
}
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\keyword{internal}
