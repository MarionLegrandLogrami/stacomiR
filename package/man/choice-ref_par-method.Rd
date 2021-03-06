% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_par.R
\docType{methods}
\name{choice,ref_par-method}
\alias{choice,ref_par-method}
\title{Choice method for ref_par referential objects}
\usage{
\S4method{choice}{ref_par}(object,
  label = "Choix d'une caracteristique de lot", nomassign = "ref_par",
  frameassign = "frame_par", is.enabled = TRUE)
}
\arguments{
\item{object}{An object of class \link{ref_par-class}}

\item{label}{The label that will be displayed in the message frame or as output text}

\item{nomassign}{The assignment name in envir_stacomi}

\item{frameassign}{The name of the frame used for assignment in envir_stacomi}

\item{is.enabled}{Default TRUE.}
}
\description{
Choice method for ref_par referential objects
}
\note{
the choice method assigns an object of class ref_par named ref_par in the environment envir_stacomi

this method choice is also on daughter classes ref_parquan, hence the parameters, however it was redefined in ref_parqual

to load the possible values of qualitative parameters
}
\examples{
 
\dontrun{
 object=new("ref_par")
 win=gwindow()
 group=ggroup(container=win,horizontal=FALSE)
 object<-charge(object)
 choice(object)
}
}
\author{
Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
}
\keyword{internal}
