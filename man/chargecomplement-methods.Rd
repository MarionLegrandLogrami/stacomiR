\name{chargecomplement-methods}
\docType{methods}
\alias{chargecomplement}
\alias{chargecomplement-methods}
\alias{chargecomplement,ref_parqual-method}
\title{ Methods for Function chargecomplement}
\description{
Methods for function \code{chargecomplement} method chargecomplement
this method is called after selecting the object (data only counts one line)
and allows a request to obtain a complement, for instance possible values for a qualitative parameter
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ref_parqual")}}{collexts possible values for a qualitative parameter, for instance for parameter sex would be values male & female }
}}
\examples{
\dontrun{
dc_selectionne=6
taxa_selectionne=2038
stage_selectionne="AGJ"
object=new("ref_parqual")
object<-charge(object)
chargecomplement(object)
}
}
\keyword{methods}
\keyword{database}
