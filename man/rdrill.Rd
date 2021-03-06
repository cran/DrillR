% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RDrillClass.R
\docType{methods}
\name{rdrill}
\alias{rdrill}
\alias{rdrill,character,numeric-method}
\title{Get the Apache Drill Connection, return the RDrill object.}
\usage{
rdrill(host, port)

\S4method{rdrill}{character,numeric}(host, port)
}
\arguments{
\item{host}{Apache Drill cluster/node host}

\item{port}{Apache Drill cluster/node port}
}
\description{
Get the Apache Drill Connection, return the RDrill object.
}
\examples{
rdrill("127.0.0.1",8047)
}
\keyword{RDrillClass}

