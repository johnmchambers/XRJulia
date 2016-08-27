%% copied from XR NOT Roxygen
\name{noScalar}
\alias{noScalar}
\title{Send a Non-scalar Version of an Object}
\usage{
noScalar(object)
}
\arguments{
\item{object}{A vector object.  Calling with a non-vector is an error.}
}
\value{
the object, but with the S4 bit turned on.
Relies on the convention that XR interfaces leave S4 objects
as vectors, not scalars, even when they are of length 1
}
\description{
Ensures that an object is interpreted as a vector (array) when sent to the server language.
The default strategy is to send length-1 vectors as scalars.
}
\references{
Chambers, John M. (2016)
\emph{Extending R},
Chapman & Hall/CRC.
( Chapter 12, discussing this package, is included in the package: \url{../doc/Chapter_XR.pdf}.)
}

