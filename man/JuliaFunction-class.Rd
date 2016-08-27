%% This is NOT a roxygen documentation file.  roxygen does not deal
%% well with documenting generator functions that correspond to
%% initialize methods.
\docType{class}
\name{JuliaFunction-class}
\alias{JuliaFunction}
\alias{JuliaFunction-class}
\alias{initialize,JuliaFunction-method}
\title{Proxy Objects in R for Julia Functions}
\usage{
JuliaFunction(...)

\S4method{initialize}{JuliaFunction}(name, module = "", evaluator =
RJulia(, ...))
}
\description{
A class and generator function for proxies in R for Julia functions.
}
\details{
An object from this class is an R function that is a proxy for a function in Julia. Calls to the R function evaluate
a call to the Julia function.  The arguments in the call are converted to equivalent Julia objects;
these typically include proxy objects for results previously computed through the XRJulia interface.
}
\arguments{
\item{name, module}{The name and module of the Julia function.}
\item{evaluator}{The evaluator object to use.  By default, and usually, the current evaluator
is used, and one is started if none has been.}
\item{...}{For \code{RJulia}, the arguments as interpreted by the
    initialize method, so typically \code{name} and optionally
    \code{module}.  Remaining arguments are passed along to the next method.}
}
\section{Slots}{

\describe{
\item{\code{name}}{the name of the server language function}

\item{\code{module}}{the name of the module, if that needs to be imported}

\item{\code{evaluatorClass}}{the class for the evaluator, by default and usually, \code{\link{JuliaInterface}}}
}}

\examples{
if(findJulia(test = TRUE)) {
  set.seed(228)
  x <- matrix(rnorm(1000),20,5)
  xm <- juliaSend(x)
  svdJ <- JuliaFunction("svdfact")
  sxm <- svdJ(xm)
  sxm
}
}