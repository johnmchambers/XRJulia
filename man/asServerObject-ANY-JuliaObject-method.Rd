\docType{methods}
\name{asServerObjectMethods}
\alias{asServerObjectMethods}
\alias{asServerObject,ANY,JuliaObject-method}
\alias{asServerObject,array,JuliaObject-method}
\alias{asServerObject,list,JuliaObject-method}
\title{Julia methods for asServerObject()}
\usage{
\S4method{asServerObject}{ANY,JuliaObject}(object, prototype)

\S4method{asServerObject}{array,JuliaObject}(object, prototype)

\S4method{asServerObject}{list,JuliaObject}(object, prototype)

}
\arguments{
\item{object}{The \R object.}

\item{prototype}{The proxy for a prototype of the Julia object, supplied by the evaluator.}
}
\description{
The default method for JuliaObject is modelled on the overall default
method in XR.

For arrays, the method uses the reshape() function in Julia to create a suitable multi-way array.

For "plain" lists, the method produces the Julia expression for a list or a dictionary;
with other attributes, uses the .RClass form.

}

