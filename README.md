# XRJulia - An Interfaces from R to Julia

This package provides an interface from R to Julia, based on the XR
structure, as implemented in the XR package, in this repository.

The interface is designed as a basis for computations in R that use
functions, objects and classes in Julia.
In particular, the design caters to programmers developing application
packages.
The XR structure encourages definition of proxy functions and classes
in \R{}, which users of the package can treat essentially as they
would in R, without special programming imposed by the interface.

The interface structure is described in the forthcoming book
*Extending R* (John M. Chambers, 2016, Chapman & Hall).
A pdf version of the XRJulia chapter from the book is included with the
documentation of this package.
