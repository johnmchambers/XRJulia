## test out the socket connection to juliaTest.jl script
sc = connect(1234)

bb = [1,2,3]

import JSON

JSON.print(sc, bb)

## and read the answer back:  it should be a 2-element array, the second element == bb
##

xx = JSON.parse(sc)

