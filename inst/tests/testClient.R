## write and read an example object to the Julia server in juliaTest.jl
require(RJSONIO)

system("julia < ./juliaTest.jl", wait = FALSE)

sc <- socketConnection(port = 1234)

xx <- list(1:2, c(.75, 1.2), "Now is the time")

writeLines(toJSON(xx), sc)

txt <- readLines(sc)
txt


fromJSON(txt)


