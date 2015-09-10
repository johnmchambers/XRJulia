system("julia < ./juliaTestLines.jl", wait = FALSE)

sc <- socketConnection(port = 1235)

writeLines(c("This is the first line", "This is the seond"), sc)

readLines(sc)

