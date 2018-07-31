## This code builds up a file with a table of elapsed times
## for transferring data.  A run uses a particular vector
## type and appends the results to file "./transferTimes"

ev = XRJulia::RJulia()
options(stringsAsFactors = FALSE)
set.seed(526)
numericTest = rnorm(5000)
integerTest  = as.integer(numericTest*100)
logicalTest = numericTest > 0
characterTest = as.character(integerTest)
complexTest <- complex(numericTest, numericTest*.1)
rawTest <- charToRaw(paste0(characterTest, collapse = ""))
length(rawTest) <- length(numericTest)
data <- data.frame(character = characterTest, numeric = numericTest, integer = integerTest, logical = logicalTest,
                      complex = complexTest, raw = rawTest)

firstTime <- TRUE
myFile <- "./transferTimes"
prev <- if(file.exists(myFile)) read.table(myFile) else data.frame()

## Transfer size N vectors of R class What, M times by JSON and by binary.
## Change N,M,What

NN <- c(10, 100, 1000)

M <- 50
What <- "character"

for(N in NN) {

  cat(What, N, "\n")
value <- data.frame(what = rep(What, 2*M), size = rep(N, 2*M),
                    how = rep(c("Binary", "JSON"), M), 
                    when = rep(as.character(Sys.time()), 2*M))

M1 <- (M+firstTime)
elapsed <- numeric(0)
for(i in 1:M1) {
    x <- sample(data[[What]], N, replace = TRUE)
    ## force first binary, then JSON
    XRJulia::juliaOptions(largeObject = as.integer(N/2))
    time1 = system.time(ev$Get(ev$Send(x)))
    XRJulia::juliaOptions(largeObject = as.integer(N*2))
    time2= system.time(ev$Get(ev$Send(x)))
    elapsed <- append(elapsed, c(time1[[3]], time2[[3]]))
}

if(firstTime){ ## omit 1st iteration, may include JIT
  elapsed <- elapsed[-(1:2)]
  firstTime <- FALSE
  }
value[["elapsed"]] <- elapsed

value  <- rbind(prev, value)
prev <- value
}


write.table(value, myFile)

boxplotTimes <- function(data) {
  sizes <- sort(unique(data$size))
  allTitle <- gettextf('Round-trip transfer times, type "%s"',
                       paste(unique(data$what), collapse = ", "))
  for(N in sizes){
    boxplot(elapsed ~ how, data, subset = data$size == N,
            main = paste0(allTitle, "; size = ", N))
  }
}

    
