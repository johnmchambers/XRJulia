# tests for Julia >= 0.7
## if not produces warnings requiring Array{...}(undef, ...)
ev = RJulia()
xx = ev$Send(1:3)
xxx = ev$Get(xx)
yy = ev$Send(list("a",2))
yyy = ev$Get(yy) # no warning
bb = 1:10000 # uses binary transfer
bj = ev$Send(bb)
bbb = ev$Get(bj) # no warning
bjs = ev$Send(paste(bb))
bjss = ev$Get(bjs)
bl = runif(10000)> .5
blj = ev$Send(bl)
blr = ev$Get(blj)
dd = list(a=xxx, b= list(c=yyy, d = paste(bbb)))
ddj = ev$Send(dd)
