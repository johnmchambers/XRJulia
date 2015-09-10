require(XRJulia)
ep = XRJulia::RJulia()
mm = matrix(rnorm(12),4,3)
mj = ep$Convert(mm)
svdJ <- JuliaFunction("svdfact")
sj <- svdJ(mj)
## ep$Command('show(toR(%s))',mj)
sr <- ep$Get(sj)
sr
