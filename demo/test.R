require(igraph)
require(mosaic)
library(manipulate)
library(devtools)
load_all()


####################################################

ccg = ccg.game(7, p=0.5, r=0.1, capacity=3)
ccg = init.mapping(ccg)
plot(ccg)
plot(ccnpk(ccg, alg="random"))
plot(ccnpk(ccg, alg="greedy-dumb"))
plot(ccnpk(ccg, alg="greedy-smart"))
plot(ccnpk(ccg, alg="greedy"))
# plot(ccnpk(ccg, alg="opt"))
system.time(plot(ccnpk(ccg, alg="greedy")))

####################################################

# Test the effectiveness of algorithms
alg.test = function(n_s) {
  p_s = 0.5
  n_t = n_s
  ccg = ccg.game(n1=n_s, p1=p_s, n2 = n_t, r=0.1, capacity = 3)
  ccg = init.mapping(ccg)
  
  algs = c("random", "greedy-dumb", "greedy-smart", "greedy")
  secs = NULL
  edges = NULL
  isCompleted=NULL
  for(alg in algs) {
    secs[paste("time.", alg, sep="")] <- system.time(result <- ccnpk(ccg, alg=alg))["elapsed"]
    edges[alg] = ecount(result$R)
    isCompleted[paste("complete.", alg, sep="")] = is.completed(result)
  }
  out = c(n_s = n_s, p_s = p_s, n_t = n_t
           , size = n_s * n_t
           , collective.expertise = collaboration(ccg$g1)
           , social.density = ecount(ccg$g1) / choose(vcount(ccg$g1), 2)
           , max.task.difficulty = max(V(ccg$g2)$difficulty)
           , edges, isCompleted, secs
#           , random = ecount(ccnpk(ccg, alg="random")$R)
#           , greedy.dumb = ecount(ccnpk(ccg, alg="greedy-dumb")$R)
#           , greedy.smart = ecount(ccnpk(ccg, alg="greedy-smart")$R)
#           , greedy = ecount(ccnpk(ccg, alg="greedy")$R)
#           , opt = ecount(ccnpk(ccg, alg="opt")$R)
  )
  return(out)
}

alg.test.many = function (n_s, numTrials) {
  return(do(numTrials) * alg.test(n_s))
}

# Choose how many graphs of each size to create
numTrials = 10
# Write the results to a file
do.it = function (n, numTrials) {
  write.table(alg.test.many(n_s = n, numTrials)
          , paste("data/ccnpk_", runif(1), ".csv", sep="")
          , row.names = FALSE, col.names = FALSE, sep=",")
}
# Do this
do.it(8, 1)

########## RUN THIS!!!! #############################

max.n = 200
m = 20
sapply(resample(100:max.n, size=m), do.it, numTrials = 10)


#####################################################


ds = read.csv("data/ds.csv")
dim(ds)

ds.long = reshape(ds, varying=8:11, v.names="numEdges"
        , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.long2 = reshape(ds, varying=12:15, v.names="isCompleted"
                   , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.long3 = reshape(ds, varying=16:19, v.names="time"
                  , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.final = cbind(ds.long, isCompleted = ds.long2$isCompleted, time = ds.long3$time)
ds.final$alg = factor(ds.final$alg)
dim(ds.final)
favstats(~n_s, data=ds)


trellis.par.set(theme = col.mosaic())
ss = trellis.par.get("superpose.symbol")
ss$cex = 1.2
ss$pch = 1
trellis.par.set(name="superpose.symbol", value=ss)

# Figure 1 - Quality of Algorithms
pdf("../gfx/quality.pdf", width=10, height=8)
xyplot(I(numEdges + 1) ~ jitter(size), groups=alg
       , data=subset(ds.final, isCompleted == 1), type=c("p", "smooth")
       , scales=list(x = list(log = 2), y=list(log=2))
       , main="Comparison of Quality of Algorithms"
       , sub = paste("N =", nrow(ds))
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Number of Edges Added"
       , alpha = 0.5, lwd = 5
       , auto.key=list(columns=2))
dev.off()

# Figure 2 - Efficiency of Algorithms
pdf("../gfx/efficiency.pdf", width=10, height=8)
xyplot(time ~ jitter(size), groups=alg, data=ds.final, type=c("p", "smooth")
       , scales=list(x = list(log = 2), y=list(log=2))
       , main="Comparison of Efficiency of Algorithms"
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Running Time (seconds)"
       , alpha = 0.5, lwd = 5
       , auto.key=list(columns=2))
dev.off()

# Figure 3 - Effectiveness of Algorithms
# Fit logistic curves
pdf("../gfx/effectiveness.pdf", width=10, height=8)
xyplot(jitter(isCompleted) ~ jitter(size), groups=alg, data=ds.final, type=c("p")
       , main="Comparison of Effectiveness of Algorithms"
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Running Time (seconds)"
       , alpha = 0.5, lwd = 5
       , auto.key=list(columns=2))
fm = glm(isCompleted ~ size + alg, data=ds.final, family=binomial)
fit.completed = makeFun(fm)
palette = trellis.par.get("superpose.symbol")$col
plotFun(fit.completed(size, alg="greedy") ~ size, add=TRUE, col=palette[1], lwd=5)
plotFun(fit.completed(size, alg="greedy.dumb") ~ size, add=TRUE, col=palette[2], lwd=5)
plotFun(fit.completed(size, alg="greedy.smart") ~ size, add=TRUE, col=palette[3], lwd=5)
plotFun(fit.completed(size, alg="random") ~ size, add=TRUE, col=palette[4], lwd=5)
dev.off()

xyplot(time ~ numEdges, data=ds.final)
bwplot(~time | isCompleted, data=ds.final)
favstats(~time | isCompleted, data=ds.final)

