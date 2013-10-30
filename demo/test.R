require(igraph)
require(mosaic)
library(manipulate)
library(devtools)
load_all()


####################################################

ccg = ccg.game(20, p=0.5, r=0.1, capacity=3)
ccg = init.mapping(ccg)
plot(ccg)
plot(ccnpk(ccg, alg="random"))
plot(ccnpk(ccg, alg="greedy-dumb"))
plot(ccnpk(ccg, alg="greedy-smart"))
plot(ccnpk(ccg, alg="greedy"))
ccnpk.lp(ccg)
plot(ccnpk(ccg, alg="opt"))
system.time(plot(ccnpk(ccg, alg="greedy")))

####################################################






# Choose how many graphs of each size to create
numTrials = 10
# Do this
alg.test.many(8, numTrials)

########## RUN THIS!!!! #############################

max.n = 200
m = 20
sapply(resample(100:max.n, size=m), do.it, numTrials = 10)


#####################################################

# Each row corresponds to one randomly generated ccg
ds = read.csv("data/ds.csv")
dim(ds)

# Some false positives on both sides??
tally(complete.random ~ complete.greedy, data=ds)

ds.long = reshape(ds, varying=8:11, v.names="numEdges"
        , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.long2 = reshape(ds, varying=12:15, v.names="isCompleted"
                   , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.long3 = reshape(ds, varying=16:19, v.names="time"
                  , timevar = "alg", times = names(ds)[8:11], direction="long")
ds.final = cbind(ds.long, isCompleted = ds.long2$isCompleted, time = ds.long3$time)
ds.final$alg = factor(ds.final$alg)

# Each row corresponds to one algorithm on one graph
# Each graph is represented four times
dim(ds.final)
favstats(~n_s, data=ds)

ds.plot = subset(ds.final, numEdges > 0)

# trellis.par.set(theme = col.whitebg())
show.settings()
ss = trellis.par.get("superpose.symbol")
ss$cex = 1
ss$pch = 19
trellis.par.set(name="superpose.symbol", value=ss)

# Figure 1 - Quality of Algorithms
# pdf("~/Dropbox/Academic/E1-composite/CCNets/figs/quality.pdf", width=10, height=8)
xyplot(I(numEdges + 1) ~ jitter(size) | as.factor(isCompleted), groups=alg
       , data=ds.plot, type=c("smooth")
#       , scales=list(x = list(log = 2), y=list(log=2))
#       , par.settings = list("superpose.symbol" = ss)
       , main="Comparison of Quality of Algorithms"
       , sub = paste("N =", nrow(ds))
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Number of Edges Added"
       , alpha = 0.5, lwd = 3, lty=1, pch = 1
       , strip = strip.custom(style = 1, factor.levels = c("All Tasks Not Completed", "All Tasks Complete"))
       , auto.key=list(columns=4))
panel.text(2^8, 2^7, "All algorithms make the same\nnumber of assignments if they fail", cex=0.8)
# dev.off()

favstats(time ~ isCompleted, data=ds.final)
# Figure 2 - Efficiency of Algorithms
# pdf("~/Dropbox/Academic/E1-composite/CCNets/figs/efficiency.pdf", width=10, height=8)
xyplot(time ~ jitter(size) | as.factor(isCompleted), groups=alg, data=ds.plot, type=c("p", "smooth")
       , scales=list(x = list(log = 2), y=list(log=2))
       , main="Comparison of Efficiency of Algorithms"
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Running Time (seconds)"
       , alpha = 0.3, lwd = 3, lty=1
       , strip = strip.custom(style = 1, factor.levels = c("All Tasks Not Completed", "All Tasks Complete"))
       , auto.key=list(columns=4))
# dev.off()

# Figure 3 - Effectiveness of Algorithms
# Fit logistic curves
# pdf("~/Dropbox/Academic/E1-composite/CCNets/figs/effectiveness.pdf", width=10, height=8)
ds.small = subset(ds.plot, size < 1000)
xyplot(jitter(isCompleted) ~ jitter(size), groups=alg, data=ds.small, type=c("p")
#       , scales=list(x = list(log = 2))
       , main="Comparison of Effectiveness of Algorithms"
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Were All Tasks Completed?"
       , alpha = 0.3, lwd = 3
       , auto.key=list(columns=4)
       )
fm = glm(isCompleted ~ size + alg, data=ds.small, family=binomial)
fit.completed = makeFun(fm)
palette = trellis.par.get("superpose.symbol")$col
plotFun(fit.completed(size, alg="greedy") ~ size, add=TRUE, col=palette[1], lwd=5)
plotFun(fit.completed(size, alg="greedy.dumb") ~ size, add=TRUE, col=palette[2], lwd=5)
plotFun(fit.completed(size, alg="greedy.smart") ~ size, add=TRUE, col=palette[3], lwd=5)
plotFun(fit.completed(size, alg="random") ~ size, add=TRUE, col=palette[4], lwd=5)
ladd(panel.text(800, 0.5, "greedy.smart\nconsiderably more likely\nto complete all tasks", cex=1))
# dev.off()

xyplot(time ~ numEdges, data=ds.final)
bwplot(~time | isCompleted, data=ds.final)
favstats(~time | isCompleted, data=ds.final)

