#
# Demonstration of the package
#
#############################################################

require(igraph)
require(mosaic)
trellis.par.set(theme = col.mosaic())
library(devtools)
load_all()


####################################################

n = 20
p = 1/2
g1 = erdos.renyi.game(n, p)
g2 = erdos.renyi.game(n, p)
cg = compgraph(g1, g2, name="myCompGraph")

summary(cg)
plot(cg)

#############################################################

# Composite Task Graph
g = ccg.game(20, 0.5, r=0.1)
plot(g)
is.completed(g)


# Baby example
E.social = c(1,2, 1,3, 1,4, 2,4)
g1 = graph(edges = E.social, directed=FALSE)
V(g1)$expertise = c(3,1,4,1)

E.task = c(1,2, 1,3, 2,4, 3,5, 3,6)
g2 = graph(edges = E.task, directed=TRUE)
V(g2)$difficulty = c(7,5,2,1,1,2)

g = compgraph(g1, g2, "SmallTask")
plot(g)

is.solvable(g, 1)
is.completed(g, 1)

##############################################################

# With no restrictions, tasks are independent
# so consider only a single task

ccg = ccg.game(n1=10, p1=0.2, n2=1, r=0.1)
plot(ccg)

##############################################################

require(manipulate)
manipulate(plot(ccg.game(n1 = n_s, p1 = p_s, n2 = n_t, r = r))
           , n_s = slider(1, 100, initial=25, label="Number of Researchers")
           , p_s = slider(0, 1, initial=0.25, label="Probability of Social Edge")
           , r = slider(0,1, initial=0.5, label="Assignment Percentage")
           , n_t = slider(1, 100, initial=1, label="Number of Tasks")
)

ccg = ccg.game(n1=10, p1=0.2, n2=1, r=0.1)

manipulate( {
  if (is.null(manipulatorGetState("myccg"))) {
    manipulatorSetState("myccg", ccg)
  } else {
    ccg = manipulatorGetState("myccg")
    ccg = add.random.assignments(ccg)
    manipulatorSetState("myccg", ccg)
  }
  plot(ccg)
}
  , x = button(label="Add Random Assignment")
)

##############################################################

# Sweep of parameter space
n = 200
p = 0.9
r = 0.1
numTrials = 20
ds = do(numTrials) * is.completed(ccg.game(n1=n, p1=p, n2=1, r=r))
sum(ds$result)

# More carefully
test = function(n_s, p_s, n_t, r) {
  ccg = ccg.game(n1=n_s, p1=p_s, n2 = n_t, r=r)
  return(c(n_s = n_s, p_s = p_s, n_t = n_t, r = r
           , collective.expertise = collaboration(ccg$g1)
           , social.density = ecount(ccg$g1) / choose(vcount(ccg$g1), 2)
           , mapping.density = ecount(ccg$R) / (vcount(ccg$g1) * vcount(ccg$g2))
           , max.task.difficulty = max(V(ccg$g2)$difficulty)
           , is.completed = is.completed(ccg)))
}

test.many = function (n_s, p_s, n_t = 1, r, numTrials) {
  return(do(numTrials) * test(n_s, p_s, n_t, r))
}
test.many(n,p, 1, r, numTrials)

# Try all the r's
n = 100

test.sweep = function (n_s, n_t = 1, numTrials, granularity = 0.1, warn=TRUE) {
  if (!(granularity > 0 & granularity < 0.2)) {
    granularity = 0.1
  }
  ps = seq(from=granularity, to=1, by=granularity)
  rs = seq(from=granularity, to=1, by=granularity)
  pairs = expand.grid(p_s = ps, r = rs)
  N = length(rs) * length(ps) * numTrials
  cat(paste("\nComputing a total of", N, "trials!"))
  if (warn) {
    # randomly sample some of the items you are going to compute
    # and then benchmark the time for all the trials
    samp.size = 100
    samp.pairs = sample(pairs, size = samp.size)  
    benchmark = system.time(do.call("rbind", mapply(FUN=test.many, samp.pairs$p_s, samp.pairs$r
               , MoreArgs=list(n_s=n_s, n_t = n_t, numTrials=1))))
    exp.time = (benchmark["elapsed"] / samp.size) * N
    cat(paste("\nThis should take about", round( exp.time / 60, 2), "minutes"))
  }
  res.mat = do.call("rbind", mapply(FUN=test.many, pairs$p_s, pairs$r
            , MoreArgs=list(n_s=n_s, n_t = n_t, numTrials=numTrials)))
#  res.mat = do.call("rbind", sapply(rs, test.many, n_s=n_s, p_s=p_s, n_t = n_t, numTrials=numTrials))
  dim(res.mat) = c(9, length(rs) * length(ps) * numTrials)
  res = data.frame(t(res.mat))
  names(res) = c("n_s", "p_s", "n_t", "r", "collective.expertise"
                 , "social.density", "mapping.density", "max.task.difficulty", "is.completed")
  return(res)
}

res = test.sweep (n_s = n, numTrials = 20)

# Plot the results
xyplot(jitter(is.completed) ~ jitter(r), groups=p_s, data=res
       , alpha=0.5, type=c("p", "smooth"), lwd=3
       , auto.key=list(columns=5), xlab="Density of Mapping (%)"
       , ylab = "Probability that the Task is Completed"
       , sub = paste("Numnber of Trials =", nrow(res), "| Number of Researchers =", max(res$n_s), "| Number of Tasks =", max(res$n_t)))
favstats(is.completed ~ r, data=res)
# plotFun((1 + 0.2) * x ~ x, add=TRUE)
# plotFun(x^(1/10) ~ x, add=TRUE)

# Note that collective expertise is approximately normally distributed
densityplot(~ r * collective.expertise, data=res)
favstats(~ r * collective.expertise | p_s, data=res)
# Compute the expected collective expertise
# Note that lambda_s = 2
res = transform(res, exS = r * n_s * (1 + p_s) * 2)
xyplot(r * collective.expertise ~ exS, groups=p_s, data=res, type=c("p", "r"))

# Confirm the difficulty is exponential
favstats(~max.task.difficulty, data=res)
densityplot(~max.task.difficulty, data=res)

# Fit a linear model
fm = lm(is.completed ~ r + p_s, data=res)
summary(fm)

# Fit a logistic model
fm2 = glm(is.completed ~ r + p_s, data=res, family=binomial)
summary(fm2)
