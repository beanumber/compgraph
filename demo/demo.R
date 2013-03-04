#
# Demonstration of the package
#
#############################################################

require(igraph)
require(mosaic)
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
ccgplot(g)
is.completed(g)


# Baby example
E.social = c(1,2, 1,3, 1,4, 2,4)
g1 = graph(edges = E.social, directed=FALSE)
V(g1)$expertise = c(3,1,4,1)

E.task = c(1,2, 1,3, 2,4, 3,5, 3,6)
g2 = graph(edges = E.task, directed=TRUE)
V(g2)$difficulty = c(7,5,2,1,1,2)

g = compgraph(g1, g2, "SmallTask")
ccgplot(g)

is.solvable(g, 1)
is.completed(g, 1)

##############################################################

# With no restrictions, tasks are independent
# so consider only a single task

ccg = ccg.game(n1=10, p1=0.2, n2=1, r=0.5)
ccgplot(ccg)

# Sweep of parameter space
n = 100
p = 0.9
r = 0.1
numTrials = 20
ds = do(numTrials) * is.completed(ccg.game(n1=n, p1=p, n2=1, r=r))
sum(ds$result)

# More carefully
test = function(n, p, r) {
  ccg = ccg.game(n1=n, p1=p, n2 = 1, r=r)
  return(c(n1 = n, p1 = p, r = r
           , social.density = ecount(ccg$g1) / choose(vcount(ccg$g1), 2)
           , mapping.density = ecount(ccg$R) / (vcount(ccg$g1) * vcount(ccg$g2))
           , is.completed = is.completed(ccg)))
}

test.many = function (n, p, r, numTrials) {
  return(do(numTrials) * test(n,p, r))
}
test.many(n,p, r, numTrials)

# Try all the r's
rs = seq(from=0.1, to=1, by=0.05)
res.mat = do.call("rbind", sapply(rs, test.many, n=n, p=p, numTrials=numTrials))
dim(res.mat) = c(6, length(rs) * numTrials)
res = data.frame(t(res.mat))
names(res) = c("n", "p", "r", "social.density", "mapping.density", "is.completed")

xyplot(jitter(is.completed) ~ r, data=res, alpha=0.3, pch=19, type=c("p", "smooth"))
favstats(is.completed ~ r, data=res)
