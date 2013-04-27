require(igraph)
require(mosaic)
library(manipulate)
trellis.par.set(theme = col.mosaic())
library(devtools)
load_all()


####################################################

ccg = ccg.game(40, p=0.5, r=0.1)
ccg = init.mapping(ccg)
plot(ccg)
plot(ccnpk(ccg, alg="random"))
plot(ccnpk(ccg, alg="greedy-dumb"))
plot(ccnpk(ccg, alg="greedy-smart"))
plot(ccnpk(ccg, alg="greedy"))
plot(ccnpk(ccg, alg="opt"))


####################################################

# Test the effectiveness of algorithms
alg.test = function(n_s) {
  p_s = 0.5
  n_t = n_s
  ccg = ccg.game(n1=n_s, p1=p_s, n2 = n_t, r=0.1, capacity = 3)
  ccg = init.mapping(ccg)
  return(c(n_s = n_s, p_s = p_s, n_t = n_t
           , size = n_s * n_t
           , collective.expertise = collaboration(ccg$g1)
           , social.density = ecount(ccg$g1) / choose(vcount(ccg$g1), 2)
           , max.task.difficulty = max(V(ccg$g2)$difficulty)
           , random = ecount(ccnpk(ccg, alg="random")$R)
           , greedy.dumb = ecount(ccnpk(ccg, alg="greedy-dumb")$R)
           , greedy.smart = ecount(ccnpk(ccg, alg="greedy-smart")$R)
           , greedy = ecount(ccnpk(ccg, alg="greedy")$R)
#           , opt = ecount(ccnpk(ccg, alg="opt")$R)
  ))
}

alg.test.many = function (n_s, numTrials) {
  return(do(numTrials) * alg.test(n_s))
}

numTrials = 10
ds = rbind(alg.test.many(n_s = 5, numTrials)
         , alg.test.many(n_s = 8, numTrials)
         , alg.test.many(n_s = 10, numTrials)
         , alg.test.many(n_s = 20, numTrials)
    )

ds.long = reshape(ds, varying=8:11, v.names="numEdges"
        , timevar = "alg", times = names(ds)[8:11], direction="long")
xyplot(numEdges ~ jitter(size), groups=alg, data=ds.long, type=c("p", "r")
       , xlab="Size of Instance (Researchers * Tasks)"
       , ylab="Number of Edges Added"
       , auto.key=list(columns=2))
