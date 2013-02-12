#
# Demonstration of the package
#
#############################################################

require(igraph)
n = 20
p = 1/2
g1 = erdos.renyi.game(n, p)
g2 = erdos.renyi.game(n, p)
cg = compgraph(g1, g2, name="myCompGraph")

summary(cg)
plot(cg)

#############################################################

# Composite Task Graph

g = ccg.game(10, 0.5)
plot(g)
is.completed(g, 1)