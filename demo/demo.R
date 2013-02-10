#
# Demonstration of the package
#
#############################################################

n = 20
p = 1/2
g1 = erdos.renyi.game(n, p)
g2 = erdos.renyi.game(n, p)
cg = compgraph(g1, g2, name="myCompGraph")
summary(cg)
