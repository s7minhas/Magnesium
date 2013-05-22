## Zachary's karate club
g <- graph.famous("Zachary")

## We put everything into a big 'try' block, in case 
## igraph was compiled without GLPK support

## The calculation only takes a couple of seconds
oc <- optimal.community(g)

## Double check the result
print(modularity(oc))
print(modularity(g, membership(oc)))

## Compare to the greedy optimizer
fc <- fastgreedy.community(g)
print(modularity(fc))