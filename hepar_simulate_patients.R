# Bayesian Network - data simulation
library(bnlearn)
library(dplyr)

dir_hepar2="yur/path/hepar2.rda" # https://www.bnlearn.com/bnrepository/discrete-large.html#hepar2
set.seed(1)
bn <- load(dir_hepar2) # open manually 

g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(bn))
g=graphviz.plot(bn, shape='ellipse', layout = "fdp")
graph::nodeRenderInfo(g) <- list(fontsize=50)
Rgraphviz::renderGraph(g)


plotD3bn <- function(bn) {
  varNames <- nodes(bn)
  # Nodes should be zero indexed!
  links <- data.frame(arcs(bn)) %>% mutate(from = match(from, varNames)-1, to = match(to, varNames)-1, value = 1)
  
  nodes <- data.frame(name = varNames) %>% mutate(group = 1, size = 30)
  
  networkD3::forceNetwork(
    Links = links,  
    Nodes = nodes,
    Source = "from",
    Target = "to",
    Value = "value",
    NodeID = "name",
    Group = "group",
    fontSize = 20,
    zoom = TRUE,
    arrows = TRUE,
    bounded = TRUE,
    opacityNoHover = 1
  )
}

# Sampling
# Note that the order of the parameters is inverted from the R sample functions
sampleHepa <- rbn(x = bn, n = 10000)
head(sampleHepa)
write.csv(sampleHepa, file ='your/path/HEPAR_simulated_patients.tsv')


# changing distribution: sex, age,  hospital
newprob_sex <- matrix(c(0.4, 0.6), byrow=TRUE, ncol=2)
colnames(newprob_sex) = c('female', 'male')
bn$sex=as.table(newprob_sex)

newprob_age = matrix(c(0.1, 0.5, 0.3, 0.1), byrow=T, ncol=4)
colnames(newprob_age) <- c('age65_100', 'age51_65', 'age31_50', 'age0_30')
bn$age = as.table(newprob_age)

newprob_hospital = matrix(c(0.65, 0.35), byrow=T, ncol=2)
colnames(newprob_hospital) <- c('present', 'absent')
bn$hospital = as.table(newprob_hospital)



# simulat
sampleHepa <- rbn(x = bn, n = 500)

write.csv(sampleHepa, file ='/your/path/HEPAR_simulated_patients_ood.tsv')
