# Load required libraries
library(phytools)
library(viridis)

# Read a phylogenetic tree from a file
bru.tree<-read.tree("N.tre")

# Check the structure of the tree object
str(bru.tree)
plotTree(bru.tree,type="fan",ftype="i")

# Check if the tree is ultrametric (all tips equidistant from root)
is.ultrametric(bru.tree)
force.ultrametric(bru.tree)

# Attempt to force the tree to be ultrametric
tree<-bru.tree

# Reassign tree to new object
tree <- ladderize(tree, right = TRUE)

plot(tree)

# Delete tip labels without cannibalism records
todelete <- c("Typhlopidae","Gerrhopilidae","Leptotyphlopidae",
              "Anomalepididae","Bolyeriidae","Xenopeltidae",
              "Loxocemidae","Calabariidae","Charinidae",
              "Ungaliophiidae","Candoiidae","Cylindrophiidae",
              "Uropeltidae","Acrochordidae","Xenodermidae",
              "Pareidae","Homalopsidae","Cyclocoridae",
              "Pseudoxenodontidae","Sibynophiidae","Calamariidae",
              "Calamariidae","Grayiidae")

# Create a tree with only cannibal tips
pt0 <- drop.tip(tree, todelete)
plot(pt0)
write.tree(pt0,file="Cannibals.tre")

# Read cannibalism records
Y<-read.csv("OC.csv",row.names=1)

numbers<-as.matrix(Y)[,1]

# Fit fastAnc
fit<-fastAnc(pt0,numbers,vars=TRUE,CI=TRUE)
fit

# Make a contmap object
obj<-contMap(pt0,numbers,plot=FALSE)
plot(obj,legend=0.7*max(nodeHeights(pt0)),
     fsize=c(0.7,0.9))

# Plot palette 1
viridis.cMap<-setMap(obj,viridisLite::plasma(n=8))
plot(viridis.cMap,fsize=c(1.1,1),leg.txt="Percentage of ophiophagy")

# Plot palette 2
viridis.cMap<-setMap(obj,viridisLite::viridis(n=8))
plot(viridis.cMap,fsize=c(1.1,1),leg.txt="Diversity")
