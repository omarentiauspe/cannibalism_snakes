
library(phytools)
library(viridis)


bru.tree<-read.tree("N.tre")

str(bru.tree)
plotTree(bru.tree,type="fan",ftype="i")

is.ultrametric(bru.tree)
force.ultrametric(bru.tree)

tree<-bru.tree

tree <- ladderize(tree, right = TRUE)

plot(tree)

todelete <- c("Typhlopidae","Gerrhopilidae","Leptotyphlopidae",
              "Anomalepididae","Bolyeriidae","Xenopeltidae",
              "Loxocemidae","Calabariidae","Charinidae",
              "Ungaliophiidae","Candoiidae","Cylindrophiidae",
              "Uropeltidae","Acrochordidae","Xenodermidae",
              "Pareidae","Homalopsidae","Cyclocoridae",
              "Pseudoxenodontidae","Sibynophiidae","Calamariidae",
              "Calamariidae","Grayiidae")

pt0 <- drop.tip(tree, todelete)
plot(pt0)
write.tree(pt0,file="Cannibals.tre")


Y<-read.csv("OC.csv",row.names=1)

numbers<-as.matrix(Y)[,1]

fit<-fastAnc(pt0,numbers,vars=TRUE,CI=TRUE)
fit

obj<-contMap(pt0,numbers,plot=FALSE)
plot(obj,legend=0.7*max(nodeHeights(pt0)),
     fsize=c(0.7,0.9))

viridis.cMap<-setMap(obj,viridisLite::plasma(n=8))
plot(viridis.cMap,fsize=c(1.1,1),leg.txt="Percentage of ophiophagy")


viridis.cMap<-setMap(obj,viridisLite::viridis(n=8))
plot(viridis.cMap,fsize=c(1.1,1),leg.txt="Diversity")
