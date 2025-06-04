setwd("~/cannibal")

library(phytools)
#install.packages("viridis")
library(viridis)


bru.tree<-read.tree("N.tre")

str(bru.tree)
plotTree(bru.tree,type="fan",ftype="i")

is.ultrametric(bru.tree)
force.ultrametric(bru.tree)

tree<-bru.tree


tree <- ladderize(tree, right = TRUE)

plot(tree)


#Read character states#

x<-read.csv("O.csv",row.names=1)
str(x)

#Change into vector#

x<-as.matrix(x)[,1]
str(x)


###

# Fitting Mk models
#Equal rates

snake_er<-fitMk(tree,x,model="ER",
                pi="fitzjohn")

snake_er


snake_ard<-fitMk(tree,x,model="ARD",
                 pi="fitzjohn")

snake_ard

#Symmetric


snake_sym<-fitMk(tree,x,model="SYM",
                 pi="fitzjohn")

snake_sym


#Run ANOVA

anova_result <- anova(snake_er,snake_ard,snake_sym)

write.csv(anova_result, file = "anova_results.csv", 
          row.names = TRUE)

###


snake_er.anc<-ancr(snake_er)
snake_er.anc


snake_ard.anc<-ancr(snake_ard)
snake_ard.anc

snake_sym.anc<-ancr(snake_sym)
snake_sym.anc

snake.avg_anc<-ancr(anova(snake_er,snake_ard,snake_sym))

###


# Plotting trees

cols<-viridisLite::inferno(n= 2, alpha = 1, begin = 0.2, end = 0.7, direction = 1)

#Plotting ER

plot(snake_er.anc, cex=0.8,piecol=cols)
mar=c(0.1,0.1,1.1,0.1)
#mtext("a) ER model marginal ancestral states",line=0,adj=0)

###

# Plotting ARD

plot(snake_ard.anc, cex=0.8,piecol=cols)
     mar=c(0.1,0.1,1.1,0.1)
     #mtext("b) ARD model marginal ancestral states",line=0,adj=0)

# Plotting SYM

plot(snake_sym.anc, cex=0.8,piecol=cols)
mar=c(0.1,0.1,1.1,0.1)
#mtext("c) SYM model marginal ancestral states",line=0,adj=0)
