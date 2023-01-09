################################################################
#Replication#
#Blood Is Thicker Than Water: Elite Kinship Networks and State Building in Imperial China#

#American Political Science Review#
  
#Yuhua Wang#
#Department of Government#
#Harvard University#
#yuhuawang@fas.harvard.edu#
  
#Created on May 14, 2019#
#This version: November 15, 2021#
#R version 4.0.0#
################################################################

################################################################
#This R script includes codes that replicate Figures 1 and 4 in the main text and Appendix Figures A1-2, A1-3, A1-4, and A1-5#
#For all the other tables and figures, please see Wang APSR Master Do File.do#
################################################################

################################
#FIGURE 1. Examples of Kinship Networks#
################################

###############################
#FIGURE 1 (a): A Dispersed Kinship Network (Wang Anshi's Kinship Network)#
##############################

rm(list = ls())
library(igraph)
library(statnet)
library("network")

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

#read in egelist
rawedgeswang<-read.csv("Wang Anshi edgelist.csv")

#turn edgelist into adjacency matrix
matrixwang <-as.matrix(get.adjacency(graph.data.frame(rawedgeswang)))

#write in a csv file
write.csv(matrixwang,"Wang Anshi adjmatrix.csv", row.names = FALSE)

#Read in Adjacency Matrix

net.adj.wang <- as.matrix(matrixwang) 

# load in vertex attributes

#orignal coordinates
nodeInfoWANG <- read.csv(file="Wang node attributes.csv",header=TRUE,stringsAsFactors=FALSE)

print(net.adj.wang) # peek at matrix 
print(nodeInfoWANG)  # peek at attribute data

# create undirected network object from matrix

net_wang<-network(net.adj.wang, directed=FALSE, matrix.type="adjacency")

# it read in vertex names from matrix col names ...
network.vertex.names(net_wang)

# ATTACHING VERTEX ATTRIBUTES

# load in other attributes 
net_wang%v%"lon" <- nodeInfoWANG$x
net_wang%v%"lat" <- nodeInfoWANG$y

# Note: order of attributes in the data frame MUST match vertex ids
# otherwise the attribute will get assigned to the wrong vertex

# check that they got loaded
list.vertex.attributes(net_wang)

summary(net_wang)

gden(net_wang)

detach("package:igraph")
degwang <- degree(net_wang,gmode="graph")

# if the degree command doesn't work, detach the igraph package using the following
#detach("package:igraph")

#rescale the size of the nodes#

rescale<-function(nchar,low,high){
  min_d<-min(nchar)
  max_d<-max(nchar)
  rscl<-((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

#A peek of Wang Anshi's kinship network#

op<-par(mar=c(0,0,0,0))
plot(net_wang,vertex.cex=rescale(degwang,0.5,3))
par(op)

###############################
#FIGURE 1 (b): A (Relatively) Concentrated Kinship Network (Lv Gongzhu's Kinship Network)#
##############################

library(igraph)
library(statnet)
library("network")

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

#read in egelist
rawedgeslv<-read.csv("Lv Gongzhu edgelist.csv")

#turn edgelist into adjacency matrix
matrixlv <-as.matrix(get.adjacency(graph.data.frame(rawedgeslv)))

#write in a csv file
write.csv(matrixlv,"Lv Gongzhu adjmatrix.csv", row.names = FALSE)

#Read in Adjacency Matrix

net.adj.lv <- as.matrix(matrixlv) 

# load in vertex attributes

#orignal coordinates
nodeInfoLV <- read.csv(file="Lv node attributes.csv",header=TRUE,stringsAsFactors=FALSE)

print(net.adj.lv) # peek at matrix 
print(nodeInfoLV)  # peek at attribute data

# create undirected network object from matrix

net_lv<-network(net.adj.lv, directed=FALSE, matrix.type="adjacency")

# it read in vertex names from matrix col names ...
network.vertex.names(net_lv)

# ATTACHING VERTEX ATTRIBUTES

# load in other attributes 
net_lv%v%"lon" <- nodeInfoLV$x
net_lv%v%"lat" <- nodeInfoLV$y

# Note: order of attributes in the data frame MUST match vertex ids
# otherwise the attribute will get assigned to the wrong vertex

# check that they got loaded
list.vertex.attributes(net_lv)

summary(net_lv)

gden(net_lv)

detach("package:igraph")
deg <- degree(net_lv,gmode="graph")

# if the degree command doesn't work, detach the igraph package using the following
#detach("package:igraph")

#rescale the size of the nodes#
rescale<-function(nchar,low,high){
  min_d<-min(nchar)
  max_d<-max(nchar)
  rscl<-((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}

#A peek of Lv Gongzhu's kinship network#

op<-par(mar=c(0,0,0,0))
plot(net_lv,vertex.cex=rescale(deg,0.5,3))
par(op)

#######################
# Add Song map
#######################

library('maps')
library('geosphere')
library(maptools)
library(foreign)
library(PBSmapping)
library(network)
library(maps)
library(statnet)
require(rgdal)

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

song <- importShapefile("songprovinces1080_gr8.shp")

proj.abbr <- attr(song, "projection")

#Creat the Song map#
plotPolys(song, projection=proj.abbr, border="grey",
          xlab="", ylab="",col="#f5f5f2", tckLab = FALSE,axes=FALSE)

#Layer on Wang Anshi's kinship network (Figure 1 (a))#
plot.network(net_wang,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(net_wang%v%"lon",net_wang%v%"lat"),  
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             vertex.col="black",
             vertex.cex=rescale(degwang,0.5,3),
             vertex.border='black',
             edge.col="grey",
             # please don't jitter the points around
             jitter=FALSE)

#output pdf in 6.66*7.46

#Creat the Song map#
plotPolys(song, projection=proj.abbr, border="grey",
          xlab="", ylab="",col="#f5f5f2", tckLab = FALSE,axes=FALSE)

#Layer on Lv Gongzhu's kinship network (Figure 1 (b))#
plot.network(net_lv,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(net_lv%v%"lon",net_lv%v%"lat"),  
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             vertex.cex=rescale(deg,1,4),
             vertex.col="black",
             vertex.border='black',
             edge.col="grey",
             #usecurve=TRUE,
             #edge.curve = 0.001,
             # please don't jitter the points around
             jitter=FALSE)

#output pdf in 6.66*7.46

################################
#FIGURE 2. Example of a Kinship Network#
################################

#Constructed in LaTex using tikz

################################
#FIGURE 3. Tomb Epitaph Example
################################

#Photos

################################
#FIGURE 4. Major Politicians’ Attitudes toward the State-Building Reform#
################################

rm(list = ls())
library(foreign) 
library(ggplot2)
library(ggrepel) 
library(plyr)
require(scales)

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

politician <- read.dta("R graph data whole sample.dta")

attach(politician)

support <- ggplot(politician, aes(x = support_continuous)) +  
  geom_histogram(aes(y = (..count..)/sum(..count..)), color="black", fill="grey") + 
  scale_y_continuous(name="",labels = scales::percent,limits=c(0,0.6), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6))+
  scale_x_continuous(name="Support for Reform", limits=c(-0.05,1.05), breaks=c(0,0.5,1))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 20))

support

#To save the graph in PDF
#ggsave(file="support.pdf", support, width = 8, height = 6, dpi = 150, units = "in", device='pdf')

################################
#Figure A1-2: Major Politicians’ Local Concentration of Kin (Estimating Sample)#
################################

rm(list = ls())
library(foreign) 
library(ggplot2)
library(ggrepel) 
library(plyr)
require(scales)

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

politician_est <- read.dta("R graph data estimating sample.dta")

attach(politician_est)

kinproximity_est <- ggplot(politician_est, aes(x = kinproximity)) +  
  geom_histogram(aes(y = (..count..)/sum(..count..)), color="black", fill="grey") + 
  scale_y_continuous(name="",labels = scales::percent,limits=c(0,0.12), breaks=c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  scale_x_continuous(name="Local Concentration of Kin", limits=c(0,40), breaks=c(0,10,20,30,40))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 20,family = "Palatino"),
    axis.text.x = element_text(size = 15,family = "Palatino"),
    axis.text.y = element_text(size = 15,family = "Palatino"),
    axis.title.y = element_text(size = 20,family = "Palatino"))

kinproximity_est

#To save the graph in PDF
#ggsave(file="kinproximity_est.pdf", kinproximity_est, width = 8, height = 6, dpi = 150, units = "in", device='pdf')

################################
#Figure A1-3: Major Politicians’ Attitudes toward the State-Building Reform (Estimating Sample)#
################################

rm(list = ls())
library(foreign) 
library(ggplot2)
library(ggrepel) 
library(plyr)
require(scales)

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

politician_est <- read.dta("R graph data estimating sample.dta")

attach(politician_est)

support_est <- ggplot(politician_est, aes(x = support_continuous)) +  
  geom_histogram(aes(y = (..count..)/sum(..count..)), color="black", fill="grey") + 
  scale_y_continuous(name="",labels = scales::percent,limits=c(0,0.55), breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  scale_x_continuous(name="Support for Reform", limits=c(-0.05,1.05), breaks=c(0,0.5,1))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 20,family = "Palatino"),
    axis.text.x = element_text(size = 15,family = "Palatino"),
    axis.text.y = element_text(size = 15,family = "Palatino"),
    axis.title.y = element_text(size = 20,family = "Palatino"))

support_est

#To save the graph in PDF
#ggsave(file="support_est.pdf", support_est, width = 8, height = 6, dpi = 150, units = "in", device='pdf')

##################################################
#Figure A1-4: Correlations between Politicians; Ranks and Attitudes#
##################################################

rm(list = ls())
library(foreign) 
library(ggplot2)
library(ggrepel) 
library(plyr)
require(scales)
library("GGally")

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

rank <- read.dta("R graph data rank.dta")

attach(rank)

globalpoi <- dplyr::select(rank, all_of(c("support_continuous", "rank_change", "rank_first", "rank_ave", "rank_max")))

rankcorr <- ggpairs(globalpoi,
                    columnLabels = c("Support for reform", "Rank change","First rank", "Average rank", "Highest rank"), 
                    axisLabels = "none", upper = list(continuous = wrap("cor", size = 4)), 
                    lower = list(continuous = wrap("points", alpha = 0.3,    size=0.6)))+
  theme_bw(base_size = 10)

rankcorr

#To save the graph in PDF
#ggsave(file="rankcorr.pdf", rankcorr, width = 6.46, height = 6.66, dpi = 150, units = "in", device='pdf')

##################################################
#Figure A1-5: Northern Song Politicians Marriage Network, 1067–1085 CE#
##################################################

rm(list = ls())
library(statnet)
library("network")

set.seed(12345)

#change this to your directory#
setwd("/Users/houruize/Desktop/dataverse_files")

#Read in Wang Anshi Reform's Major Officials' Adjacency Matrix

net.adj.child3<-read.csv(file="major officials adj matrix.csv",header=FALSE,stringsAsFactors=FALSE)

net.adj.child3 <- as.matrix(net.adj.child3) 

# load in vertex attributes
nodeInfoCHILD3 <- read.csv(file="vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)

print(net.adj.child3) # peek at matrix 
print(nodeInfoCHILD3)  # peek at attribute data

# Since our relational data has no row/column names, let's set them now
rownames(net.adj.child3) <- nodeInfoCHILD3$name_pinyin
colnames(net.adj.child3) <- nodeInfoCHILD3$name_pinyin

print(net.adj.child3)

# create undirected network object from matrix

net_reformofficial_child3<-network(net.adj.child3, directed=FALSE, matrix.type="adjacency")

# it read in vertex names from matrix col names ...
network.vertex.names(net_reformofficial_child3)

# ATTACHING VERTEX ATTRIBUTES

# ... but could also set vertex.names with 
net_reformofficial_child3%v%'vertex.names'<- nodeInfoCHILD3$name_pinyin

# load in other attributes 
net_reformofficial_child3%v%"support" <- nodeInfoCHILD3$policy_opinion_tri

# Note: order of attributes in the data frame MUST match vertex ids
# otherwise the attribute will get assigned to the wrong vertex

# check that they got loaded
list.vertex.attributes(net_reformofficial_child3)

summary(net_reformofficial_child3)

gden(net_reformofficial_child3)

# make my color palette

library(RColorBrewer) 

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

display.brewer.all()
display.brewer.pal(3, "RdYlGn")

my_pal<-brewer.pal(3,"RdYlGn") 

# assign different colors to supports and opponents
detach("package:igraph", unload=TRUE)
rolecat_child3<-as.factor(get.vertex.attribute(net_reformofficial_child3,"support"))

#if the as.factor command doesn't work, detach the igraph package first using the following:
#detach("package:igraph", unload=TRUE)#

# centrality measures

#Degree centrality#
deg3_child<-degree(net_reformofficial_child3,gmode="graph")
deg3_child

deg3_child.data<-data.frame(deg3_child)
deg3_child.data

#Betweenness centrality#
btw3_child<-betweenness(net_reformofficial_child3,gmode="graph")
btw3_child

btw3_child.data<-data.frame(btw3_child)
btw3_child.data

#Bonacich centrality#
bonacich3_child<-bonpow(net_reformofficial_child3,gmode="graph")
bonacich3_child

bonacich3_child.data<-data.frame(bonacich3_child)
bonacich3_child.data

#Transitivity
trans3_child <- gtrans(net_reformofficial_child3)
trans3_child

#Density
cent3_child <- gden(net_reformofficial_child3)
cent3_child

#Clique
clique3_child <- clique.census(net_reformofficial_child3)
clique3_child

clique3_child$cliques[3]
clique3_child$cliques[4]
clique3_child$cliques[5]
clique3_child$cliques[6]
clique3_child$cliques[10]

clique3_child$clique.count[,1]

#closeness
detach("package:igraph", unload=TRUE)
closeness3_child <- closeness(net_reformofficial_child3)
closeness3_child

#evcent
#detach("package:igraph", unload=TRUE)
evcent3_child <- evcent(net_reformofficial_child3)
evcent3_child

# Network graph

rolelab_child3<-get.vertex.attribute(net_reformofficial_child3,"support")

library(igraph)
library(gcookbook)

rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low 
  rscl}

op<-par(mar=c(0,0,0,0))
gplot(net_reformofficial_child3, gmode="graph", mode='fruchtermanreingold', displaylabels=F,
      pad=0.4, vertex.cex=rescale(btw3_child,1,4), edge.col="grey90",
      vertex.col=addalpha(my_pal[rolecat_child3], 0.4))
legend("topleft",legend=c("Oppose","Unkown","Support"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n")
par(op)

############################################################
#The End#
############################################################