rm(list=ls())
library(rstudioapi)
library(dplyr)
library(colorspace)
library(ggplot2)
library(gridExtra)
library(grid)
library(matrixStats)
#library(ggpubr)
#library("gridExtra")
#library("cowplot")
library(RColorBrewer)
library(extrafont)
library(BBmisc)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print( getwd() )
colorvec=c("firebrick1","ghostwhite","dodgerblue1")

#reading data
library(gdata)
library(readxl)

colorvec=rev(diverge_hsv(10))
ffdata1 = read.csv('ALL Pairwise.csv')
expmat=as.matrix(ffdata1[,3:10])
hnorm=matrix(0,dim(expmat)[1],dim(expmat)[2])
for(i in 1:dim(expmat)[1]){
  #hnorm[i,]<-(as.numeric(expmat[i,])-mean(as.numeric(expmat[i,])))/sd(as.numeric(expmat[i,]))
  tmp=as.numeric(expmat[i,])
  hnorm[i,]<-(as.numeric(expmat[i,])-min(tmp))/(max(tmp)-min(tmp))
 
}
rownames(expmat)<-ffdata1$ï..GeneId
rownames(hnorm)<-ffdata1$ï..GeneId
colnames(expmat)<-colnames(ffdata1)[3:10]
colnames(hnorm)<-colnames(ffdata1)[3:10]
pdf(file = 'heatmap.pdf', width = 6.85, height = 10)
xM1=pheatmap::pheatmap((hnorm),cluster_cols = T,kmeans_k = 15,cluster_rows = T,scale = 'none',show_rownames = T ,color = colorvec,main ='',silent = F,clustering_distance_rows='correlation',show_colnames = TRUE,fontsize = 12)
#fnn=paste(panel_names[panel],"_hmap.pdf",sep = "")
dev.off()
