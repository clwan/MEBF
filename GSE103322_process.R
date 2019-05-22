library(Rtsne)
cl <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC7979")


#### original t-SNE ####
tsne<-Rtsne(t(File_data), dims = 2, perplexity=30, verbose=TRUE, max_iter = 20000)

#### MEBF t-SNE ####
File_use<-apply(File_data>0, c(1,2), as.numeric)
FILE_result<-MEBF(File_use,Thres = 0.6,DIM=5)
File_use_2<-File_use*(apply((FILE_result[[1]]%*%FILE_result[[2]]), c(1,2), as.numeric))
REMOVE<-grep("T",duplicated(t(File_use_2)))
MG_tsne<-Rtsne(t(File_use_2[,-REMOVE]), dims = 2, perplexity=30, verbose=TRUE, max_iter = 20000)
META4<-META1[-REMOVE]



#### visulization ####
tsne_use<-tsne
tsne_use$Y<-tsne$Y[-REMOVE,]
plot(x=tsne_use$Y[which(META4=="Malignant"),1],
     y=tsne_use$Y[which(META4=="Malignant"),2],
     pch=18,cex=0.7,col=cl[9],
     xlim=c(min(tsne_use$Y)-2,max(tsne_use$Y)+2),
     ylim=c(min(tsne_use$Y)-2,max(tsne_use$Y)+2),,
     frame.plot = F,
     xlab = "t-SNE 1",
     ylab = "t-SNE 2",
     main="original")
points(x=tsne_use$Y[which(META4=="B cell"),1],
       y=tsne_use$Y[which(META4=="B cell"),2],
       pch=18,cex=0.7,col=cl[1])
points(x=tsne_use$Y[which(META4=="myocyte"),1],
       y=tsne_use$Y[which(META4=="myocyte"),2],
       pch=18,cex=0.7,col=cl[2])
points(x=tsne_use$Y[which(META4=="Macrophage"),1],
       y=tsne_use$Y[which(META4=="Macrophage"),2],
       pch=18,cex=0.7,col=cl[4])
points(x=tsne_use$Y[which(META4=="Endothelial"),1],
       y=tsne_use$Y[which(META4=="Endothelial"),2],
       pch=18,cex=0.7,col=cl[5])
points(x=tsne_use$Y[which(META4=="Dendritic"),1],
       y=tsne_use$Y[which(META4=="Dendritic"),2],
       pch=18,cex=0.7,col=cl[8])
points(x=tsne_use$Y[which(META4=="Mast"),1],
       y=tsne_use$Y[which(META4=="Mast"),2],
       pch=18,cex=0.7,col=cl[7])
points(x=tsne_use$Y[which(META4=="Fibroblast"),1],
       y=tsne_use$Y[which(META4=="Fibroblast"),2],
       pch=18,cex=0.7,col=cl[6])
points(x=tsne_use$Y[which(META4=="T cell"),1],
       y=tsne_use$Y[which(META4=="T cell"),2],
       pch=18,cex=0.7,col=cl[3])



plot(x=MG_tsne$Y[which(META4=="Malignant"),1],
     y=MG_tsne$Y[which(META4=="Malignant"),2],
     pch=18,cex=0.7,col=cl[9],
     xlim=c(min(MG_tsne$Y)-2,max(MG_tsne$Y)+2),
     ylim=c(min(MG_tsne$Y)-2,max(MG_tsne$Y)+2),,
     frame.plot = F,
     xlab = "t-SNE 1",
     ylab = "t-SNE 2",
     main = "MEBF")
points(x=MG_tsne$Y[which(META4=="B cell"),1],
       y=MG_tsne$Y[which(META4=="B cell"),2],
       pch=18,cex=0.7,col=cl[1])
points(x=MG_tsne$Y[which(META4=="myocyte"),1],
       y=MG_tsne$Y[which(META4=="myocyte"),2],
       pch=18,cex=0.7,col=cl[2])
points(x=MG_tsne$Y[which(META4=="Macrophage"),1],
       y=MG_tsne$Y[which(META4=="Macrophage"),2],
       pch=18,cex=0.7,col=cl[4])
points(x=MG_tsne$Y[which(META4=="Endothelial"),1],
       y=MG_tsne$Y[which(META4=="Endothelial"),2],
       pch=18,cex=0.7,col=cl[5])
points(x=MG_tsne$Y[which(META4=="Dendritic"),1],
       y=MG_tsne$Y[which(META4=="Dendritic"),2],
       pch=18,cex=0.7,col=cl[8])
points(x=MG_tsne$Y[which(META4=="Mast"),1],
       y=MG_tsne$Y[which(META4=="Mast"),2],
       pch=18,cex=0.7,col=cl[7])
points(x=MG_tsne$Y[which(META4=="Fibroblast"),1],
       y=MG_tsne$Y[which(META4=="Fibroblast"),2],
       pch=18,cex=0.7,col=cl[6])
points(x=MG_tsne$Y[which(META4=="T cell"),1],
       y=MG_tsne$Y[which(META4=="T cell"),2],
       pch=18,cex=0.7,col=cl[3])



