MEBF<-function(MAT,DIM=1000,Thres,P=0.05){
  
  if(min(ncol(MAT),nrow(MAT))<DIM){
    DIM<-min(ncol(MAT),nrow(MAT))-1
  }
  
  m1<-MAT
  SUM<-sum(MAT)
  MAT_B<-NULL
  MAT_C<-NULL
  for (i in 1:DIM) {
    
    if(sum(m1)<=P*SUM){
      result<-list()
      result[[1]]<-MAT_B
      result[[2]]<-MAT_C
      return(result)
    }
    
    C1<-0
    B1<-rep(0,nrow(m1))
    B1_use<-B1
    B2<-rep(0,ncol(m1))
    B2_use<-B2
    COL<-colSums(m1)
    ROW<-rowSums(m1)
    
    # START with column
    if(median(COL[COL>0])>1){
      TEMP<-which(colSums(m1)==min(COL[which(COL>=median(COL[COL>0]))]))
      if(length(TEMP)==1){
        B1<-m1[,TEMP]
        B2[which(colSums(m1[which(B1==1),])>=min(Thres*sum(B1)+1,sum(B1)))]<-1
        C2<-(sum(B1)-1)*(sum(B2)-1)-sum(m1[which(B1==1),which(B2==1)]==0)
        if(C2>C1){
          C1<-C2
          B1_use<-B1
          B2_use<-B2
        }
        B1<-rep(0,nrow(m1))
        B2<-rep(0,ncol(m1))
      }else{
        for(j in 1:length(TEMP)){
          B1<-m1[,TEMP[j]]
          B2[which(colSums(m1[which(B1==1),])>=min(Thres*sum(B1)+1,sum(B1)))]<-1
          C2<-(sum(B1)-1)*(sum(B2)-1)-sum(m1[which(B1==1),which(B2==1)]==0)
          if(C2>C1){
            C1<-C2
            B1_use<-B1
            B2_use<-B2
          }
          B1<-rep(0,nrow(m1))
          B2<-rep(0,ncol(m1))
        }
      }
    }
    
    # START with ROW
    if(median(ROW[ROW>0])>1){
      TEMP<-which(rowSums(m1)==min(ROW[ROW>=median(ROW[ROW>0])]))
      if(length(TEMP)==1){
        B2<-m1[TEMP,]
        B1[which(rowSums(m1[,which(B2==1)])>=min(Thres*sum(B2)+1,sum(B2)))]<-1
        C2<-(sum(B1)-1)*(sum(B2)-1)-sum(m1[which(B1==1),which(B2==1)]==0)
        if(C2>C1){
          C1<-C2
          B1_use<-B1
          B2_use<-B2
        }
        B1<-rep(0,nrow(m1))
        B2<-rep(0,ncol(m1))
      }else{
        for(j in 1:length(TEMP)){
          B2<-m1[TEMP[j],]
          B1[which(rowSums(m1[,which(B2==1)])>=min(Thres*sum(B2)+1,sum(B2)))]<-1
          C2<-(sum(B1)-1)*(sum(B2)-1)-sum(m1[which(B1==1),which(B2==1)]==0)
          if(C2>C1){
            C1<-C2
            B1_use<-B1
            B2_use<-B2
          }
          B1<-rep(0,nrow(m1))
          B2<-rep(0,ncol(m1))
        }
      }
    }
    
    if(C1==0){
      ROW<-order(rowSums(m1),decreasing = T)
      COL<-order(colSums(m1),decreasing = T)
      # start from ROW
      B1_1<-rep(0,nrow(m1))
      if(length(which(rowSums(m1[,COL[1:2]])==2))>1){
        B1_1[which(rowSums(m1[,COL[1:2]])==2)]<-1
        B1_2<-rep(0,ncol(m1))
        B1_2[colSums(m1[which(B1_1==1),])>=min((Thres*sum(m1[which(B1_1==1),COL[1]])+1),sum(m1[which(B1_1==1),COL[1]]))]<-1
        C1<-(sum(B1_1)-1)*(sum(B1_2)-1)-sum(m1[which(B1_1==1),which(B1_2==1)]==0)
      }else{
        C1<-(-Inf)
      }
      
      # start from COL
      B2_2<-rep(0,ncol(m1))
      if(length(which(colSums(m1[ROW[1:2],])==2))>1){
        B2_2[which(colSums(m1[ROW[1:2],])==2)]<-1
        B2_1<-rep(0,nrow(m1))
        B2_1[rowSums(m1[,which(B2_2==1)])>=min(Thres*sum(m1[ROW[1],which(B2_2==1)])+1,sum(m1[ROW[1],which(B2_2==1)]))]<-1
        C2<-(sum(B2_1)-1)*(sum(B2_2)-1)-sum(m1[which(B2_1==1),which(B2_2==1)]==0)
      }else{
        C2<-(-Inf)
      }
      
      if((C1==(-Inf))&(C2==(-Inf))){
        break
      }else{
        if(C1>C2){
          B1_use<-B1_1
          B2_use<-B1_2
          m1[which(B1_1==1),which(B1_2==1)]<-0
        }else{
          B1_use<-B2_1
          B2_use<-B2_2
          m1[which(B2_1==1),which(B2_2==1)]<-0
        }
      }
    }
    MAT_B<-cbind(MAT_B,B1_use)
    MAT_C<-rbind(MAT_C,B2_use)
    m1[which(MAT_B[,i]==1),which(MAT_C[i,]==1)]<-0
  }
  result<-list()
  result[[1]]<-MAT_B
  result[[2]]<-MAT_C
  return(result)
}
