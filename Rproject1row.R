##Choose the file
f=file.choose()
f1=read.csv(f)
data<-f1
data
##array for mean of normalized data of each student
mean_data<-data.frame()
##Calculating mean
for(i in 1:nrow(data))
{
  s<-0
  m<-0
  for(j in 2:(ncol(data)-1))
  {
    s<-s+data[i,j]
  }
  m<-s/(ncol(data)-2)
  mean_data[i,1]<-m
}
colnames(mean_data)<-c("Mean of each student")
mean_data
##Difference between the calculated mean and grade of each subject
diff_data<-data.frame(USN=c(data[,1]),MA11=0,PY12=0,MEL19=0,EC15=0,CV14=0,ME13=0,PYL18=0,CS26=0,CY22=0,ME29=0,CYL27=0,EE25=0,MA21=0,HS24=0,AL21=0,CSL28=0)
for(i in 1:nrow(data))
{
  for(j in 2:ncol(data))
  {
    diff_data[i,j]<-round(abs(mean_data[i,1]-data[i,j]))
  }
}
##Golden Rule mean_data <3
subset <- diff_data
subset[,]=matrix(ncol=ncol(subset), rep(NA, prod(dim(subset))))
subset[,1]<-data[,1]
for(j in 2:ncol(diff_data))
{
  for(i in 1:nrow(data))
  {
    if(diff_data[i,j]<=3)
    {
      subset[i,j]=1;
    }
    else
    {
      subset[i,j]=0;
    }
  }
}

##Combination of two subjects
comb_two0 <- data.frame()
comb_two1 <- data.frame()
comb_two2 <- data.frame()
comb_two3 <- data.frame()
x <- c("Number Of Combinations")
no_row0<-0
no_row1<-0
no_row2<-0
no_row3<-0
##30% is taken as minimum support value
minsup<-as.integer(0.3*nrow(diff_data))
for(a in 2:(ncol(diff_data)-1))
{
  for(b in (a+1):ncol(diff_data))
  {
    comb0<-0
    comb1<-0
    comb2<-0
    comb3<-0
    for(k in 1:nrow(data))
    {
      if(diff_data[k,a]==0 && diff_data[k,b]==0)
      {
        comb0<-comb0+1
        
        
      }else if(diff_data[k,a]==1 && diff_data[k,b]==1)
      {
        comb1<-comb1+1
        
      }else if(diff_data[k,a]==2 && diff_data[k,b]==2)
      {
        comb2<-comb2+1
        
        
      }else if(diff_data[k,a]==3 && diff_data[k,b]==3)
      {
        comb3<-comb3+1
        
      }
      
    }
    if(comb0>=minsup)
    {
      no_row0<-no_row0+1 
      comb_two0[no_row0,1]<-comb0
      colnames(comb_two0) <- x
      rownames(comb_two0)[no_row0] <-paste(a,b,sep="&")
    }
    if(comb1>=minsup)
    {
      no_row1<-no_row1+1 
      comb_two1[no_row1,1]<-comb1
      colnames(comb_two1) <- x
      rownames(comb_two1)[no_row1] <-paste(a,b,sep="&")
    }
    if(comb2>=minsup)
    {
      no_row2<-no_row2+1 
      comb_two2[no_row2,1]<-comb2
      colnames(comb_two2) <- x
      rownames(comb_two2)[no_row2] <-paste(a,b,sep="&")
    }
    if(comb3>=minsup)
    {
      no_row3<-no_row3+1 
      comb_two3[no_row3,1]<-comb
      colnames(comb_two0) <- x
      rownames(comb_two3)[no_row3] <-paste(a,b,sep="&")
    }
  }
}

