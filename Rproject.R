##Choose the file
f=file.choose()
f1=read.csv(f)
data<-f1
data

##file path for combinations.csv to save combinations; file must be empty
filepath<-file.choose()

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

##Count for each subject as per golden rule<3
subjects<- data.frame(matrix(0,ncol=2))
colnames(subjects) <- c("Subject No.","count")
count<-0
no_row<-0
for(i in 2:ncol(diff_data)){
  for(k in 1:nrow(diff_data)){
    if(diff_data[k,i]<3){
      count<-count+1
    }
  }
  no_row<-no_row+1
  subjects[no_row,1]<-paste((i-1))
  subjects[no_row,2]<-count
  count<-0
}
##Acepting the number of combinations
comb<-as.integer(readline(prompt = "Enter the combination number:"))
maxcomb<-13
##Accepting the minimum support cost value
minsup<-as.integer(readline(prompt = "Enter the minimum support cost:"))
if(minsup=="")
{
  minsup<-5 ##default value
}
##Combination of two subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-1))
{
  for(b in (a+1):ncol(diff_data))
  {
    
    for(k in 1:nrow(data))
    {
      if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b])
      {
        count<-count+1
        
      }
    }
    if(count>=minsup){
      no_row=no_row+1
      df[no_row,1]<-paste(a,b,sep="&")
      df[no_row,2]<-count
      count<-0
    }
  }
}
write.csv(df,filepath, row.names = FALSE)
read.csv(filepath)

##Combination of three subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-2))
{
  for(b in (a+1):(ncol(diff_data)-1))
  {
    for(c in (b+1):ncol(diff_data))
    {
      for(k in 1:nrow(data))
      {
        if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]){
          count<-count+1
        }
      }
      if(count>=minsup){
        no_row=no_row+1
        df[no_row,1]<-paste(a,b,c,sep="&")
        df[no_row,2]<-count
        count<-0
      }
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of four subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-3))
{
  for(b in (a+1):(ncol(diff_data)-2))
  {
    for(c in (b+1):(ncol(diff_data)-1))
    {
      for(d in (c+1):ncol(diff_data))
      {
          for(k in 1:nrow(data))
        {
          if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
             && diff_data[k,c]==diff_data[k,d]){
            count<-count+1
          }
        }
        if(count>=minsup){
          no_row=no_row+1
          df[no_row,1]<-paste(a,b,c,d,sep="&")
          df[no_row,2]<-count
          count<-0
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of five subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-4))
{
  for(b in (a+1):(ncol(diff_data)-3))
  {
    for(c in (b+1):(ncol(diff_data)-2))
    {
      for(d in (c+1):(ncol(diff_data)-1))
      {
        for(e in (d+1):ncol(diff_data))
        {
          for(k in 1:nrow(data))
          {
            if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
               && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e]){
              count<-count+1
            }
          }
          if(count>=minsup){
            no_row=no_row+1
            df[no_row,1]<-paste(a,b,c,d,e,sep="&")
            df[no_row,2]<-count
            count<-0
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of six subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-5))
{
  for(b in (a+1):(ncol(diff_data)-4))
  {
    for(c in (b+1):(ncol(diff_data)-3))
    {
      for(d in (c+1):(ncol(diff_data)-2))
      {
        for(e in (d+1):(ncol(diff_data)-1))
        {
          for(f in (e+1):ncol(diff_data))
          {
            for(k in 1:nrow(data))
            {
              if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                 && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]){
                count<-count+1
              }
            }
            if(count>=minsup){
              no_row=no_row+1
              df[no_row,1]<-paste(a,b,c,d,e,f,sep="&")
              df[no_row,2]<-count
              count<-0
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of seven subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-6))
{
  for(b in (a+1):(ncol(diff_data)-5))
  {
    for(c in (b+1):(ncol(diff_data)-4))
    {
      for(d in (c+1):(ncol(diff_data)-3))
      {
        for(e in (d+1):(ncol(diff_data)-2))
        {
          for(f in (e+1):(ncol(diff_data)-1))
          {
            for(g in (f+1):ncol(diff_data))
            {
              for(k in 1:nrow(data))
              {
                if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                   && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                   && diff_data[k,f]==diff_data[k,g]){
                  count<-count+1
                }
              }
              if(count>=minsup){
                no_row=no_row+1
                df[no_row,1]<-paste(a,b,c,d,e,f,g,sep="&")
                df[no_row,2]<-count
                count<-0
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of eight subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-7))
{
  for(b in (a+1):(ncol(diff_data)-6))
  {
    for(c in (b+1):(ncol(diff_data)-5))
    {
      for(d in (c+1):(ncol(diff_data)-4))
      {
        for(e in (d+1):(ncol(diff_data)-3))
        {
          for(f in (e+1):(ncol(diff_data)-2))
          {
            for(g in (f+1):(ncol(diff_data)-1))
            {
              for(h in (g+1):ncol(diff_data))
              {
                for(k in 1:nrow(data))
                {
                  if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                     && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                     && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]){
                    count<-count+1
                  }
                }
                if(count>=minsup){
                  no_row=no_row+1
                  df[no_row,1]<-paste(a,b,c,d,e,f,g,h,sep="&")
                  df[no_row,2]<-count
                  count<-0
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of nine subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-8))
{
  for(b in (a+1):(ncol(diff_data)-7))
  {
    for(c in (b+1):(ncol(diff_data)-6))
    {
      for(d in (c+1):(ncol(diff_data)-5))
      {
        for(e in (d+1):(ncol(diff_data)-4))
        {
          for(f in (e+1):(ncol(diff_data)-3))
          {
            for(g in (f+1):(ncol(diff_data)-2))
            {
              for(h in (g+1):(ncol(diff_data)-1))
              {
                for(i in (h+1):ncol(diff_data))
                {
                  for(k in 1:nrow(data))
                  {
                    if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                       && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                       && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]){
                      count<-count+1
                    }
                  }
                  if(count>=minsup){
                    no_row=no_row+1
                    df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,sep="&")
                    df[no_row,2]<-count
                    count<-0
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of ten subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-9))
{
  for(b in (a+1):(ncol(diff_data)-8))
  {
    for(c in (b+1):(ncol(diff_data)-7))
    {
      for(d in (c+1):(ncol(diff_data)-6))
      {
        for(e in (d+1):(ncol(diff_data)-5))
        {
          for(f in (e+1):(ncol(diff_data)-4))
          {
            for(g in (f+1):(ncol(diff_data)-3))
            {
              for(h in (g+1):(ncol(diff_data)-2))
              {
                for(i in (h+1):(ncol(diff_data)-1))
                {
                  for(j in (i+1):ncol(diff_data))
                  {
                    for(k in 1:nrow(data))
                    {
                      if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                         && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                         && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                         && diff_data[k,i]==diff_data[k,j]){
                        count<-count+1
                      }
                    }
                    if(count>=minsup){
                      no_row=no_row+1
                      df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,sep="&")
                      df[no_row,2]<-count
                      count<-0
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of eleven subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-10))
{
  for(b in (a+1):(ncol(diff_data)-9))
  {
    for(c in (b+1):(ncol(diff_data)-8))
    {
      for(d in (c+1):(ncol(diff_data)-7))
      {
        for(e in (d+1):(ncol(diff_data)-6))
        {
          for(f in (e+1):(ncol(diff_data)-5))
          {
            for(g in (f+1):(ncol(diff_data)-4))
            {
              for(h in (g+1):(ncol(diff_data)-3))
              {
                for(i in (h+1):(ncol(diff_data)-2))
                {
                  for(j in (i+1):(ncol(diff_data)-1))
                  {
                    for(l in (j+1):ncol(diff_data))
                    {
                      for(k in 1:nrow(data))
                      {
                        if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                           && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                           && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                           && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]){
                          count<-count+1
                        }
                      }
                      if(count>=minsup){
                        no_row=no_row+1
                        df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,sep="&")
                        df[no_row,2]<-count
                        count<-0
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of tweleve subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-11))
{
  for(b in (a+1):(ncol(diff_data)-10))
  {
    for(c in (b+1):(ncol(diff_data)-9))
    {
      for(d in (c+1):(ncol(diff_data)-8))
      {
        for(e in (d+1):(ncol(diff_data)-7))
        {
          for(f in (e+1):(ncol(diff_data)-6))
          {
            for(g in (f+1):(ncol(diff_data)-5))
            {
              for(h in (g+1):(ncol(diff_data)-4))
              {
                for(i in (h+1):(ncol(diff_data)-3))
                {
                  for(j in (i+1):(ncol(diff_data)-2))
                  {
                    for(l in (j+1):(ncol(diff_data)-1))
                    {
                      for(m in (l+1):ncol(diff_data))
                      {
                        for(k in 1:nrow(data))
                        {
                          if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                             && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                             && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                             && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]){
                            count<-count+1
                          }
                        }
                        if(count>=minsup){
                          no_row=no_row+1
                          df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,sep="&")
                          df[no_row,2]<-count
                          count<-0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of thirteen subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-12))
{
  for(b in (a+1):(ncol(diff_data)-11))
  {
    for(c in (b+1):(ncol(diff_data)-10))
    {
      for(d in (c+1):(ncol(diff_data)-9))
      {
        for(e in (d+1):(ncol(diff_data)-8))
        {
          for(f in (e+1):(ncol(diff_data)-7))
          {
            for(g in (f+1):(ncol(diff_data)-6))
            {
              for(h in (g+1):(ncol(diff_data)-5))
              {
                for(i in (h+1):(ncol(diff_data)-4))
                {
                  for(j in (i+1):(ncol(diff_data)-3))
                  {
                    for(l in (j+1):(ncol(diff_data)-2))
                    {
                      for(m in (l+1):(ncol(diff_data)-1))
                      {
                        for(n in (m+1):ncol(diff_data))
                        {
                          for(k in 1:nrow(data))
                          {
                            if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                               && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                               && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                               && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]
                               && diff_data[k,m]==diff_data[k,n]){
                              count<-count+1
                            }
                          }
                          if(count>=minsup){
                            no_row=no_row+1
                            df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,n,sep="&")
                            df[no_row,2]<-count
                            count<-0
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of fourteen subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-13))
{
  for(b in (a+1):(ncol(diff_data)-12))
  {
    for(c in (b+1):(ncol(diff_data)-11))
    {
      for(d in (c+1):(ncol(diff_data)-10))
      {
        for(e in (d+1):(ncol(diff_data)-9))
        {
          for(f in (e+1):(ncol(diff_data)-8))
          {
            for(g in (f+1):(ncol(diff_data)-7))
            {
              for(h in (g+1):(ncol(diff_data)-6))
              {
                for(i in (h+1):(ncol(diff_data)-5))
                {
                  for(j in (i+1):(ncol(diff_data)-4))
                  {
                    for(l in (j+1):(ncol(diff_data)-3))
                    {
                      for(m in (l+1):(ncol(diff_data)-2))
                      {
                        for(n in (m+1):(ncol(diff_data)-1))
                        {
                          for(o in (n+1):ncol(diff_data))
                          {
                            for(k in 1:nrow(data))
                            {
                              if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                                 && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                                 && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                                 && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]
                                 && diff_data[k,m]==diff_data[k,n]&& diff_data[k,n]==diff_data[k,o]){
                                count<-count+1
                              }
                            }
                            if(count>=minsup){
                              no_row=no_row+1
                              df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,n,o,sep="&")
                              df[no_row,2]<-count
                              count<-0
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of fifteen subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-14))
{
  for(b in (a+1):(ncol(diff_data)-13))
  {
    for(c in (b+1):(ncol(diff_data)-12))
    {
      for(d in (c+1):(ncol(diff_data)-11))
      {
        for(e in (d+1):(ncol(diff_data)-10))
        {
          for(f in (e+1):(ncol(diff_data)-9))
          {
            for(g in (f+1):(ncol(diff_data)-8))
            {
              for(h in (g+1):(ncol(diff_data)-7))
              {
                for(i in (h+1):(ncol(diff_data)-6))
                {
                  for(j in (i+1):(ncol(diff_data)-5))
                  {
                    for(l in (j+1):(ncol(diff_data)-4))
                    {
                      for(m in (l+1):(ncol(diff_data)-3))
                      {
                        for(n in (m+1):(ncol(diff_data)-2))
                        {
                          for(o in (n+1):(ncol(diff_data)-1))
                          {
                            for(p in (o+1):ncol(diff_data))
                            {
                              for(k in 1:nrow(data))
                              {
                                if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                                   && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                                   && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                                   && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]
                                   && diff_data[k,m]==diff_data[k,n]&& diff_data[k,n]==diff_data[k,o]&& diff_data[k,o]==diff_data[k,p]){
                                  count<-count+1
                                }
                              }
                              if(count>=minsup){
                                no_row=no_row+1
                                df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,n,o,p,sep="&")
                                df[no_row,2]<-count
                                count<-0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of sixteen subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-15))
{
  for(b in (a+1):(ncol(diff_data)-14))
  {
    for(c in (b+1):(ncol(diff_data)-13))
    {
      for(d in (c+1):(ncol(diff_data)-12))
      {
        for(e in (d+1):(ncol(diff_data)-11))
        {
          for(f in (e+1):(ncol(diff_data)-10))
          {
            for(g in (f+1):(ncol(diff_data)-9))
            {
              for(h in (g+1):(ncol(diff_data)-8))
              {
                for(i in (h+1):(ncol(diff_data)-7))
                {
                  for(j in (i+1):(ncol(diff_data)-6))
                  {
                    for(l in (j+1):(ncol(diff_data)-5))
                    {
                      for(m in (l+1):(ncol(diff_data)-4))
                      {
                        for(n in (m+1):(ncol(diff_data)-3))
                        {
                          for(o in (n+1):(ncol(diff_data)-2))
                          {
                            for(p in (o+1):(ncol(diff_data)-1))
                            {
                              for(q in (p+1):ncol(diff_data))
                              {
                                for(k in 1:nrow(data))
                                {
                                  if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                                     && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                                     && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                                     && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]
                                     && diff_data[k,m]==diff_data[k,n]&& diff_data[k,n]==diff_data[k,o]&& diff_data[k,o]==diff_data[k,p]
                                     && diff_data[k,p]==diff_data[k,q]){
                                    count<-count+1
                                  }
                                }
                                if(count>=minsup){
                                  no_row=no_row+1
                                  df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,n,o,p,q,sep="&")
                                  df[no_row,2]<-count
                                  count<-0
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){   write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE) }

##Combination of seventeen subjects
df <- data.frame(matrix(0,ncol=2))
colnames(df) <- c("Combination","count")
no_row<-0
count<-0
for(a in 2:(ncol(diff_data)-16))
{
  for(b in (a+1):(ncol(diff_data)-15))
  {
    for(c in (b+1):(ncol(diff_data)-14))
    {
      for(d in (c+1):(ncol(diff_data)-13))
      {
        for(e in (d+1):(ncol(diff_data)-12))
        {
          for(f in (e+1):(ncol(diff_data)-11))
          {
            for(g in (f+1):(ncol(diff_data)-10))
            {
              for(h in (g+1):(ncol(diff_data)-9))
              {
                for(i in (h+1):(ncol(diff_data)-8))
                {
                  for(j in (i+1):(ncol(diff_data)-7))
                  {
                    for(l in (j+1):(ncol(diff_data)-6))
                    {
                      for(m in (l+1):(ncol(diff_data)-5))
                      {
                        for(n in (m+1):(ncol(diff_data)-4))
                        {
                          for(o in (n+1):(ncol(diff_data)-3))
                          {
                            for(p in (o+1):(ncol(diff_data)-2))
                            {
                              for(q in (p+1):(ncol(diff_data)-1))
                              {
                                for(r in (q+1):ncol(diff_data))
                                {
                                  for(k in 1:nrow(data))
                                  {
                                    if(diff_data[k,a]<3 && diff_data[k,a]==diff_data[k,b] && diff_data[k,b]==diff_data[k,c]
                                       && diff_data[k,c]==diff_data[k,d] && diff_data[k,d]==diff_data[k,e] && diff_data[k,f]==diff_data[k,e]
                                       && diff_data[k,f]==diff_data[k,g]&& diff_data[k,g]==diff_data[k,h]&& diff_data[k,h]==diff_data[k,i]
                                       && diff_data[k,i]==diff_data[k,j]&& diff_data[k,j]==diff_data[k,l]&& diff_data[k,l]==diff_data[k,m]
                                       && diff_data[k,m]==diff_data[k,n]&& diff_data[k,n]==diff_data[k,o]&& diff_data[k,o]==diff_data[k,p]
                                       && diff_data[k,p]==diff_data[k,q]&& diff_data[k,q]==diff_data[k,r]){
                                      count<-count+1
                                    }
                                  }
                                  if(count>=minsup){
                                    no_row=no_row+1
                                    df[no_row,1]<-paste(a,b,c,d,e,f,g,h,i,j,l,m,n,o,p,q,r,sep="&")
                                    df[no_row,2]<-count
                                    count<-0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
  }
}
if(df[1,1]!=0){
  write.table(df, filepath, sep = ",", col.names = !file.exists(filepath), append = T,row.names = FALSE)
}


