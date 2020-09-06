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
  for(j in 2:ncol(data))
  {
    s<-s+data[i,j]
  }
  m<-s/(ncol(data)-1)
  mean_data[i,1]<-m
}
colnames(mean_data)<-c("Mean of each student")
mean_data

##Difference between the calculated mean and grade of each subject
diff_data<-data.frame(MA11=0,PY12=0,MEL19=0,EC15=0,CV14=0,ME13=0,PYL18=0,CS26=0,CY22=0,ME29=0,CYL27=0,EE25=0,MA21=0,HS24=0,AL21=0,CSL28=0)
for(i in 1:nrow(data))
{
  for(j in 2:ncol(data))
  {
    diff_data[i,j-1]<-round(abs(mean_data[i,1]-data[i,j]))
  }
}

##Count for each subject as per golden rule<3
subjects<- data.frame(matrix(0,ncol=2))
colnames(subjects) <- c("Subject No.","count")
count<-0
no_row<-0
for(i in 1:ncol(diff_data)){
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
if(comb==2)
{
  ##Combination of two subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-1))
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
}else if(comb==3){
  ##Combination of three subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-2))
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
  
}else if(comb==4){
  ##Combination of four subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-3))
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
  
}else if(comb==5){
  ##Combination of five subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-4))
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
  
}else if(comb==6){
  ##Combination of six subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-5))
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
  
}else if(comb==7){
  ##Combination of seven subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-6))
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
  
}else if(comb==8){
  ##Combination of eight subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-7))
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
  
}else if(comb==9){
  ##Combination of nine subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-8))
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
  
}else if(comb==10){
  ##Combination of ten subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-9))
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
  
}else if(comb==11){
  ##Combination of eleven subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-10))
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
  
}else if(comb==12){
  ##Combination of tweleve subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-11))
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
  
}else if(comb==13){
  ##Combination of thirteen subjects
  df <- data.frame(matrix(0,ncol=2))
  colnames(df) <- c("Combination","count")
  no_row<-0
  count<-0
  for(a in 1:(ncol(diff_data)-12))
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
  
}else{
  print("Combination does not exist")
  quit()
}
##Displaying final result
out=read.csv(filepath)
colnames(out) <- c("Combinations","count")
output<-out[order(out$count,decreasing = TRUE),]
View(output)
print("The displayed table gives the subjects whose performances by the students are related to each other")
print("Combination: The subject nos. combinations whose performances are related")
print("Count: The number of students whose performances reflect the combination")
##Converting final result into graphs based on the user's choice
print("Press 1 for bar graph")
print("Press 2 for line graph")
choice<-as.integer(readline(prompt = "Enter your choice:"))

if(choice==1){
  ##Generate bar graph
  bar_no_prev<-1
  bar_no_next<-40
  file_no<-1
  jpg_path<-""
  out_portion<-data.frame()
  while(bar_no_prev<=nrow(out)){
    
    out_portion<-out[c(bar_no_prev:bar_no_next),c(1:2)]
    jpg_path<-paste("rplot",file_no,".jpg",sep="")
    library(ggplot2)
    ggplot(data=out_portion, aes(x=Combinations, y=count, fill=count)) +
      geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle=90, hjust=1))
    ggsave(jpg_path,width=16.7,height = 11.1)
    bar_no_prev<-bar_no_next
    bar_no_next<-bar_no_next+40
    file_no<-file_no+1
    
  }
}else if(choice==2){
  ##Generate line graph
  line_no_prev<-1
  line_no_next<-40
  file_no<-1
  jpg_path<-""
  out_portion<-data.frame()
  while(line_no_prev<=nrow(out)){
    
    out_portion<-out[c(line_no_prev:line_no_next),c(1:2)]
    jpg_path<-paste("rplot",file_no,".jpg",sep="")
    library(ggplot2)
    ggplot(data=out_portion, aes(x=Combinations, y=count, group=1)) + 
      geom_line(colour="red", linetype="dashed", size=1.5) + 
      geom_point(colour="red", size=4, shape=21, fill="white")+
      theme(axis.text.x = element_text(angle=90, hjust=1))
    ggsave(jpg_path,width=16.7,height = 11.1)
    line_no_prev<-line_no_next
    line_no_next<-line_no_next+40
    file_no<-file_no+1
    
  }
}else{
  print("Invalid Choice")
}




