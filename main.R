rm(list = ls())
data1=read.csv("data_test1.csv")
test=read.csv("ciqinggan.csv")



data1=data.frame(data1,test)

test=read.csv("cixing.csv")
data1=data.frame(data1,test)
data1=data1[,-2]
data1=data1[,-3]
data1=data1[,-3]
data1=data1[,-4]
data1=data1[,-4]

colnames(data1)=c("word","emotion","tag")
data1$word=as.character(data1$word)
data1$tag_sum=as.factor(as.character(substr(data1$tag,1,1)))


test=read.csv("cixiangliang_shape.csv")
for (i in 1:nrow(test)) {
  if (test[i,2]==-1) {
    test[i,2]=NA
    test[i,1]='nomean'
  }
  else{
    test[i,1]='meaningful'
  }
}

data2=data.frame(data1,test)
data2=data2[,-2]
data2=data2[,-2]
rm(test,i)



tagword="ÊµÖÊ"
for (i in 1:nrow(data1)) {
  if (data1$word[i]==tagword) {
    print(data1$tag[i])
  }
}

data3=data.frame(data1,data2)

data_n=subset(data3,data3$tag_sum=="n")
data_v=subset(data3,data3$tag_sum=="v")
data_a=subset(data3,data3$tag=="a")
data_adv=subset(data3,data3$tag=="ad")
data_an=subset(data3,data3$tag=="an")
data_adj=rbind(data_a,data_an)
rm(data_a,data_an)

set.seed(1234)
words=10
data2_nona=na.omit(data_adv)
data2_nona=data2_nona[,-2]
data2_nona=data2_nona[,-2]
data2_nona=data2_nona[,-2]
data2_nona=data2_nona[,-2]
mykm=kmeans(x = data2_nona[,3:102],centers = words)
center=mykm$centers
for (i in 1:words) {
  
  dist_min=Inf
  dist_min_No.=NA
  for (j in 1:nrow(data2_nona)) {
    distdata=rbind(center[i,],data2_nona[j,3:102])
    dist=dist(distdata,p = 2)
    if (dist<dist_min) {
      dist_min=dist
      dist_min_No.=j
    }
  }
  print(data2_nona[dist_min_No.,1])
}
rm(i,j,dist,dist_min,dist_min_No.,distdata,center,mykm)
