
library(ggplot2)
library(ggthemes)

# For data processing
library(dplyr)

# To draw the radar map
library(fmsb)

# To use the grid.arrange function
# a function can arrange multiple ggplot images
library(gridExtra)

# Get workspace path
getwd()
# Empty existing workspace
rm(list = ls())

# Read data
raw_data=read.csv('data.csv')
# Remove outliers
raw_data <- raw_data %>% 
  mutate(sum1_1 = attr1_1 + sinc1_1 + intel1_1 + amb1_1 + shar1_1) %>%
  mutate(attr1_1 = (attr1_1/sum1_1)*100) %>% 
  mutate(sinc1_1 = (sinc1_1/sum1_1)*100) %>% 
  mutate(intel1_1 = (intel1_1/sum1_1)*100) %>% 
  mutate(amb1_1 = (amb1_1/sum1_1)*100) %>% 
  mutate(shar1_1 = (shar1_1/sum1_1)*100) %>% 
  mutate(sum2_1 = attr2_1 + sinc2_1 + intel2_1 + amb2_1 + shar2_1) %>%
  mutate(attr2_1 = (attr2_1/sum2_1)*100) %>% 
  mutate(sinc2_1 = (sinc2_1/sum2_1)*100) %>% 
  mutate(intel2_1 = (intel2_1/sum2_1)*100) %>% 
  mutate(amb2_1 = (amb2_1/sum2_1)*100) %>% 
  mutate(shar2_1 = (shar2_1/sum2_1)*100) 

data_features <- raw_data %>% filter(!is.na(sum1_1)) %>% 
  filter(!is.na(sum2_1)) %>% select(iid, gender, attr1_1:shar2_1)
data_features <- unique(data_features, by = idd) 

# Delete useless variables
rawdat=raw_data
dat <- rawdat %>% select(-id, -idg, -condtn, -round, -position, -positin1, -order, -partner)

dataframe00 <-dat %>%select(iid, pid, dec, gender, attr, sinc, intel, fun, amb, shar, like, prob) %>% filter(!pid == "NA")

dataframe00[is.na(dataframe00)] <- 1000
dataframe00$total <- rowSums(dataframe00[,c("attr", "sinc", "intel", "fun", "amb", "shar")])
dataframe00 <-dataframe00 %>% filter(!total == "6000")
dataframe00[dataframe00 == "1000"] <- NA
dataframe00$total <- rowSums(dataframe00[,c("attr", "sinc", "intel", "fun", "amb", "shar")], na.rm=TRUE)

dataframe00 <-  dataframe00 %>%   filter(!total == "0")

# Integrate the data of a matched pair of volunteers

dataframe00 <-  dataframe00 %>%   mutate(pgender = ifelse(gender == 0, 1, 0))
dataframe11<-dat %>%group_by(gender) %>%select(iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1) %>% unique()
dataframe11[is.na(dataframe11)] <- 0
dataframe11$total <- rowSums(dataframe11[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1")])
dataframe11<-  dataframe11 %>%   filter(!total == "0")

dataframe11$attr1_1 <- round(dataframe11$attr1_1/dataframe11$total*100, digits = 2)
dataframe11$sinc1_1 <- round(dataframe11$sinc1_1/dataframe11$total*100, digits = 2)
dataframe11$intel1_1 <- round(dataframe11$intel1_1/dataframe11$total*100, digits = 2)
dataframe11$fun1_1 <- round(dataframe11$fun1_1/dataframe11$total*100, digits = 2)
dataframe11$amb1_1 <- round(dataframe11$amb1_1/dataframe11$total*100, digits = 2)
dataframe11$shar1_1 <- round(dataframe11$shar1_1/dataframe11$total*100, digits = 2)
dataframe11$total <- rowSums(dataframe11[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1")])
dataframe11$total <- round(dataframe11$total, digits = 0)

# Convert data frame
data.transformed <- rawdat
data.transformed[rawdat$gender == 0,]$gender <- 'Female'
data.transformed[rawdat$gender == 1,]$gender <- 'Male'
data.transformed$gender <- as.factor(data.transformed$gender)
data.transformed %>%
  filter(!pid == "NA" | !id =="NA")

mydata=raw_data
male = mydata[mydata$gender =="1", ]
female = mydata[mydata$gender =="0", ]
Attract = c ( (mean(female$attr3_1 , na.rm = T)) ,( mean(female$attr_o , na.rm = T)))
Sinc = c ( (mean(female$sinc3_1 , na.rm = T)) ,( mean(female$sinc_o , na.rm = T)))
Intel  = c ( (mean(female$intel3_1 , na.rm = T)) ,( mean(female$intel_o , na.rm = T)))
Fun   = c ( (mean(female$fun3_1 , na.rm = T)) ,( mean(female$fun_o , na.rm = T)))
Amb = c ( (mean(female$amb3_1 , na.rm = T)) ,( mean(female$amb_o , na.rm = T)))
testdata = data.frame(Attract ,Sinc, Intel, Fun, Amb)
testdata = t(testdata)
testdata = as.data.frame(testdata)
colnames(testdata)[1]= "self_perception"
colnames(testdata)[2]= "men_perception"

Attract1 = c ( (mean(male$attr3_1 , na.rm = T)) ,( mean(male$attr_o , na.rm = T)))
Sinc1 = c ( (mean(male$sinc3_1 , na.rm = T)) ,( mean(male$sinc_o , na.rm = T)))
Fun1   = c ( (mean(male$fun3_1 , na.rm = T)) ,( mean(male$fun_o , na.rm = T)))
Amb1 = c ( (mean(male$amb3_1 , na.rm = T)) ,( mean(male$amb_o , na.rm = T)))
Intel1  = c ( (mean(male$intel3_1 , na.rm = T)) ,( mean(male$intel_o , na.rm = T)))
testdata1 = data.frame(Attract1 ,Sinc1, Intel1, Fun1, Amb1)
testdata1 = t(testdata1)
testdata1 = as.data.frame(testdata1)
colnames(testdata1)[1]= "men_self_perception"
colnames(testdata1)[2]= "women_perception"

testdata11 = testdata


# data processing: Women's self-evaluation and evaluation of matched men
radar_women = t(testdata11)
radar_women = as.data.frame(radar_women)
maxmin <- data.frame(
  Attract = c(10, 0),
  Sinc = c(10, 0),
  Intel = c(10, 0),
  Fun = c(10, 0),
  Amb = c(10, 0))
radar_women1 <- rbind(maxmin, radar_women)
# plot: Women's self-evaluation and evaluation of matched men
radarchart(radar_women1,pcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           pfcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           plty = 1,plwd = 3,vlcex = 0.8,title="Women's self-evaluation and evaluation of matched men")

legend(x = 0, y = 1.2, legend = c("matched men's evaluation", "women self evaluation"), 
       bty = "n", pch = 20 , col = c(rgb(0.9, 0.5, 0.5, 0.4), rgb(0.1, 0.5, 0.1, 0.4)) , 
       text.col = "black", cex = 1, pt.cex = 2)

# data processing: Men's self-evaluation and evaluation of matched women
radar_men = t(testdata1)
radar_men = as.data.frame(radar_men)
maxmin <- data.frame(
  Attract1 = c(10, 0),
  Sinc1 = c(10, 0),
  Intel1 = c(10, 0),
  Fun1 = c(10, 0),
  Amb1 = c(10, 0))
radar_men1 <- rbind(maxmin, radar_men)
# plot: Men's self-evaluation and evaluation of matched women
radarchart(radar_men1,pcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           pfcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           plty = 1,plwd = 3,vlcex = 0.8,title="Men's self-evaluation and evaluation of matched women")

legend(x = 0, y = 1.2, legend = c("matched women's evaluation", "Men self evaluation"), 
       bty = "n", pch = 20 , col = c(rgb(0.9, 0.5, 0.5, 0.4), rgb(0.1, 0.5, 0.1, 0.4)) , 
       text.col = "black", cex = 1, pt.cex = 2)

# data processing: People's self cognition and opposite sex's cognition
data.cleaned <-data.transformed %>%filter(!pid == "NA" | !id =="NA")

data.analyse <-data.cleaned %>%select(iid,pid, gender, attr3_1, sinc3_1, intel3_1, 
                                      fun3_1, amb3_1, attr5_1, sinc5_1, intel5_1, 
                                      fun5_1, amb5_1, attr, sinc, intel, fun, amb, 
                                      shar,attr_o, sinc_o, intel_o, fun_o, amb_o, 
                                      shar_o,dec_o,dec)

data.others.yourself.partners <-
  data.analyse %>% 
  group_by(iid) %>% 
  summarise(
    Gender = first(gender),
    PositiveResponseScore = mean(dec_o, na.rm = TRUE),
    Attractive.Others = first(attr5_1), 
    Sincere.Others = first(sinc5_1), 
    Intelligent.Others = first(intel5_1), 
    Fun.Others = first(fun5_1), 
    Ambitious.Others = first(amb5_1), 
    Attractive.Yourself = first(attr3_1), 
    Sincere.Yourself = first(sinc3_1), 
    Intelligent.Yourself = first(intel3_1), 
    Fun.Yourself = first(fun3_1), 
    Ambitious.Yourself = first(amb3_1),
    Attractive.Partners = mean(attr_o, na.rm = TRUE), 
    Sincere.Partners = mean(sinc_o, na.rm = TRUE), 
    Intelligent.Partners = mean(intel_o, na.rm = TRUE), 
    Fun.Partners = mean(fun_o, na.rm = TRUE), 
    Ambitious.Partners = mean(amb_o, na.rm = TRUE))

data.yourself.partners <-
  data.others.yourself.partners %>% 
  filter(!is.na(Attractive.Partners)&!is.na(Sincere.Partners)&!is.na(Intelligent.Partners)&!is.na(Fun.Partners)&!is.na(Ambitious.Partners))

# plot: People's self cognition and opposite sex's cognition
plot.yourself.partners <- ggplot(data = data.yourself.partners) + 
  geom_point(position = "jitter", alpha = 1/2) +  
  facet_wrap(~Gender) + geom_smooth(method = lm) + scale_x_continuous(breaks = seq(0,10,2)) + 
  scale_y_continuous(breaks = seq(0,10,2))+
  theme_pander()

plot.yourself.partners.attractive <- plot.yourself.partners  + aes(x = Attractive.Yourself, y = Attractive.Partners)
plot.yourself.partners.ambitious <- plot.yourself.partners  + aes(x = Ambitious.Yourself, y = Ambitious.Partners)  
plot.yourself.partners.sincere <- plot.yourself.partners  + aes(x = Sincere.Yourself, y = Sincere.Partners)
plot.yourself.partners.intelligent <- plot.yourself.partners  + aes(x = Intelligent.Yourself, y = Intelligent.Partners)
plot.yourself.partners.fun <- plot.yourself.partners  + aes(x = Fun.Yourself, y = Fun.Partners)

grid.arrange(plot.yourself.partners.attractive, plot.yourself.partners.ambitious, 
             plot.yourself.partners.sincere, plot.yourself.partners.intelligent,
             plot.yourself.partners.fun)

# data processing: The opposite sex traits people look for
test1 <-
  dataframe11 %>%
  group_by(gender) %>%
  summarise(Attractive = mean(attr1_1), Sincere = mean(sinc1_1), Intelligent = mean(intel1_1), Fun = mean(fun1_1), Ambitious = mean(amb1_1), Interest = mean(shar1_1))

test1forplot <-
  test1 %>% 
  select(-gender)

maxmin <- data.frame(
  Attractive = c(36, 0),
  Sincere = c(36, 0),
  Intelligent = c(36, 0),
  Fun = c(36, 0),
  Ambitious = c(36, 0),
  Interest = c(36, 0))

test11 <- rbind(maxmin, test1forplot)

test11male <- test11[c(1,2,4),]
test11female <- test11[c(1,2,3),]

# plot :The opposite sex traits people look for
radarchart(test11,pcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           pfcol = c(rgb(0.1, 0.5, 0.1, 0.4),rgb(0.9, 0.5, 0.5, 0.4)),
           plty = 1,plwd = 3,vlcex = 0.8,title="The opposite sex traits people look for")


legend(x = 0.5, y = 1.2, legend = c("Male perspective", "Female perspective"), 
       bty = "n", pch = 20 , col = c(rgb(0.9, 0.5, 0.5, 0.4), rgb(0.1, 0.5, 0.1, 0.4)) , 
       text.col = "black", cex = 1, pt.cex = 2)

# data processing: What people value most about opposite sex

men <- filter(data_features, gender =="1")
women <-filter(data_features, gender =="0") 

row_label <- c("Self", "Majority")
column_label <- c("Attractive", "Sincere", "Intelligent", 
                  "Fun", "Ambitious", "Shared Interests")

# data processing: What men value most about women
radar_men <- as.data.frame(matrix(0, nrow = 2, ncol = 6))
colnames(radar_men) <- column_label
rownames(radar_men) <- row_label

for (i in (1:nrow(radar_men))) {
  for(j in c(1:ncol(radar_men))) {
    if(i == 1) {
      radar_men[i, j] <- mean(men[ , 2 + j])
    }
    if(i == 2){
      radar_men[i,j] <- mean(women[ , 14 + j])
    }  
  }
}
# plot: What men value most about women
radar_men = rbind(rep(40, 5) , rep(0, 5) , radar_men)
radarchart(radar_men, pcol= c( rgb(0.2, 0.5, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9)), 
           pfcol = c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4)),  
           plwd = 3 , plty = 1, vlcex = 0.8, 
           title = "What men value most about women")
legend(x = 0.5, y = 1.2, legend = c("Men perspective", "Women perspective"), 
       bty = "n", pch = 20 , col = c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4)), 
       text.col = "black", cex = 1, pt.cex = 2)


# data processing: What women value most about men
radar_women <- as.data.frame(matrix(0, nrow = 2, ncol = 6))
colnames(radar_women) <- column_label
rownames(radar_women) <- row_label

for (i in (1:nrow(radar_women))) {
  for(j in c(1:ncol(radar_women))) {
    if(i == 1) {
      radar_women[i,j] <- mean(women[ , 2 + j])
    }
    if( i == 2) {
      radar_women[i,j] <- mean(men[ , 14 + j])
    }  
  }
}
# plot: What women value most about men
radar_women = rbind(rep(40, 5) , rep(0, 5) , radar_women)
radarchart(radar_women, pcol = c( rgb(0.8, 0.2, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9)), 
           pfcol = c(rgb(0.8, 0.2, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4)),  
           plwd = 3 , plty = 1, vlcex = 0.8,
           title = "What women value most about men")
legend(x = 0.5, y = 1.2, legend = c("Women perspective", "Man perspective"), 
       bty = "n", pch = 20 , col = c(rgb(0.8, 0.2, 0.5, 0.4) , rgb(0.7, 0.5, 0.1, 0.4)), 
       text.col = "black", cex = 1, pt.cex = 2)

# data processing: People's statements and choices at seeking for a mate
test2 <-dataframe00 %>%  group_by(pid, pgender) %>%  summarise(Decision = mean(dec), Attractive = mean(attr), Sincere = mean(sinc), Intelligent = mean(intel), Fun = mean(fun), Ambitious = mean(amb), Interest = mean(shar))
test2a <-test2 %>%   select(pid, pgender, Decision, Attractive) %>%   filter(!Attractive == "NA")
test2b <-test2 %>%   select(pid, pgender, Decision, Sincere) %>%   filter(!Sincere == "NA")
test2c <-test2 %>%   select(pid, pgender, Decision, Intelligent) %>%   filter(!Intelligent == "NA")
test2d <-test2 %>%   select(pid, pgender, Decision, Fun) %>%   filter(!Fun == "NA")
test2e <-test2 %>%   select(pid, pgender, Decision, Ambitious) %>%   filter(!Ambitious == "NA")
test2f <-test2 %>%   select(pid, pgender, Decision, Interest) %>%   filter(!Interest == "NA")

coratr <- cor(test2a$Decision, test2a$Attractive)
corsin <- cor(test2b$Decision, test2b$Sincere)
corint <- cor(test2c$Decision, test2c$Intelligent)
corfun <- cor(test2d$Decision, test2d$Fun)
coramb <- cor(test2e$Decision, test2e$Ambitious)
corshar <- cor(test2f$Decision, test2f$Interest)

test2am <-test2 %>%   select(pid, pgender, Decision, Attractive) %>%   filter(!Attractive == "NA") %>%   filter(pgender == "1")
test2af <-test2 %>%   select(pid, pgender, Decision, Attractive) %>%   filter(!Attractive == "NA") %>%   filter(pgender == "0")

cormatr <- cor(test2am$Decision, test2am$Attractive)
corfatr <- cor(test2af$Decision, test2af$Attractive)

test2bm <-test2 %>%   select(pid, pgender, Decision, Sincere) %>%   filter(!Sincere == "NA") %>%   filter(pgender == "1")
test2bf <-test2 %>%   select(pid, pgender, Decision, Sincere) %>%   filter(!Sincere == "NA") %>%   filter(pgender == "0")  

cormsin <- cor(test2bm$Decision, test2bm$Sincere)
corfsin <- cor(test2bf$Decision, test2bf$Sincere)

test2cm <-test2 %>%   select(pid, pgender, Decision, Intelligent) %>%   filter(!Intelligent == "NA") %>%   filter(pgender == "1")
test2cf <-test2 %>%   select(pid, pgender, Decision, Intelligent) %>%   filter(!Intelligent == "NA") %>%   filter(pgender == "0")

cormint <- cor(test2cm$Decision, test2cm$Intelligent)
corfint <- cor(test2cf$Decision, test2cf$Intelligent)

test2dm <-test2 %>%   select(pid, pgender, Decision, Fun) %>%   filter(!Fun == "NA") %>%   filter(pgender == "1")
test2df <-test2 %>%   select(pid, pgender, Decision, Fun) %>%   filter(!Fun == "NA") %>%   filter(pgender == "0")

cormfun <- cor(test2dm$Decision, test2dm$Fun)
corffun <- cor(test2df$Decision, test2df$Fun)

test2em <-test2 %>%   select(pid, pgender, Decision, Ambitious) %>%   filter(!Ambitious == "NA") %>%   filter(pgender == "1")
test2ef <-test2 %>%   select(pid, pgender, Decision, Ambitious) %>%   filter(!Ambitious == "NA") %>%   filter(pgender == "0")

cormamb <- cor(test2em$Decision, test2em$Ambitious)
corfamb <- cor(test2ef$Decision, test2ef$Ambitious)

test2fm <-test2 %>%   select(pid, pgender, Decision, Interest) %>%   filter(!Interest == "NA") %>%   filter(pgender == "1")
test2ff <-test2 %>%   select(pid, pgender, Decision, Interest) %>%   filter(!Interest == "NA") %>%   filter(pgender == "0")

cormshar <- cor(test2fm$Decision, test2fm$Interest)
corfshar <- cor(test2ff$Decision, test2ff$Interest)

corforgraph2 <-data.frame(Traits = c("Average", "Male", "Female"),                          corAttractive = c(coratr, cormatr, corfatr),                          corSincere = c(corsin, cormsin, corfsin),                          corIntelligence = c(corint, cormint, corfint),                          corFun = c(corfun, cormfun, corffun),                          corAmbitious = c(coramb, cormamb, corfamb),                          corInterest = c(corshar, cormshar, corfshar))

fin <- corforgraph2
fin$total <- rowSums(fin[,c("corAttractive", "corSincere", "corIntelligence", "corFun", "corAmbitious", "corInterest")])

fin$corAttractive <- round(fin$corAttractive/fin$total*100, digits = 2)
fin$corSincere <- round(fin$corSincere/fin$total*100, digits = 2)
fin$corIntelligence <- round(fin$corIntelligence/fin$total*100, digits = 2)
fin$corFun <- round(fin$corFun/fin$total*100, digits = 2)
fin$corAmbitious <- round(fin$corAmbitious/fin$total*100, digits = 2)
fin$corInterest <- round(fin$corInterest/fin$total*100, digits = 2)

fin <-fin %>%   select(corAttractive, corSincere, corIntelligence, corFun, corAmbitious, corInterest)

colnames(fin) <- c("Attractive","Sincere", "Intelligent", "Fun", "Ambitious", "Interest")
testn <- rbind(maxmin, fin, test1forplot)
testnmale <- testn[-c(3, 4, 6), ]
testnfemale <- testn[-c(3, 5, 7), ]

# plot: People's statements and choices at seeking for a mate
radarchart(testnmale, pcol= c( rgb(0.2, 0.5, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9)), 
           pfcol = c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4)),  
           plwd = 3 , plty = 1, vlcex = 0.8, 
           title = "Men's statements and choices in seeking for a mate")

legend(x = 0.5, y = 1.2, legend =  c("What men say", "What men do"), 
       bty = "n", 
       pch = 20 , 
       col = c(rgb(0.7, 0.5, 0.1, 0.4),rgb(0.2, 0.5, 0.5, 0.4)), 
       text.col = "black", cex = 1, pt.cex = 2)


radarchart(testnfemale, pcol= c( rgb(0.8, 0.2, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9)), 
           pfcol = c(rgb(0.8, 0.2, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4)),  
           plwd = 3 , plty = 1, vlcex = 0.8, 
           title = "Women's statements and choices in seeking for a mate")

legend(x = 0.5, y = 1.2, legend =  c("What women say", "What women do"), 
       bty = "n", 
       pch = 20 , 
       col = c(rgb(0.7, 0.5, 0.1, 0.4),rgb(0.8, 0.2, 0.5, 0.4)), 
       text.col = "black", cex = 1, pt.cex = 2)


# Empty existing workspace
rm(list = ls())
# Read Google trends data
data_boyngirl=read.csv("boyngirl.csv")

# Transforming data into long data is convenient for ggplot drawing
data_new=data.frame(matrix(nrow = (nrow(data_boyngirl)*10),ncol = 2))

for (i in 2:11) {
  for (j in 1:nrow(data_boyngirl)) {
    data_new[(i-2)*nrow(data_boyngirl)+j,1]=data_boyngirl[j,i]
  }
  data_new[((i-2)*52):((i-1)*52),2]=as.character(colnames(data_boyngirl)[i])
}
data_boyngirl=data_new
rm(i,j,data_new)
colnames(data_boyngirl)=c('value','word')

# plot
ggplot(data=data_boyngirl,aes(y=log(value+1)))+
  geom_boxplot(aes(fill =word))+theme_pander()+ #+theme_wsj()
  #ggtitle("БъЬт")+
  xlab("word")+ylab("Search index in Google Trends(log(x+1))")


rm(data_boyngirl)


# plot grid.arrange(to write the report)


