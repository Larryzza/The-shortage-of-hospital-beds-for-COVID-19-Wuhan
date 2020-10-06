library(wordcloud2)
library(jiebaR)
library(ggplot2)
library(jpeg)
library(reshape2)
library(wordcloud)
library(stringr)
library(readxl)
#setwd("/Users/zza/Downloads/wuhancrisis.github.io-master/_posts")

namelist<-dir()
combine.text<-NULL
for(i in namelist){
  temp<-readLines(i)
  if(length(temp)<20){temp<-c("-",temp)}
  combine.text<-rbind(combine.text,temp)
}
combine.text<-combine.text[,c(5,6,9,11,12,13,14,15,18)]

deletword<-c("【","】","（","）","联系电话","非新冠肺炎","武汉非肺炎危重病人求助","#","是否有病史","地址"
             ,"主要诉求","非肺炎患者求助","其他紧急联系人","病情描述","志愿者代发","转发好友求助信息"
             ,"武汉加油","非肺炎患者求助","非新冠肺炎危重病人","性别","content_full_raw: "
             ,"求助人信息","若有相关化验单，请上传图片","姓名","年龄","所在城市","帮转"
             ,"所在小区、社区","患病时间","联系方式","●●●","志愿者代发","整理","转发","转自"
             ,"非新冠肺炎求助超话","武汉非肺炎危重病人求助","非新冠肺炎","所在城市、社区","岁"
             ,"name_: ","age_: ","city_: ","address_: ","desc_: ","since_: ","央视新闻","岁"
             ,"中国日报","武汉晚报","人民日报","人民网","央视网","央视新闻","丁香医生","新华网"
             ,"中国新闻网","热点新闻","中国新闻周刊","24小时新闻","网易新闻客户端","凤凰新闻客户端"
             ,"新京报动新闻","全球头条新闻事件","央视新闻周刊","@","侠客岛","章磊老师","武汉发布"
             ,"楚天交通广播","楚天都市报","求助超话","湖北日报","头条新闻","长江日报","中报视频","其他紧急联系请大家看到我微博"
             ,"梁晚笛","荆楚网","武汉同城","武汉同城会","凤凰网","澎湃新闻","老陶在路上","西藏昌都人韩红"
             ,"武汉市长专线","三联生活周刊","武汉政府","武汉大学人民医院","姚晨","新华日报","移动叔叔"
             ,"中国新闻周刊","北京晚报","国际版小助理","广州日报","环球时报","千钧客","微博管理员","中国新闻周刊"
             ,"中国新闻网","移动叔叔","新华视点","肺炎患者求助超话","患者","...全文","头条代发","病情详细描述")

for(k in 3:7){
  #k=3
  for(i in deletword){
    combine.text[,k]<-str_replace_all(combine.text[,k],i,"")
    #print(i)
  }
}

combine.text1<-combine.text[which(combine.text[,1]==str_subset(combine.text[,1], "08002020")),]
see<-str_subset(combine.text[,1], "08002020")
combine.text1<-NULL
for(k in str_subset(combine.text[,1], "08002020")){
  temp<-combine.text[which(combine.text[,1]==k),]
  combine.text1<-rbind(combine.text1,temp)
}
combine.text1[,1]<-str_sub(combine.text1[,1],18,22)

write.csv(combine.text1,file = "total.csv")

for(i in 1:length(combine.text1[,1])){
  #i=1
  for(n in 4:7){
    if(combine.text1[i,n]==""){combine.text1[i,n]<-"NA"}
    else(combine.text1[i,3]<-str_replace(combine.text1[i,3],combine.text1[i,n],""))
  }
}

combine.text.feb[,1]<-str_replace(combine.text.feb[,1],"Feb","2020-02-")
combine.text.Mar[,1]<-str_replace(combine.text.Mar[,1],"Mar","2020-03-")
combine.text.Mar<-data.frame(Date=as.Date(combine.text.Mar[,1]),combine.text.Mar[,-1])
combine.text.feb<-data.frame(Date=as.Date(combine.text.feb[,1]),combine.text.feb[,-1])


new.dat<-as.matrix(read_xlsx("整理.xlsx"))
for(i in 1:length(new.dat[,1])){
  #i=1
  for(n in 3:5){
    if(is.na(new.dat[i,n])){new.dat[i,n]<-"NA"}
    if(new.dat[i,n]==""){new.dat[i,n]<-"NA"}
    else(new.dat[i,2]<-str_replace(new.dat[i,2],new.dat[i,n],""))
  }
}
for(k in 2:5){
  #k=3
  for(i in deletword){
    new.dat[,k]<-str_replace_all(new.dat[,k],i,"")
    #print(i)
  }
}
'new.dat<-new.dat[which(str_detect(new.dat[,5], "湖北")==TRUE
                       |str_detect(new.dat[,5], "武汉")==TRUE),]'
new.dat<-new.dat[which(str_detect(new.dat[,5], "武汉")==TRUE),]
#"排除新冠" "不是新冠肺炎" 
Feiyan<-new.dat[which(str_detect(new.dat[,2], "确诊")==TRUE
                      |str_detect(new.dat[,2], "肺")==TRUE
                      |str_detect(new.dat[,2], "发热")==TRUE
                      |str_detect(new.dat[,2], "发烧")==TRUE
                      |str_detect(new.dat[,2], "冠")==TRUE
                      |str_detect(new.dat[,2], "疑似")==TRUE
                      |str_detect(new.dat[,2], "核酸")==TRUE
                      |str_detect(new.dat[,2], "隔离")==TRUE
                      |str_detect(new.dat[,2], "呼吸")==TRUE
                      |str_detect(new.dat[,2], "咳嗽")==TRUE
                      |str_detect(new.dat[,2], "阳")==TRUE),]
feifeiyan<-Feiyan[which(str_detect(Feiyan[,2], "不是新冠肺炎")==TRUE
                        |str_detect(Feiyan[,2], "排除新冠")==TRUE),]
Feiyan<-Feiyan[-which(str_detect(Feiyan[,2], "不是新冠肺炎")==TRUE
                     |str_detect(Feiyan[,2], "排除新冠")==TRUE),]
feiyan.num<-data.frame(date=as.Date(names(table(Feiyan[,1]))),
                       num=as.numeric(table(Feiyan[,1])),type="COVID-19") 


Feifeiyan<-new.dat[-which(str_detect(new.dat[,2], "确诊")==TRUE
                          |str_detect(new.dat[,2], "肺")==TRUE
                          |str_detect(new.dat[,2], "发热")==TRUE
                          |str_detect(new.dat[,2], "发烧")==TRUE
                          |str_detect(new.dat[,2], "冠")==TRUE
                          |str_detect(new.dat[,2], "疑似")==TRUE
                          |str_detect(new.dat[,2], "核酸")==TRUE
                          |str_detect(new.dat[,2], "隔离")==TRUE
                          |str_detect(new.dat[,2], "呼吸")==TRUE
                          |str_detect(new.dat[,2], "咳嗽")==TRUE
                          |str_detect(new.dat[,2], "阳")==TRUE),]
Feifeiyan<-rbind(Feifeiyan,feifeiyan)
Feifeiyan<-Feifeiyan[-c(1,2,3,4,5,6,21),]
Feifeiyan.num<-data.frame(date=as.Date(names(table(Feifeiyan[,1]))),
                          num=as.numeric(table(Feifeiyan[,1])),type="non-COVID-19") 

wQuezhen<-new.dat[which(str_detect(new.dat[,2], "未能确诊")==TRUE
                       |str_detect(new.dat[,2], "不确诊")==TRUE
                       |str_detect(new.dat[,2], "未确诊")==TRUE
                       |str_detect(new.dat[,2], "还没确诊")==TRUE
                       |str_detect(new.dat[,2], "疑似")==TRUE
                       |str_detect(new.dat[,2], "阴性")==TRUE),]
wQuezhen.num<-data.frame(date=as.Date(names(table(wQuezhen[,1]))),
                          num=as.numeric(table(wQuezhen[,1])),type="suspected COVID-19")

Quezhen<-new.dat[-which(str_detect(new.dat[,2], "未能确诊")==TRUE
                       |str_detect(new.dat[,2], "不确诊")==TRUE
                       |str_detect(new.dat[,2], "未确诊")==TRUE
                       |str_detect(new.dat[,2], "还没确诊")==TRUE
                       |str_detect(new.dat[,2], "疑似")==TRUE
                       |str_detect(new.dat[,2], "阴性")==TRUE),]
Manxingbing<-new.dat[which(str_detect(new.dat[,2], "癌")==TRUE
                        |str_detect(new.dat[,2], "肾")==TRUE
                        |str_detect(new.dat[,2], "脑")==TRUE
                        |str_detect(new.dat[,2], "肝")==TRUE
                        |str_detect(new.dat[,2], "瘤")==TRUE
                        |str_detect(new.dat[,2], "白血病")==TRUE
                        |str_detect(new.dat[,2], "糖尿")==TRUE
                        |str_detect(new.dat[,2], "高血压")==TRUE
                        |str_detect(new.dat[,2], "心脏病")==TRUE),]
Both_Manxingbing<-Feiyan[which(str_detect(Feiyan[,2], "癌")==TRUE
                               |str_detect(Feiyan[,2], "肾")==TRUE
                               |str_detect(Feiyan[,2], "脑")==TRUE
                               |str_detect(Feiyan[,2], "肝")==TRUE
                               |str_detect(Feiyan[,2], "瘤")==TRUE
                               |str_detect(Feiyan[,2], "白血病")==TRUE
                               |str_detect(Feiyan[,2], "糖尿")==TRUE
                               |str_detect(Feiyan[,2], "高血压")==TRUE
                               |str_detect(Feiyan[,2], "心脏病")==TRUE),]
Both_Manxingbing.num<-data.frame(date=as.Date(names(table(Both_Manxingbing[,1]))),
                         num=as.numeric(table(Both_Manxingbing[,1])),type="chronic disease (COVID-19)")

Only_Manxingbing<-Feifeiyan[which(str_detect(Feifeiyan[,2], "癌")==TRUE
                                  |str_detect(Feifeiyan[,2], "肾")==TRUE
                                  |str_detect(Feifeiyan[,2], "脑")==TRUE
                                  |str_detect(Feifeiyan[,2], "肝")==TRUE
                                  |str_detect(Feifeiyan[,2], "瘤")==TRUE
                                  |str_detect(Feifeiyan[,2], "白血病")==TRUE
                                  |str_detect(Feifeiyan[,2], "糖尿")==TRUE
                                  |str_detect(Feifeiyan[,2], "高血压")==TRUE
                                  |str_detect(Feifeiyan[,2], "心脏病")==TRUE),]
'Feifeiyan.num<-data.frame(date=as.Date(names(table(Only_Manxingbing[,1]))),
                          num=as.numeric(table(Only_Manxingbing[,1])),type="chronic disease (non-COVID-19)") '

Wuhan.case<-new.dat[which(str_detect(new.dat[,5], "武汉")==TRUE),]


com.dat<-rbind(feiyan.num,Feifeiyan.num)
length(Feiyan[,1])/length(new.dat[,1])

length(Feifeiyan[,1])/length(new.dat[,1])
length(Both_Manxingbing[,1])/length(new.dat[,1])
length(wQuezhen[,1])/length(new.dat[,1])
length(Wuhan.case[,1])/length(new.dat[,1])
com.dat<-com.dat[-c(28:38,51:53),]
ggplot()+
  geom_line(data = com.dat,aes(y=num,x=date,group=type,color=type))+
  scale_x_date(date_breaks = "3 days",date_labels = "%b-%d")+
  geom_vline(aes(xintercept = 18302 ),linetype="dashed",color="black")+
  geom_vline(aes(xintercept = 18312 ),linetype="dashed",color="black")+
  #geom_vline(aes(xintercept = 18321 ),linetype="dashed",color="black")+
  scale_y_log10(breaks=c(1,5,10,50,100,200,300))+
  labs(x= NULL,y = "number of posts")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=13),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->textpic
ggsave(textpic,filename = "Figure S3.pdf",width = 11, height = 6)


age.daily<-NULL
for(i in unique(new.dat[,1])){
  #i<-"2020-02-04"
  temp<-data.frame(new.dat[which(str_detect(new.dat[,4], "NA")==FALSE),])
  temp<-temp[which(temp[,1]==i),]
  temp2<-as.numeric(as.character(temp[,4]))
  temp3<-c(i,length(temp[,1]),round(summary(temp2)))
  age.daily<-rbind(age.daily,temp3)
  #print(i)
}
see<-load_nCov2019()
see<-see$data
wuhan.dat<-see[which(see$city=="Wuhan"),]
wuhan.dat<-wuhan.dat[c(65:100),]
comb.dat<-cbind(data.frame(age.daily),wuhan.dat[,c(4:6)])

length(Feiyan[,1])/(length(Feiyan[,1])+length(Feifeiyan[,1]))
length(wQuezhen[,1])/(length(wQuezhen[,1])+length(Quezhen[,1]))
length(Manxingbing[,1])/length(new.dat[,1])
length(Both_Manxingbing[,1])/length(new.dat[,1])
length(Only_Manxingbing[,1])/length(new.dat[,1])
length(Wuhan.case[,1])/length(new.dat[,1])

k<-as.numeric(table(new.dat[,1]))
k<-data.frame(date=names(table(new.dat[,1])),number=as.numeric(table(new.dat[,1])))
plot(k)



wk<-worker(stop_word="cn_stopwords.txt")
wk[new.dat[,2]]

word.daily<-NULL
new.dat<-data.frame(new.dat)
for(i in unique(new.dat[,1])){
  #i <- "2020-02-04"
  temp <- new.dat[which(new.dat[,1]==i),]
  text <- unlist(wk[as.character(temp[,2])])
  freq <- data.frame(table(text))
  freq <- freq[nchar(as.character(freq$text))>=2,]
  freq <- freq[order(-freq$Freq),]
  #str(freq)
  temp.c<-as.character(freq$text[c(1:10)])
  word.daily<-rbind(word.daily,temp.c)
}
word.daily <- data.frame(date=unique(new.dat[,1]),word.daily)

wordcloud2(freq)



comb.dat$cum_confirm<-diff(temp[,4])
comb.dat$cum_heal<-diff(temp[,5])
comb.dat$cum_dead<-diff(temp[,6])
Num_post<-as.numeric(as.character(comb.dat$V2))/mean(as.numeric(as.character(comb.dat$V2)))
Mean_age<-as.numeric(as.character(comb.dat$Mean))/mean(as.numeric(as.character(comb.dat$Mean)))
Daily_confirm<-as.numeric(as.character(comb.dat$cum_confirm))/mean(as.numeric(as.character(comb.dat$cum_confirm)))
Daily_heal<-as.numeric(as.character(comb.dat$cum_heal))/mean(as.numeric(as.character(comb.dat$cum_heal)))
Daily_dead<-as.numeric(as.character(comb.dat$cum_dead))/mean(as.numeric(as.character(comb.dat$cum_dead)))
comb.ratio<-data.frame(cbind(Num_post,Mean_age,Daily_confirm,Daily_heal,Daily_dead),date=unique(new.dat[,1]))
comb.ratio<-melt(comb.ratio)
comb.ratio$date<-as.Date(comb.ratio$date)

ggplot()+
  geom_line(data = comb.ratio,aes(y=value,x=comb.ratio$date,group=variable,color=variable))+
  scale_x_date(date_breaks = "10 days",date_labels = "%b-%d")+
  scale_y_sqrt(breaks=c(1,2,3,5,7,10,15))+
  labs(x= "date",y = "ratio(daily number/average)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))
write.csv(comb.dat,file = "combination.csv")
write.csv(word.daily,file = "worddaily.csv")
##########################previous version
'combine.text.feb<-NULL
for(k in str_subset(combine.text1[,1], "Feb")){
  temp<-combine.text[which(combine.text[,1]==k),]
  combine.text.feb<-rbind(combine.text.feb,temp)
}
combine.text.feb[,1]<-str_sub(combine.text.feb[,1],18,22)
combine.text.Mar<-NULL
for(k in str_subset(combine.text1[,1], "Mar")){
  temp<-combine.text[which(combine.text[,1]==k),]
  combine.text.Mar<-rbind(combine.text.Mar,temp)
}
combine.text.Mar[,1]<-str_sub(combine.text.Mar[,1],18,22)


for(k in 1:9){
  #k=3
  for(i in deletword){
    combine.text.Mar[,k]<-str_replace_all(combine.text.Mar[,k],i,"")
  }
  for(i in deletword){
    combine.text.feb[,k]<-str_replace_all(combine.text.feb[,k],i,"")
  }
}
for(i in 1:length(combine.text.Mar[,1])){
  #i=1
  for(n in 4:7){
    if(combine.text.Mar[i,n]==""){combine.text.Mar[i,n]<-"NA"}
    else(combine.text.Mar[i,3]<-str_replace(combine.text.Mar[i,3],combine.text.Mar[i,n],""))
  }
}
#combine.text.feb[,c(3:7)]<-as.character(combine.text.feb[,c(3:7)])
for(i in 1:length(combine.text.feb[,1])){
  #i=1
  for(n in 4:7){
    if(combine.text.feb[i,n]==""){combine.text.feb[i,n]<-"NA"}
    else(combine.text.feb[i,3]<-str_replace(combine.text.feb[i,3],combine.text.feb[i,n],""))
  }
}
combine.text.feb[,1]<-str_replace(combine.text.feb[,1],"Feb","2020-02-")
combine.text.Mar[,1]<-str_replace(combine.text.Mar[,1],"Mar","2020-03-")
combine.text.Mar<-data.frame(Date=as.Date(combine.text.Mar[,1]),combine.text.Mar[,-1])
combine.text.feb<-data.frame(Date=as.Date(combine.text.feb[,1]),combine.text.feb[,-1])
table(combine.text.feb[,1])

write.csv(combine.text.Mar,file = "mar.csv")
write.csv(combine.text.feb,file = "feb.csv")
write.csv(combine.text,file = "tol.csv")


'