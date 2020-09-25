# version 3.0, 2020-9-4
rm( list=ls() )
setwd("D:/RA-POLYU/lockdown/9-4")
library(reshape2)
library(plyr)
library(tidyverse)
library(readxl)
library(ggpubr)
library(parallel)
library(scales)
source( "function eva.R" ) 
Sys.setlocale("LC_TIME", "English")
travelout <- read.csv("out_20.csv")
travelout.ref <- read.csv("out_19.csv")
travelin <- read.csv("in_20.csv")
travelin.ref <- read.csv("in_19.csv")

simulate.travel(travelout,travelout.ref)-> travel.out
simulate.travel(travelin,travelin.ref)-> travel.in

m<-travel.in[[2]]
city.seq<-colnames(travel.in[[1]])
travel.date.seq<-travel.in[[3]]

# import incidence data
obs <- read.csv("reported case.csv")
obs <- as.data.frame(obs[,-1])
rownames(obs) <- city.seq

# import population data
pop <- read.csv( "selected.city.population.csv" )
N <- pop$Pop
names(N) <- pop$City


# death data
D<-read.csv("death.csv")
D<-D[1,-1]

#assumptions
rate<-0.01
t.max <- 51 # 11 Feb 2020

#make possible value

for(round.ind in 1:300){
  set.seed(round.ind)

  fit.val <- expand.grid(round(runif(5, 0.3, 0.75),2), #beta_1
                         round(runif(5, 0.14, 0.5),2), #sigema
                         round(runif(5, 0.13, 0.3),2), #gamma
                         round(runif(5, 0, 100),0), #I0W
                         round(runif(5, 0, 30),0), #Trans
                         0, #other
                         0.045, #d
                         round(runif(5, 0, 50),0)*300, #p
                         round(runif(5, 1, 4),2)
  )
  fit.val <- fit.val[-which(fit.val$Var1/fit.val$Var3 >= 3 | 
                             fit.val$Var1/fit.val$Var3 <= 2.3),]
  if(length(fit.val$Var1)!=0){
    fit.val$index<-c(1:length(fit.val$Var1))
    fit.val <- as.matrix(fit.val)
    colnames(fit.val)<-c("beta_1", "sigema",  "gamma",  "I0W", 
                         "Trans",  "other",  "d", "p", "delay","index")
    
    cl.cores <- detectCores(logical = T)
    cl <- makeCluster(cl.cores)
    clusterExport(cl, varlist=c("restrict.epidemic.sim","m","N","rate","obs","D",
                                "city.seq","t.max","travel.in","travel.out",
                                "fit.val"),envir=environment())
    clusterEvalQ(cl, library(tidyverse))
    
    system.time(fit.resul <- parApply(cl,fit.val,1,logLikelihood))
    stopCluster(cl)
    
    fit.result <- cbind(fit.resul,fit.val)
    write.csv(fit.result, paste0("sim1/",round.ind,"_sim.csv"))
  }
  print(round.ind)
}




############find CI : 0.5*qchisq(0.95,1) for both sides
for(ss in 1:3){
  #ss=1
  if(ss==1){setwd("D:/RA-POLYU/lockdown/9-4/sim0")}
  if(ss==2){setwd("D:/RA-POLYU/lockdown/9-4/sim1")}
  if(ss==3){setwd("D:/RA-POLYU/lockdown/9-4/sim2")}
  file.list<-dir()
  comb.sim<-NULL
  for(i in file.list){
    #i<-file.list[1]
    temp<-read.csv(i)
    comb.sim<-rbind(comb.sim,temp)
  }
  comb.sim$R <- round(comb.sim$beta_1/comb.sim$gamma,2)
  comb.sim1<- filter(comb.sim, delay<4)
  if(ss==1){
    par1 <- unlist(matrix(comb.sim1[which.max(comb.sim1$fit.resul),-c(1:2)]))
    ci.com.s1 <- find_CI(comb.sim1,par1)
    sim.compa.s1 <- sim.ci.results.fun(ci.com.s1,1)}
  if(ss==2){
    par2 <- unlist(matrix(comb.sim1[which.max(comb.sim1$fit.resul),-c(1:2)]))
    ci.com.s2 <- find_CI(comb.sim1,par2)
    sim.compa.s2 <- sim.ci.results.fun(ci.com.s2,1.5)}
  if(ss==3){
    par3 <- unlist(matrix(comb.sim1[which.max(comb.sim1$fit.resul),-c(1:2)]))
    ci.com.s3 <- find_CI(comb.sim1,par3)
    sim.compa.s3 <- sim.ci.results.fun(ci.com.s3,0.5)}
  ss <- ss+1
}

####see fitting result period 
compa <- rbind(compa.for.pic(sim.compa.s1,par1[1:9],1,"scenario_baseline"),
               compa.for.pic(sim.compa.s2,par2[1:9],1.5,"scenario2"),
               compa.for.pic(sim.compa.s3,par3[1:9],0.5,"scenario3"))
report<-data.frame(value=as.matrix(obs)[51,],
                   date=travel.date.seq[1:length(as.matrix(obs)[51,])])
#2.11
paste0(sum(compa$value[1:52]),"(",sum(compa$lo[1:52]),",",sum(compa$hi[1:52]),")")
paste0(sum(compa$value[157:208]),"(",sum(compa$lo[157:208]),",",sum(compa$hi[157:208]),")")
paste0(sum(compa$value[313:364]),"(",sum(compa$lo[313:364]),",",sum(compa$hi[313:364]),")")

ggplot()+
  geom_line(data=compa,aes(x=date, y=value, group=Type, color=Type),size = 1)+
  geom_ribbon(data=compa,aes(x=date, ymin = lo, ymax = hi, fill = Type, color=Type),
              alpha = 0.4,linetype = 2)+
  scale_colour_manual(values= c("darkred","black","darkblue"))+
  scale_fill_manual(values= c("darkred","gray","darkblue"))+
  geom_point(data=report,aes(x=date, y=value),colour="darkblue",size=1,
             alpha=0.8)+
  facet_wrap(~name.char,scales="free")+
  labs(x= "",y = "Newly reported cases")+  ##,title = "Overall cases fitting"
  theme_bw()+
  scale_y_sqrt()+
  scale_x_date(date_breaks = "15 days",date_labels = "%b-%d")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))
ggsave(file=paste0("fitted.check.pdf"),width=15,height=5)
#########


pic.data1 <- draw.function(par=par1,num.days=108,"scenario1")
pic.data2 <- draw.function(par=par2,num.days=108,"scenario2")
pic.data3 <- draw.function(par=par3,num.days=108,"scenario3")

pic.data1$add_total$type <- paste0(pic.data1$add_total$type," (scenario_baseline)")
pic.data2$add_total$type <- paste0(pic.data2$add_total$type," (scenario_2)")
pic.data3$add_total$type <- paste0(pic.data3$add_total$type," (scenario_3)")

ggplot ()+
  geom_line(data = pic.data1$add_total, aes(x=data, y=value*100,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values=c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.3","restrict_by_2.12"))+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->fig1
ggplot ()+
  geom_line(data = pic.data2$add_total, aes(x=data, y=value*100,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values=c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.3","restrict_by_2.12"))+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->fig2
ggplot ()+
  geom_line(data = pic.data3$add_total, aes(x=data, y=value*100,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values=c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.3","restrict_by_2.12"))+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->fig3
ggarrange(fig1,fig2,fig3,ncol=1,nrow=3,labels = letters[1:3])

ggsave(file=paste0("reduce.long.pdf"),width=15,height=13)

#####data for simulation need 
setwd("D:/RA-POLYU/lockdown/9-4/scenarios.sim")
t.max <- 69
for(s.type in 0:5){
  bed.sim.1 <- sim.ci.results.fun(ci.com.s1,1,69,type=s.type)
  bed.sim.1c <- compa.for.pic(bed.sim.1,par1[1:9],1,"scenario_baseline")
  write.csv(data.frame(bed.sim.1c,date=travel.date.seq[1:70]),
            paste0("est_new_daily1",s.type,".csv"))
  
  bed.sim.2 <- sim.ci.results.fun(ci.com.s2,1.5,69,type=s.type)
  bed.sim.2c <- compa.for.pic(bed.sim.2,par2[1:9],1.5,"scenario_2")
  write.csv(data.frame(bed.sim.2c,date=travel.date.seq[1:70]), 
            paste0("est_new_daily2",s.type,".csv"))
  
  bed.sim.3 <- sim.ci.results.fun(ci.com.s3,0.5,69,type=s.type)
  bed.sim.3c <- compa.for.pic(bed.sim.3,par3[1:9],0.5,"scenario_3")
  write.csv(data.frame(bed.sim.3c,date=travel.date.seq[1:70]), 
            paste0("est_new_daily3",s.type,".csv"))
}

################### dif traffic scenario sim wuhan / other cities



com.res.per<-NULL
for(n in c(T,F,"total")){
  com.res1 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s1,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,4],3)*100,"% (",
                 round(temp[109,6],3)*100,"%, ",
                 round(temp[109,5],3)*100,"%)")
    com.res1 <- cbind(com.res1,temp)
  }
  com.res2 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s2,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,4],3)*100,"% (",
                 round(temp[109,6],3)*100,"%, ",
                 round(temp[109,5],3)*100,"%)")
    com.res2 <- cbind(com.res2,temp)
  }
  com.res3 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s3,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,4],3)*100,"% (",
                 round(temp[109,6],3)*100,"%, ",
                 round(temp[109,5],3)*100,"%)")
    com.res3 <- cbind(com.res3,temp)
  }
  com.res.per <- rbind(com.res.per,com.res1,com.res2,com.res3)
}

com.res.num<-NULL
for(n in c(T,F,"total")){
  com.res1 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s1,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,1])," (",
                 round(temp[109,3]),", ",
                 round(temp[109,2]),")")
    com.res1 <- cbind(com.res1,temp)
  }
  com.res2 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s2,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,1])," (",
                 round(temp[109,3]),", ",
                 round(temp[109,2]),")")
    com.res2 <- cbind(com.res2,temp)
  }
  com.res3 <- NULL
  for(i in 1:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s3,1,108,type=i,citys = n)
    temp<-paste0(round(temp[109,1])," (",
                 round(temp[109,3]),", ",
                 round(temp[109,2]),")")
    com.res3 <- cbind(com.res3,temp)
  }
  com.res.num <- rbind(com.res.num,com.res1,com.res2,com.res3)
}

write.csv(com.res.num,"D:/RA-POLYU/lockdown/9-4/citiescomb.csv")
write.csv(com.res.per,"D:/RA-POLYU/lockdown/9-4/citiescomb.per.csv")


com.res.pic<-NULL
for(n in c(T,F,"total")){
  com.res1 <- NULL
  for(i in 0:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s1,1,108,type=i,citys = n)
    temp<-data.frame(temp[,4:6]*100,type=i,group=n, Type="scenario_baseline")
    com.res1 <- rbind(com.res1,temp)
  }
  com.res2 <- NULL
  for(i in 0:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s2,1,108,type=i,citys = n)
    temp<-data.frame(temp[,4:6]*100,type=i,group=n,Type="scenario_2")
    com.res2 <- rbind(com.res2,temp)
  }
  com.res3 <- NULL
  for(i in 0:5){
    #i=1
    temp<-sim.ci.results.fun.dif.sce(ci.com.s3,1,108,type=i,citys = n)
    temp<-data.frame(temp[,4:6]*100,type=i,group=n,Type="scenario_3")
    com.res3 <- rbind(com.res3,temp)
  }
  com.res.pic <- rbind(com.res.pic,com.res1,com.res2,com.res3)
}
com.res.pic$date <- rep(travel.date.seq[1:109],18)
com.res.pic$type <- com.res.pic$type%>%as.character()
com.res.pic$type[which(com.res.pic$type==0)] <- "Real scenario"
com.res.pic$type[which(com.res.pic$type==1)] <- "restrict_by_2.12"
com.res.pic$type[which(com.res.pic$type==2)] <- "restrict_by_1.3"
com.res.pic$type[which(com.res.pic$type==3)] <- "reduce_80%"
com.res.pic$type[which(com.res.pic$type==4)] <- "reduce_50%"
com.res.pic$type[which(com.res.pic$type==5)] <- "reduce_0%"

com.res.pic[which(is.na(com.res.pic[,1])==T),1]<-0
com.res.pic[which(is.na(com.res.pic[,2])==T),2]<-0
com.res.pic[which(is.na(com.res.pic[,3])==T),3]<-0

com.res.pic$Type <- com.res.pic$Type%>%as.character()
com.res.pic$Type[which(com.res.pic$Type=="scenario_baseline")]<-"1)scenario_baseline"
com.res.pic$Type[which(com.res.pic$Type=="scenario_2")]<-"2)scenario_2"
com.res.pic$Type[which(com.res.pic$Type=="scenario_3")]<-"3)scenario_3"

citys.pic<-filter(com.res.pic,group==T)
wuhan.pic<-filter(com.res.pic,group==F)
all.pic<-filter(com.res.pic,group=="total")

citys.pic$group<-paste0("Other 50 cities (",citys.pic$Type,")")
wuhan.pic$group<-paste0("Wuhan (",wuhan.pic$Type,")")
all.pic$group<-paste0("Total (",all.pic$Type,")")

ggplot ()+
  geom_line(data = citys.pic, aes(x=date, y=mean.1,group=type,color=type),size=1) +
  geom_ribbon(data= citys.pic, aes(x=date, ymin = lo.1, ymax = hi.1, fill = type, color=type),
              alpha = 0.1,linetype = 2)+
  facet_wrap(~group,scales="free")+
  #scale_fill_brewer(palette = 'PRGn')+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->p2

ggplot ()+
  geom_line(data = wuhan.pic, aes(x=date, y=mean.1,group=type,color=type),size=1) +
  geom_ribbon(data= wuhan.pic, aes(x=date, ymin = lo.1, ymax = hi.1, fill = type, color=type),
              alpha = 0.1,linetype = 2)+
  facet_wrap(~group,scales="free")+
  #scale_fill_brewer(palette = 'PRGn')+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->p1

ggplot ()+
  geom_line(data = all.pic, aes(x=date, y=mean.1,group=type,color=type),size=1) +
  geom_ribbon(data= all.pic, aes(x=date, ymin = lo.1, ymax = hi.1, fill = type, color=type),
              alpha = 0.2,linetype = 2)+
  facet_wrap(~group,scales="free")+
  #scale_fill_brewer(palette = 'Spectral')+
  #scale_color_brewer(palette = 'Spectral')+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->p3
ggarrange(p1,p2,p3,ncol=1,nrow=3,labels = letters[1:3])

ggsave(file=paste0("D:/RA-POLYU/lockdown/9-4/diff.sce.sim.pdf"),width=15,height=13)
################### second wave
setwd("D:/RA-POLYU/lockdown/9-4/scenarios.sim.secondwave")
t.max <- 254

par.types1 <- make.par.types(par1[1:9])
par.types2 <- make.par.types(par2[1:9])
par.types3 <- make.par.types(par3[1:9])

for(s.type in c(0,6,7)){
  bed.sim.1 <- sim.ci.results.fun(ci.com.s1,1,254,type=s.type)
  bed.sim.1c <- compa.for.pic(bed.sim.1,par.types1[s.type+1,],1,"scenario_baseline")
  write.csv(data.frame(bed.sim.1c,date=travel.date.seq[1:255]),
            paste0("est_new_daily1",s.type,".csv"))
  
  bed.sim.2 <- sim.ci.results.fun(ci.com.s2,1.5,254,type=s.type)
  bed.sim.2c <- compa.for.pic(bed.sim.2,par.types2[s.type+1,],1.5,"scenario_2")
  write.csv(data.frame(bed.sim.2c,date=travel.date.seq[1:255]), 
            paste0("est_new_daily2",s.type,".csv"))
  
  bed.sim.3 <- sim.ci.results.fun(ci.com.s3,0.5,254,type=s.type)
  bed.sim.3c <- compa.for.pic(bed.sim.3,par.types3[s.type+1,],0.5,"scenario_3")
  write.csv(data.frame(bed.sim.3c,date=travel.date.seq[1:255]), 
            paste0("est_new_daily3",s.type,".csv"))
}
namelist <- dir()
second.wave.comb <- NULL
for(i in 1:length(namelist)){
  #i=1
  temp <- data.frame(read.csv(namelist[i]),type=substr(namelist[i],14,15))
  second.wave.comb <- rbind(second.wave.comb,temp)
}
second.wave.comb$type <- second.wave.comb$type %>% as.character()
second.wave.comb$type[which(substr(second.wave.comb$type,2,2)==0)] <- "current"
second.wave.comb$type[which(substr(second.wave.comb$type,2,2)==6)] <- "1/30"
second.wave.comb$type[which(substr(second.wave.comb$type,2,2)==7)] <- "1/40"
second.wave.comb$date <- second.wave.comb$date %>% as.Date()
#second.wave.comb <- filter(second.wave.comb,name.char=="scenario_baseline")
ggplot()+
  geom_line(data=second.wave.comb,aes(x=date, y=compa, group=type, color=type),size = 1)+
  geom_ribbon(data=second.wave.comb,aes(x=date, ymin = lo, ymax = hi, fill = type, color=type),
              alpha = 0.4,linetype = 2)+
  scale_colour_manual(values= c("darkred","black","darkblue"))+
  scale_fill_manual(values= c("darkred","gray","darkblue"))+
  facet_wrap(~name.char,scales="free")+
  labs(x= "",y = "Newly reported cases")+  ##,title = "Overall cases fitting"
  theme_bw()+
  scale_y_sqrt()+
  scale_x_date(date_breaks = "2 months",date_labels = "%b-%d")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))
ggsave(file=paste0("D:/RA-POLYU/lockdown/9-4/reduce.long.pdf"),
       width=15,height=5)

################### for bed table
setwd("D:/RA-POLYU/lockdown/9-4")
bedtable<-read.csv("bed.table.csv")

bedtable$group<-bedtable$group%>%as.character()
bedtable$group[which(bedtable$scenario==1)]<-paste0(bedtable$group[which(bedtable$scenario==1)]," (scenario_baseline)")
bedtable$group[which(bedtable$scenario==2)]<-paste0(bedtable$group[which(bedtable$scenario==2)]," (scenario_2)")
bedtable$group[which(bedtable$scenario==3)]<-paste0(bedtable$group[which(bedtable$scenario==3)]," (scenario_3)")


ggplot()+
  geom_bar(data=filter(bedtable,scenario==1),aes(x=date, y=value, fill=type),
           stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Spectral')+
  facet_wrap(~group,scales="free")+
  #scale_x_date(date_breaks = "7 days",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  labs(x= NULL,y = "Number of hospital beds shortage")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=10),axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),legend.text=element_text(size=15),legend.title = element_text(size=15),
        strip.text.x = element_text(size = 15))->Pic1

ggplot()+
  geom_bar(data=filter(bedtable,scenario==2),aes(x=date, y=value, fill=type),
           stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Spectral')+
  facet_wrap(~group,scales="free")+
  #scale_x_date(date_breaks = "7 days",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  labs(x= NULL,y = "Number of hospital beds shortage")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=10),axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),legend.text=element_text(size=15),legend.title = element_text(size=15),
        strip.text.x = element_text(size = 15))->Pic2

ggplot()+
  geom_bar(data=filter(bedtable,scenario==3),aes(x=date, y=value, fill=type),
           stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Spectral')+
  facet_wrap(~group,scales="free")+
  #scale_x_date(date_breaks = "7 days",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  labs(x= NULL,y = "Number of hospital beds shortage")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=10),axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),legend.text=element_text(size=15),legend.title = element_text(size=15),
        strip.text.x = element_text(size = 15))->Pic3
ggarrange(Pic1,Pic2,Pic3,ncol=1,nrow=3,labels = letters[1:3])

ggsave(file=paste0("bed.table.pdf"),width=15,height=13)




######old version
est_real<-cumsum(test.set$onset_data[51,]) 
est_confirmed<-cumsum(test.set$res_China[51,])
est_report<-cumsum(phi[51,]*(test.set$res_China[51,]))
report<-data.frame(value=cumsum(as.matrix(obs)[51,]),date=travel.date.seq[1:length(est_report)])


compa<-melt(data.frame('estimates by onset date'=est_real,
                       'estimates by diagnosis date'=est_confirmed,
                       'model fit to observed data'=est_report))
compa$date<-rep(travel.date.seq[1:length(est_report)],3)
colnames(compa)[1]<-"Type"
New_fit<-data.frame(compa,type="2) Cumulative number")
New_report<-data.frame(report,type="2) Cumulative number")
ggplot()+
  geom_line(data=compa,aes(x=date, y=value, group=Type, color=Type))+
  geom_point(data=report,aes(x=date, y=value),colour="darkblue",size=1,
             alpha=0.8)+
  labs(x= "",y = "Newly reported cases")+  ##,title = "Overall cases fitting"
  theme_bw()+
  scale_y_sqrt()+
  scale_x_date(date_breaks = "10 days",date_labels = "%b-%d")->pic2
  
add_fit<-rbind(Total_fit,New_fit)
add_report<-rbind(Total_report,New_report)
add_report$Type<-"observed data"
ggplot()+
  geom_point(data=add_report,aes(x=date, y=value,fill=Type),colour="darkblue",size=1,
             alpha=0.8)+
  geom_line(data=add_fit,aes(x=date, y=value, group=Type, color=Type))+
  facet_wrap(~type,scales="free")+
  labs(x= "",y = "Number of cases")+  ##,title = "Overall cases fitting"
  scale_x_date(date_breaks = "10 days",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 17))
ggsave(file=paste0("fitted.check.pdf"),width=13,height=6)

## fitting details for cities
cities_data_est <- test.set$res_China
cities_data_obs <- as.matrix(obs)
colnames(cities_data_est) <- travel.date.seq[1:(t.max+1)]
colnames(cities_data_obs) <- travel.date.seq[1:(t.max+1)]
cities.num.est<-data.frame(melt(cities_data_est),type="est")
cities.num.real<-data.frame(melt(cities_data_obs),type="real")
cities.num.est$Var2 %>% as.Date(origin="1970-1-1") -> cities.num.est$Var2
cities.num.real$Var2 %>% as.Date(origin="1970-1-1") -> cities.num.real$Var2
cities.num.comb <- rbind(cities.num.est,cities.num.real)

ggplot ()+
  geom_line(data = cities.num.comb, aes(x=Var2, y=value, group=type, color=type)) +
  facet_wrap(~Var1,scales="free",nrow = 6)+
  scale_colour_manual(values= c("darkred","darkblue"))+
  labs(x= "",y = "Cum number")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "2 month",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))






