setwd("D:/RA-POLYU/lockdown/9-4")
library(reshape2)
library(ggplot2)
library(dplyr)
bed<-read.csv("武汉定点医院病床使用情况.csv")
bed$Date<-as.Date(bed$Date)
bed.total<-NULL
datelist<-c(unique(bed$Date))
for(i in datelist){
  #i<-"2020-02-25"
  temp1<-bed[which(bed$Date==i),]
  temp2<-data.frame(total=sum(temp1[,2]),used=sum(temp1[,3]),available=sum(temp1[,4]),date=as.Date(i,"1970-01-01"))
  bed.total<-rbind(temp2,bed.total)
}
bed.total$d.tol<-c(0,diff(bed.total$total))
bed.total$d.use<-c(0,diff(bed.total$used))
bed.total$d.ava<-c(0,diff(bed.total$available))

for(i in 1:length(bed.total$used)){
  if(bed.total$total[i]<bed.total$used[i]){
    bed.total$total[i]<-bed.total$used[i]
  }
}

fangcang<-read.csv("Fangcang.bed.csv")
bed.hospital<-bed.total$total
bed.fangcang<-c(0,0,0,0,fangcang$Total)

bed.total$total[c(5:25)]<-fangcang$Total+bed.total$total[c(5:25)]
bed.total$used[c(5:25)]<-fangcang$Used+bed.total$used[c(5:25)]
'plot.dat<-rbind(data.frame(Date=bed.total$date[c(5:25)],num=fangcang$Used,type="Used beds"),
                data.frame(Date=bed.total$date[c(5:25)],num=fangcang$Total,type="Total beds"))'
plot.dat<-rbind(data.frame(Date=bed.total$date,num=bed.total$used,type="Used beds"),
                data.frame(Date=bed.total$date,num=bed.total$total,type="Total beds"))


ggplot()+
  geom_line(data = plot.dat,aes(y=num,x=as.Date(Date),group=type,color=type))+
  scale_x_date(date_breaks = "3 days",date_labels = "%b-%d")+
  scale_y_continuous(breaks=c(10000,15000,20000,25000,30000,35000))+
  geom_vline(aes(xintercept = 18300 ),linetype="dashed",color="dark gray")+
  geom_vline(aes(xintercept = 18308 ),linetype="dashed",color="dark gray")+
  labs(x= NULL,y = "number of beds")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=13),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->bedpic
ggsave(bedpic,filename = "beds.pdf",width = 10, height = 6)

##############################
'obs<-read.csv("est_new_daily.csv")

param1<-9000/bed.hospital[length(bed.hospital)]
param2<-245/7286

obs$date<-as.Date(obs$date)
heavy.situation<-data.frame(num=c(1:round(2800*param1*(1-param2))),days=c(0),patients=c(0),length=c(0),type="heavy")
light.situation<-data.frame(num=c(1:round(2800*(1-param1))),days=c(0),patients=c(0),length=c(0),type="light")
icu.situation<-data.frame(num=c(1:round(2800*param1*param2)),days=c(0),patients=c(0),length=c(0),type="icu")
bed.situation<-rbind(icu.situation,heavy.situation,light.situation)

bed.hospital[4]<-8279
add.bed.heavy<-round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*param1*(1-param2))
add.bed.icu<-round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*param1*param2)
add.bed.light<-c(0,0,0,0,0,0,0,0,0,diff(bed.fangcang))+
  round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*(1-param1))

patient.situation<-obs$num[10]

#light 80%

heavy.list<-NULL
light.list<-NULL
icu.list<-NULL
available_bed<-NULL
set.seed(13)
for(d in 1:70){
  
  #prepare
  if(d==1){
    gap.heavy=0
    gap.light=0
    gap.icu=0
  }
  
  #add bed
  if(d>33 & d<=66){
    temp1<-add.bed.heavy[d-33]
    temp2<-add.bed.light[d-33]
    temp3<-add.bed.icu[d-33]
    if(temp1>0){
      newbed<-data.frame(num=c(1:temp1),days=c(0),patients=c(0),length=c(0),type="heavy")
      bed.situation<-rbind(bed.situation,newbed)
      #print(temp1)
    }
    if(temp2>0){
      newbed<-data.frame(num=c(1:temp2),days=c(0),patients=c(0),length=c(0),type="light")
      bed.situation<-rbind(bed.situation,newbed)
      #print(temp2)
    }
    if(temp3>0){
      newbed<-data.frame(num=c(1:temp3),days=c(0),patients=c(0),length=c(0),type="icu")
      bed.situation<-rbind(bed.situation,newbed)
      #print(temp3)
    }
  }
  
  #adjust record
  temp<-which(bed.situation$days>0 & bed.situation$days==bed.situation$length)
  bed.situation$days[temp]<-0
  temp<-which(bed.situation$days>0 & bed.situation$days<bed.situation$length)
  bed.situation$days[temp]<-bed.situation$days[temp]+1
  
  #calculate need
  need.icu <- ceiling(obs$num[d]*0.05) + gap.icu
  need.heavy <- ceiling(obs$num[d]*0.14) + gap.heavy
  need.light <- ceiling(obs$num[d]*0.81) + gap.light
  
  ###icu
  if(need.icu>length(which(bed.situation$days==0 & bed.situation$type=="icu"))){
    need.bed <- length(which(bed.situation$days==0 & bed.situation$type=="icu"))
    gap.icu <- need.icu - length(which(bed.situation$days==0 & bed.situation$type=="icu"))
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="icu"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,8,2))
      }
    }
  } else {
    need.bed<-need.icu
    gap.icu<-0
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="icu"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,8,2))
      }
    }
  }
  #heavy
  if(need.heavy>length(which(bed.situation$days==0 & bed.situation$type=="heavy"))){
    need.bed <- length(which(bed.situation$days==0 & bed.situation$type=="heavy"))
    gap.heavy <- need.heavy - length(which(bed.situation$days==0 & bed.situation$type=="heavy"))
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="heavy"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,11,2))
      }
    }
  } else {
    need.bed<-need.heavy
    gap.heavy<-0
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="heavy"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,11,2))
      }
    }
  }
  #light
  if(need.light>length(which(bed.situation$days==0 & bed.situation$type=="light"))){
    need.bed <- length(which(bed.situation$days==0 & bed.situation$type=="light"))
    gap.light <- need.light - length(which(bed.situation$days==0 & bed.situation$type=="light"))
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="light"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,11,2))
      }
    }
  }
  else {
    need.bed<-need.light
    gap.light<-0
    if(need.bed>0){
      for(i in 1:need.bed){
        index<-min(which(bed.situation$days==0 & bed.situation$type=="light"))
        bed.situation$days[index]<-1
        bed.situation$patients[index]<-bed.situation$patients[index]+1
        bed.situation$length[index]<-ceiling(rnorm(1,11,2))
      }
    }
  }
  
  heavy.list<-c(heavy.list,gap.heavy)
  light.list<-c(light.list,gap.light)
  icu.list<-c(icu.list,gap.icu)
  #print(bed.situation$patients[1])
  tempbed<-c(icu=length(which(bed.situation$days==0 & bed.situation$type=="icu")),
             heavy=length(which(bed.situation$days==0 & bed.situation$type=="heavy")),
             light=length(which(bed.situation$days==0 & bed.situation$type=="light")))
  available_bed<-rbind(available_bed,tempbed)
  print(d)
  print(paste0(gap.icu,"  ",length(which(bed.situation$days==0 & bed.situation$type=="icu")),
               "  ",length(bed.situation[which(bed.situation$type=="icu"),5])))
  print(paste0(gap.heavy,"  ",length(which(bed.situation$days==0 & bed.situation$type=="heavy")),
               "  ",length(bed.situation[which(bed.situation$type=="heavy"),5])))
  print(paste0(gap.light,"  ",length(which(bed.situation$days==0 & bed.situation$type=="light")),
               "  ",length(bed.situation[which(bed.situation$type=="light"),5])))
  print(length(bed.situation$num))
}

available_bed<-data.frame(available_bed[c(42:70),],date=obs$date[42:70])
Gap.situation<-data.frame("Mild"=light.list,"Severe"=heavy.list,"Critical"=icu.list,date=obs$date[1:length(light.list)])
melt(Gap.situation[,c(1,2,3)])
plot.gap<-Gap.situation[c(42:70),]
plot.gap<-data.frame(melt(plot.gap[,c(1,2,3)]),date=rep(plot.gap$date,3))
colnames(plot.gap)[1]<-"estimated.shortage"
ggplot()+
  geom_point(data = plot.gap,aes(y=value,x=as.Date(date),group=estimated.shortage,shape=estimated.shortage),color="darkorange")+
  #scale_fill_manual(values=c(hue_pal()(3)))+
  geom_line(data = dif_bed,aes(y=type.value,x=as.Date(date),group=designated.beds,color=designated.beds))+
  guides(color=guide_legend(title="designated.beds"),shap=guide_legend(title="estimated.shortage"))+
  #scale_colour_manual(values=c(hue_pal()(5))[3:5])+
  scale_x_date(date_breaks = "3 days",date_labels = "%b-%d")+
  scale_y_sqrt(breaks=c(0,300,1000,2500,5000,10000,20000,30000,40000))+
  geom_vline(aes(xintercept = 18302 ),linetype="dashed",color="black")+
  geom_vline(aes(xintercept = 18312 ),linetype="dashed",color="black")+
  labs(x= NULL,y = "Daily number of hospital beds")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=13),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->gappic
print(gappic)
ggsave(gappic,filename = "gappic.pdf",width = 11, height = 7)
#plot.gap$value[379:567]+plot.gap$value[190:378]-available_bed$heavy[1:189]


heavybed<-round(2800*param1*(1-param2))+c(0,cumsum(add.bed.heavy),rep(cumsum(add.bed.heavy)[33],4))
icubed<-round(2800*param1*param2)+c(0,cumsum(add.bed.icu),rep(cumsum(add.bed.icu)[33],4))
lightbed<-round(2800*(1-param1))+c(0,cumsum(add.bed.light),rep(cumsum(add.bed.light)[33],4))
dif_bed<-data.frame(Critical=icubed,Severe=heavybed,Mild=lightbed)
dif_bed<-data.frame(type=melt(dif_bed),date=rep(travel.date.seq[33:70],3))
dif_bed<-dif_bed[-c(1:9,39:47,77:85),]
colnames(dif_bed)[1]<-"designated.beds"
"designated.beds"
ggplot()+
  geom_bar(data = dif_bed,aes(y=type.value,x=as.Date(date),group=type.variable,fill=type.variable),stat='identity',position='dodge')+
  scale_x_date(date_breaks = "7 days",date_labels = "%b-%d")+
  scale_y_sqrt(breaks=c(0,300,1000,2500,5000,10000,20000,30000,40000))+
  labs(x= NULL,y = "Number of hospital beds")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=13),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->bedsplot
print(bedsplot)
ggsave(bedsplot,filename = "bedsplot.pdf",width = 11, height = 7)

#plot scenarios
bed_scenarios<-read.csv("bed_scenario.csv",encoding = "UTF-8")
colnames(bed_scenarios)[1]<-"Stage"
bed_scenarios$Type<-factor(bed_scenarios$Type,levels = c(levels(bed_scenarios$Type)[2],levels(bed_scenarios$Type)[3],levels(bed_scenarios$Type)[1]))
bed_scenarios$Policy<-factor(bed_scenarios$Policy,levels = c(levels(bed_scenarios$Policy)[2],
                                                                 levels(bed_scenarios$Policy)[4],
                                                                 levels(bed_scenarios$Policy)[3],
                                                                 levels(bed_scenarios$Policy)[5],
                                                                 levels(bed_scenarios$Policy)[6],
                                                                 levels(bed_scenarios$Policy)[1]))
                                                            
ggplot()+
  geom_bar(data = bed_scenarios,aes(y=Number,x=Stage,group=Policy,fill=Policy),stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Spectral')+
  facet_wrap(~Type,scales="free")+
  #scale_x_date(date_breaks = "7 days",date_labels = "%b-%d")+
  #scale_y_sqrt()+
  labs(x= NULL,y = "Number of hospital beds shortage")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=10),axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),legend.text=element_text(size=15),legend.title = element_text(size=15),
        strip.text.x = element_text(size = 15))
ggsave(filename = "bed_scenarios.pdf",width = 15, height = 7)

ceiling(mean(plot.gap$value[30:39]))
ceiling(mean(plot.gap$value[40:49]))
ceiling(mean(plot.gap$value[50:58]))

ceiling(mean(plot.gap$value[59:68]))
ceiling(mean(plot.gap$value[69:78]))
ceiling(mean(plot.gap$value[79:87]))

ceiling(mean(plot.gap$value[1:10]))
ceiling(mean(plot.gap$value[11:20]))
ceiling(mean(plot.gap$value[21:29]))

ggsave(gappic,filename = "gap.pdf",width = 10, height = 6)

#677 104 506 157 1975 685 446 174 1166 1722 170 1669 1664 1133
'




############################# updated method with CI


for(z in 1:9){
  #z<-2
  #setwd("D:/RA-POLYU/lockdown/9-4/scenarios.sim") #1-18
  setwd("D:/RA-POLYU/lockdown/9-4/scenarios.sim.secondwave") #1-9
  scenarios <- dir()
  obs.ref<-read.csv(scenarios[z])
  
  param1<-9000/bed.hospital[length(bed.hospital)]
  param2<-245/7286
  
  
  bed.hospital[4]<-8279
  add.bed.heavy<-round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*param1*(1-param2))
  add.bed.icu<-round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*param1*param2)
  add.bed.light<-c(0,0,0,0,0,0,0,0,0,diff(bed.fangcang))+
    round(c(0,1240,0,110,0,1100,790,370,420,diff(bed.hospital))*(1-param1))
  
  heavybed<-round(2800*param1*(1-param2))+c(rep(0,33),cumsum(add.bed.heavy),rep(cumsum(add.bed.heavy)[33],200))
  icubed<-round(2800*param1*param2)+c(rep(0,33),cumsum(add.bed.icu),rep(cumsum(add.bed.icu)[33],200))
  lightbed<-round(2800*(1-param1))+c(rep(0,33),cumsum(add.bed.light),rep(cumsum(add.bed.light)[33],200))
  
  #l<-70
  l<-255
  
  #setwd("D:/RA-POLYU/lockdown/9-4/bed_sim_comb")
  setwd("D:/RA-POLYU/lockdown/9-4/bed_sim_comb_long")
  name.all <- dir()
  name.ref <- substr(scenarios[z],
                     str_locate(scenarios[z],"y")[1]+1,
                     str_locate(scenarios[z],"y")[1]+2)
  #path <- paste0("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/",
   #              name.all[grep(name.ref,name.all)])
  path <- paste0("D:/RA-POLYU/lockdown/9-4/bed_sim_comb_long/",
                 name.all[grep(name.ref,name.all)])
  setwd(path)
  
  death.situation.combine<-NULL
  sim.patients <- matrix(NA,300,l+1)
  for(i in 1:300){
    for(k in 1:l){
      sim.patients[i,k] <- runif(1,obs.ref$lo[k],obs.ref$hi[k])
    }
    sim.patients[i,k+1] <- i
  }
  #summary(apply(sim.patients, 1, sum))
  cl.cores <- detectCores(logical = T)
  cl <- makeCluster(cl.cores)
  clusterExport(cl, varlist=c("l","icubed","heavybed","lightbed","travel.date.seq",
                              "death.situation.combine"),envir=environment())
  system.time(parApply(cl,sim.patients,1,sim.bed.fun))
  stopCluster(cl)
  print(z)
  gc()
}

#par<-sim.patients[1,]
#apply(sim.patients,1,sim.bed.fun)

sim.bed.fun <- function(par){
  #print(par[l+1])
  dif_bed<-data.frame(Critical=icubed,Severe=heavybed,Mild=lightbed)[c(1:l),]
  dif_bed<-data.frame(dif_bed,date=travel.date.seq[1:l],occupied.M=0,occupied.S=0,occupied.C=0,
                      gap.M=0,gap.S=0,gap.C=0)
  patient.situation<-NULL
  death.situation<-NULL
  for(d in 1:l){
    #print(severe.bed)
    #d<-29
    ####add new patients
    new.critical.1<-NULL
    new.critical.2<-NULL
    new.severe<-NULL
    new.mild<-NULL
    if(round(par[d]*0.05*(137/191))!=0){
      new.critical.1<-data.frame(stage1=ceiling(rnorm(round(par[d]*0.05*(137/191)),6,1)),
                                 stage2=ceiling(rnorm(round(par[d]*0.05*(137/191)),5.5,1)),
                                 stage3=ceiling(rnorm(round(par[d]*0.05*(137/191)),6.5,1)),
                                 stage4=ceiling(rnorm(round(par[d]*0.05*(137/191)),3,1)),
                                 situation="mild",bed=0,length.wait=0,length.hos=0,result=0)
    }
    if(round(par[d]*0.05*(54/191))!=0){
      new.critical.2<-data.frame(stage1=ceiling(rnorm(round(par[d]*0.05*(54/191)),6,1)),
                                 stage2=ceiling(rnorm(round(par[d]*0.05*(54/191)),5,1)),
                                 stage3=ceiling(rnorm(round(par[d]*0.05*(54/191)),7.5,1)),
                                 stage4=0,
                                 situation="mild",bed=0,length.wait=0,length.hos=0,result=2)
    }
    if(round(par[d]*0.14)!=0){
      new.severe<-data.frame(stage1=ceiling(rnorm(round(par[d]*0.14),6,1)),
                             stage2=ceiling(rnorm(round(par[d]*0.14),7,1)),
                             stage3=0,
                             stage4=0,
                             situation="mild",bed=0,length.wait=0,length.hos=0,result=0)
    }
    if(round(par[d]*0.81)!=0){
      new.mild<-data.frame(stage1=ceiling(rnorm(round(par[d]*0.81),11,1)),
                           stage2=0,
                           stage3=0,
                           stage4=0,
                           situation="mild",bed=0,length.wait=0,length.hos=0,result=0)
    }
    patient.situation<-rbind(patient.situation,new.critical.1,new.critical.2,new.severe,new.mild)
    patient.situation$situation<-as.character(patient.situation$situation)
    #####hospital available beds
    if(d>1){
      mild.bed<-dif_bed[d,3]-dif_bed[d-1,5]
      severe.bed<-dif_bed[d,2]-dif_bed[d-1,6]
      icu.bed<-dif_bed[d,1]-dif_bed[d-1,7]
    }else{
      mild.bed<-dif_bed[d,3]
      severe.bed<-dif_bed[d,2]
      icu.bed<-dif_bed[d,1]
    }
    
    #####trasfer type of cases
    patient.situation$situation[which(patient.situation$stage1==0 & patient.situation$stage2>0 &
                                        (patient.situation$result==0|patient.situation$result==2))]<-"severe"
    patient.situation$situation[which(patient.situation$stage2==0 & patient.situation$stage3>0 &
                                        (patient.situation$result==0|patient.situation$result==2))]<-"critical"
    patient.situation$situation[which(patient.situation$stage3==0 & patient.situation$stage4>0 &
                                        (patient.situation$result==0|patient.situation$result==2))]<-"severe"
    
    #spcial cases
    patient.situation$situation[which(patient.situation$stage2!=0 & patient.situation$stage1==patient.situation$length.wait &
                                        (patient.situation$result==0|patient.situation$result==2))]<-"severe"
    
    patient.situation$length.wait[which(patient.situation$stage2!=0 & patient.situation$stage1==patient.situation$length.wait &
                                          (patient.situation$result==0|patient.situation$result==2))]<-0
    
    patient.situation$situation[which(patient.situation$stage3!=0 & patient.situation$stage2==patient.situation$length.wait &
                                        (patient.situation$result==0|patient.situation$result==2))]<-"critical"
    
    patient.situation$length.wait[which(patient.situation$stage3!=0 & patient.situation$stage2==patient.situation$length.wait &
                                          (patient.situation$result==0|patient.situation$result==2))]<-0
    
    patient.situation$stage1[which(patient.situation$situation=="severe"|patient.situation$situation=="critical")]<-0
    patient.situation$stage2[which(patient.situation$situation=="critical")]<-0
    
    temp<-length(patient.situation$bed[which(patient.situation$stage3==0 & patient.situation$bed==3
                                             & patient.situation$stage4>0 & patient.situation$result==0)])
    ###priority
    if(severe.bed>temp){
      patient.situation$bed[which(patient.situation$stage3==0 & patient.situation$bed==3
                                  & patient.situation$stage4>0 & patient.situation$result==0)]<-2
      icu.bed<-icu.bed+temp
      severe.bed<-severe.bed-temp
    }else if(severe.bed>0){
      patient.situation$bed[which(patient.situation$stage3==0 & patient.situation$stage4>0 &
                                    (patient.situation$result==0|patient.situation$result==2))][sample(temp,severe.bed,replace = F)]<-2
      severe.bed<-0
      icu.bed<-icu.bed+severe.bed
    }
    
    ####patients admitted
    mild.list<-which(patient.situation$situation=="mild" & patient.situation$bed!=1 &
                       (patient.situation$result==0|patient.situation$result==2))
    severe.list<-which(patient.situation$situation=="severe" & patient.situation$bed!=2 &
                         (patient.situation$result==0|patient.situation$result==2))
    icu.list<-which(patient.situation$situation=="critical" & patient.situation$bed!=3 &
                      (patient.situation$result==0|patient.situation$result==2))
    
    if(length(mild.list)>mild.bed){
      mild.admit<-sample(mild.list,mild.bed,replace = F)
      patient.situation$bed[mild.admit]<-1
    }else{
      patient.situation$bed[mild.list]<-1
    }
    if(length(severe.list)>severe.bed){
      severe.admit<-sample(severe.list,severe.bed,replace = F)
      patient.situation$bed[severe.admit]<-2
    }else{
      patient.situation$bed[severe.list]<-2
    }
    if(length(icu.list)>icu.bed){
      icu.admit<-sample(icu.list,icu.bed,replace = F)
      patient.situation$bed[icu.admit]<-3
    }else{
      patient.situation$bed[icu.list]<-3
    }
    #print(length(severe.list))
    #print(severe.bed)
    #emergency cases
    dif_bed[d,10]<-length(which(patient.situation$situation=="critical" & patient.situation$bed!=3 &
                                  (patient.situation$result==0|patient.situation$result==2)))
    patient.situation$result[which(patient.situation$situation=="critical" & patient.situation$bed!=3 &
                                     (patient.situation$result==0|patient.situation$result==2))]<-4
    patient.situation$bed[which(patient.situation$situation=="critical" & patient.situation$bed!=3 &
                                  patient.situation$result==4)]<-0
    ####record bed count
    dif_bed[d,5]<-length(patient.situation$situation[which(patient.situation$bed==1)])
    dif_bed[d,6]<-length(patient.situation$situation[which(patient.situation$bed==2)])
    dif_bed[d,7]<-length(patient.situation$situation[which(patient.situation$bed==3)])
    
    dif_bed[d,8]<-length(patient.situation$situation[which(patient.situation$situation=="mild" & patient.situation$bed!=1 &
                                                             (patient.situation$result==0|patient.situation$result==2))])
    dif_bed[d,9]<-length(patient.situation$situation[which(patient.situation$situation=="severe" & patient.situation$bed!=2 &
                                                             (patient.situation$result==0|patient.situation$result==2))])
    
    #####calculate days in hospital
    patient.situation$length.wait[which(patient.situation$bed==0 & patient.situation$result==0)]<-
      patient.situation$length.wait[which(patient.situation$bed==0 & patient.situation$result==0)]+1
    patient.situation$length.wait[which(patient.situation$bed==0 & patient.situation$result==2)]<-
      patient.situation$length.wait[which(patient.situation$bed==0 & patient.situation$result==2)]+1
    
    patient.situation$length.hos[which(patient.situation$bed!=0 & patient.situation$result==0)]<-
      patient.situation$length.hos[which(patient.situation$bed!=0 & patient.situation$result==0)]+1
    patient.situation$length.hos[which(patient.situation$bed!=0 & patient.situation$result==2)]<-
      patient.situation$length.hos[which(patient.situation$bed!=0 & patient.situation$result==2)]+1
    
    patient.situation$stage1[which(patient.situation$stage1>0 & patient.situation$bed==1)]<-
      patient.situation$stage1[which(patient.situation$stage1>0 & patient.situation$bed==1)]-1
    
    patient.situation$stage2[which(patient.situation$stage1==0 & patient.situation$stage2>0 & patient.situation$bed==2)]<-
      patient.situation$stage2[which(patient.situation$stage1==0 & patient.situation$stage2>0 & patient.situation$bed==2)]-1
    
    patient.situation$stage3[which(patient.situation$stage2==0 & patient.situation$stage3>0 & patient.situation$bed==3)]<-
      patient.situation$stage3[which(patient.situation$stage2==0 & patient.situation$stage3>0 & patient.situation$bed==3)]-1
    
    patient.situation$stage4[which(patient.situation$stage3==0 & patient.situation$stage4>0 & patient.situation$bed==2)]<-
      patient.situation$stage4[which(patient.situation$stage3==0 & patient.situation$stage4>0 & patient.situation$bed==2)]-1
    
    ##### check discharge
    patient.situation$result[which(patient.situation$stage1==0 & patient.situation$stage2==0 & patient.situation$result==0 &
                                     patient.situation$stage3==0 & patient.situation$stage4==0)]<-1
    patient.situation$result[which(patient.situation$stage1==0 & patient.situation$stage2==0 & patient.situation$result==2 &
                                     patient.situation$stage3==0 & patient.situation$stage4==0)]<-3
    patient.situation$bed[which(patient.situation$stage1==0 & patient.situation$stage2==0 & (patient.situation$result==1|patient.situation$result==3|patient.situation$result==4) &
                                  patient.situation$stage3==0 & patient.situation$stage4==0)]<-0
    
    patient.situation$result[which(patient.situation$stage1!=0 & patient.situation$stage1==patient.situation$length.wait & patient.situation$stage2==0 & patient.situation$result==0 &
                                     patient.situation$stage3==0 & patient.situation$stage4==0)]<-1
    
    death.situation<-rbind(death.situation,length(which(patient.situation$result==3|patient.situation$result==4)))
  }
  write.csv(death.situation,paste0(par[l+1],"_sim_D.csv"))
  write.csv(dif_bed,paste0(par[l+1],"_sims.csv"))
}



plot.gap.fun <- function(path,scenarios.type,plot=F){
  setwd(path)
  namelist<-dir()
  namelist<-namelist[-grep("_D",namelist)]
  gap.s<-NULL
  for(i in namelist){
    #i<-namelist[1]
    temp<-read.csv(i)
    gap.s<-cbind(temp$gap.S,gap.s)
  }
  gap.s.result<-NULL
  for(i in 1:70){
    temp<-quantile(gap.s[i,],c(0.025,0.5,0.975))
    gap.s.result<-rbind(gap.s.result,temp)
  }
  
  gap.c<-NULL
  for(i in namelist){
    #i<-namelist[1]
    temp<-read.csv(i)
    gap.c<-cbind(temp$gap.C,gap.c)
  }
  gap.c.result<-NULL
  for(i in 1:70){
    temp<-quantile(gap.c[i,],c(0.025,0.5,0.975))
    gap.c.result<-rbind(gap.c.result,temp)
  }
  
  gap.m<-NULL
  for(i in namelist){
    #i<-namelist[1]
    temp<-read.csv(i)
    gap.m<-cbind(temp$gap.M,gap.m)
  }
  gap.m.result<-NULL
  for(i in 1:70){
    temp<-quantile(gap.m[i,],c(0.025,0.5,0.975))
    gap.m.result<-rbind(gap.m.result,temp)
  }
  

  get.gap.ci <- function(k,CI=T,chr=NA){
    if(CI==T){
      cbind(paste0(round(mean(k[c(42:51),2]))," (",
                   round(mean(k[c(42:51),1]))," ,",
                   round(mean(k[c(42:51),3])),")"),
            paste0(round(mean(k[c(52:61),2]))," (",
                   round(mean(k[c(52:61),1]))," ,",
                   round(mean(k[c(52:61),3])),")"),
            paste0(round(mean(k[c(62:70),2]))," (",
                   round(mean(k[c(62:70),1]))," ,",
                   round(mean(k[c(62:70),3])),")"))
    }else{
      cbind(rbind(round(mean(k[c(42:51),2])),
            round(mean(k[c(52:61),2])),
            round(mean(k[c(62:70),2]))),
            c("Feb 1 – Feb 10","Feb 11 – Feb 20","Feb 21 – Feb 29"),
            c(chr),c(substr(path,str_locate(path,"comb")[2]+2,
                            str_locate(path,"comb")[2]+3)))
    }
  }
  situation.gap.ci <- data.frame(cbind(get.gap.ci(gap.m.result),
                                       get.gap.ci(gap.s.result),
                                       get.gap.ci(gap.c.result)))
  situation.gap <- data.frame(rbind(get.gap.ci(gap.m.result,F,"mild"),
                                    get.gap.ci(gap.s.result,F,"severe"),
                                    get.gap.ci(gap.c.result,F,"critical")))
  write.csv(situation.gap,paste0("D:/RA-POLYU/lockdown/9-4/simdatacomb/gap",
                                 substr(path,str_locate(path,"comb")[2]+2,
                                        str_locate(path,"comb")[2]+3),".csv"),row.names = F)
  write.csv(situation.gap.ci,paste0("D:/RA-POLYU/lockdown/9-4/simdatacomb/gap.ci",
                                    substr(path,str_locate(path,"comb")[2]+2,
                                           str_locate(path,"comb")[2]+3),".csv"),row.names = F)
  'length(which(patient.situation$result==3|patient.situation$result==4))
  sum(dif_bed$gap.C)
  
  length(which(patient.situation$situation=="critical"))
  
  length(which(patient.situation$situation=="severe"))'
  
  if(plot==T){
    gap.c.sort<-matrix(NA,nrow = 70,ncol = 950)
    for(i in 1:70){
      #i<-50
      k<-gap.c.result
      gap.c.sort[i,]<-gap.c[i,which(gap.c[i,]>=gap.c.result[i,1]&gap.c[i,]<=gap.c.result[i,3])][1:950]
      print(i)
    }
    gap.s.sort<-matrix(NA,nrow = 70,ncol = 950)
    for(i in 1:70){
      #i<-50
      k<-gap.s.result
      gap.s.sort[i,]<-gap.s[i,which(gap.s[i,]>=gap.s.result[i,1]&gap.s[i,]<=gap.s.result[i,3])][1:950]
      print(i)
    }
    gap.m.sort<-matrix(NA,nrow = 70,ncol = 950)
    for(i in 1:70){
      #i<-50
      k<-gap.m.result
      gap.m.sort[i,]<-gap.m[i,which(gap.m[i,]>=gap.m.result[i,1]&gap.m[i,]<=gap.m.result[i,3])][1:950]
      print(i)
    }
    
    plot.c<-data.frame(num=melt(gap.c.sort[c(42:70),]),date=rep(travel.date.seq[42:70],950),
                       group=paste0("Critical",scenarios.type),type="sim")[,c(3:6)]
    plot.m<-data.frame(num=melt(gap.m.sort[c(42:70),]),date=rep(travel.date.seq[42:70],950),
                       group=paste0("Mild",scenarios.type),type="sim")[,c(3:6)]
    plot.s<-data.frame(num=melt(gap.s.sort[c(42:70),]),date=rep(travel.date.seq[42:70],950),
                       group=paste0("Severe",scenarios.type),type="sim")[,c(3:6)]
    
    plot.m.50<-data.frame(num.value=(gap.m.result[c(42:70),2]),date=travel.date.seq[42:70],
                          group=paste0("Mild",scenarios.type),type="est_med")
    plot.c.50<-data.frame(num.value=(gap.c.result[c(42:70),2]),date=travel.date.seq[42:70],
                          group=paste0("Critical",scenarios.type),type="est_med")
    plot.s.50<-data.frame(num.value=(gap.s.result[c(42:70),2]),date=travel.date.seq[42:70],
                          group=paste0("Severe",scenarios.type),type="est_med")
    
    bed.c<-data.frame(num.value=(icubed[42:70]),date=travel.date.seq[42:70],
                      group=paste0("Critical",scenarios.type),type="bed_supply")
    bed.s<-data.frame(num.value=(heavybed[42:70]),date=travel.date.seq[42:70],
                      group=paste0("Severe",scenarios.type),type="bed_supply")
    bed.m<-data.frame(num.value=(lightbed[42:70]),date=travel.date.seq[42:70],
                      group=paste0("Mild",scenarios.type),type="bed_supply")
    plot.bed.total<-rbind(bed.m,bed.s,bed.c)
    plot.gap.med<-rbind(plot.m.50,plot.s.50,plot.c.50)
    plot.gap.total<-rbind(plot.m,plot.s,plot.c)#,
    #                      plot.c.025,plot.s.025,plot.m.025,
    #                      plot.c.50,plot.s.50,plot.m.50,
    #                      plot.c.975,plot.m.975,plot.s.975)
    
    ggplot ()+
      geom_point(data = plot.gap.total, aes(x=as.Date(date), y=num.value,group=type,color=type)) +
      geom_line(data = plot.bed.total, aes(x=as.Date(date), y=num.value,group=type,color=type),size=1.5) +
      geom_line(data = plot.gap.med, aes(x=as.Date(date), y=num.value,group=type,color=type),size=0.5) +
      facet_wrap(~group,scales="free")+
      geom_vline(data = plot.gap.total,aes(xintercept = 18302 ),linetype="dashed",color="black")+
      geom_vline(data = plot.gap.total,aes(xintercept = 18312 ),linetype="dashed",color="black")+
      scale_colour_manual(values= c("darkblue","darkred","gray"),
                          labels = c("beds supply", "estimated beds shortage", "95% CI"))+
      #scale_colour_manual(values= c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
      #labels = c("Real scenario", "reduce_80%", "reduce_50%",
      #"reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
      labs(x= "",y = "Number of beds")+ #,title = "effectiveness of different actions"
      scale_x_date(date_breaks = "9 days",date_labels = "%b-%d")+
      #scale_y_sqrt()+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
            axis.text=element_text(size=12),axis.title.x=element_text(size=14),
            axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
            strip.text.x = element_text(size = 15))->bedpic
    return(bedpic)
  }
}

setwd("D:/RA-POLYU/lockdown/9-4/bed_sim_comb")
sim.comb.name <- dir()
for(i in 1:18){
  plot.gap.fun(path = paste0("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/",sim.comb.name[i]),
               scenarios.type = paste0("scenario_",sim.comb.name[i]))
}

pic1 <- plot.gap.fun("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/10",
                     "",plot = T)
ggsave(filename = "D:/RA-POLYU/lockdown/9-4/bedspic.tiff",
       width = 15, height = 5)

pic2 <- plot.gap.fun("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/20",
                     "scenario_2",plot = T)
pic3 <- plot.gap.fun("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/30",
                     "scenario_3",plot = T)

ggarrange(pic1,pic2,pic3,ncol=1,nrow=3,labels = letters[1:3])
ggsave(filename = "D:/RA-POLYU/lockdown/9-4/bedspic.pdf",
       width = 16, height = 13)


############ find peak

setwd("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/30")
namelist<-dir()
namelist<-namelist[-grep("_D",namelist)]
gap.s<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.s<-cbind(temp$gap.S,gap.s)
}
gap.s.result<-NULL
for(i in 1:70){
  temp<-quantile(gap.s[i,],c(0.025,0.5,0.975))
  gap.s.result<-rbind(gap.s.result,temp)
}

gap.c<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.c<-cbind(temp$gap.C,gap.c)
}
gap.c.result<-NULL
for(i in 1:70){
  temp<-quantile(gap.c[i,],c(0.025,0.5,0.975))
  gap.c.result<-rbind(gap.c.result,temp)
}

gap.m<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.m<-cbind(temp$gap.M,gap.m)
}
gap.m.result<-NULL
for(i in 1:70){
  temp<-quantile(gap.m[i,],c(0.025,0.5,0.975))
  gap.m.result<-rbind(gap.m.result,temp)
}
see <- cbind(gap.m.result,gap.s.result,gap.c.result,as.character(travel.date.seq[1:70]))
#########make table

setwd("D:/RA-POLYU/lockdown/9-4/simdatacomb")
namelist <- dir()[grep("ci",dir())]
sce.table <- matrix(NA,18,10)
sce.table[,1] <- c(10,20,30,11,21,31,
                   12,22,32,13,23,33,
                   14,24,34,15,25,35)
for(i in 1:length(namelist)){
  #i<-1
  temp<-read.csv(namelist[i])
  num.ref<-substr(namelist[i],str_locate(namelist[i],"i")+1,
                  str_locate(namelist[i],"i")+2)
  sce.table[which(sce.table[,1]==num.ref),2:10]<-as.matrix(temp)
}

write.csv(sce.table,"D:/RA-POLYU/lockdown/9-4/sce.table.csv",row.names = F)

setwd("D:/RA-POLYU/lockdown/9-4/simdatacomb")
namelist <- dir()[-grep("ci",dir())]
beds.situation <- NULL

for(i in 1:length(namelist)){
  #i<-1
  temp <- read.csv(namelist[i])
  beds.situation <- rbind(beds.situation,temp)
}
write.csv(beds.situation,"D:/RA-POLYU/lockdown/9-4/bad.table.csv",row.names = F)
############death cases 
filename <- c("10","20","30")
death.sort.comb <- NULL
death.result.comb <- NULL
for(n in filename){
  setwd(paste0("D:/RA-POLYU/lockdown/9-4/bed_sim_comb/",n))
  namelist <- dir()[grep("D",dir())]
  death.result.ref<-NULL
  for(i in namelist){
    #i<-namelist[1]
    temp<-read.csv(i)
    death.result.ref<-cbind(temp$V1,death.result.ref)
  }
  death.result.ref <- death.result.ref[1:70,]
  death.result<-NULL
  for(i in 1:70){
    temp<-quantile(death.result.ref[i,],c(0.025,0.5,0.975))
    death.result<-rbind(death.result,temp)
  }
  death.sort<-matrix(NA,nrow = 70,ncol = 950)
  for(i in 1:70){
    death.sort[i,]<-death.result.ref[i,which(death.result.ref[i,]>=death.result[i,1]&
                                               death.result.ref[i,]<=death.result[i,3])][1:950]
    #print(i)
  }
  death.sort<-data.frame(num=melt(death.sort),date=rep(travel.date.seq[1:70],950),type="sim")
  death.result<-data.frame(death.result,date=travel.date.seq[1:70],type="real")
  death.sort.comb <- rbind(death.sort.comb,data.frame(death.sort,Type=n))
  death.result.comb <- rbind(death.result.comb,data.frame(death.result,Type=n))
}

death.result.comb$Type <- death.result.comb$Type %>% as.character()
death.result.comb$Type[which(death.result.comb$Type==10)] <- "1) scenario_baseline"
death.result.comb$Type[which(death.result.comb$Type==20)] <- "2) scenario_2"
death.result.comb$Type[which(death.result.comb$Type==30)] <- "3) scenario_3"

death.ref<-read.csv("D:/RA-POLYU/lockdown/9-4/death_ref.csv",header = F)
death.report<-data.frame(num=t(death.ref),date=travel.date.seq[1:70],type="reported")
death.result.comb <- death.result.comb %>% filter(Type==10)
ggplot ()+
  geom_ribbon(data = death.result.comb, aes(x=as.Date(date), ymin = X2.5., ymax = X97.5.),
              color="darkgray",fill="gray",alpha = 1,linetype = 2) +
  geom_line(data = death.result.comb, aes(x=as.Date(date), y=X50.,group=type,color=type),size=1) +
  geom_point(data = death.report, aes(x=as.Date(date), y=num,group=type,color=type),size=1) +
  scale_colour_manual(values= c("darkred","darkblue"),labels = c("estimated","reported"))+
  #facet_wrap(~Type,scales="free")+
  labs(x= "",y = "Total number of death")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "20 days",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))
ggsave(filename = "D:/RA-POLYU/lockdown/9-4/death.tiff",width = 7, height = 4)

################# second wave predict

setwd("D:/RA-POLYU/lockdown/9-4/bed_sim_comb_long/37")
namelist <- dir()[-grep("D",dir())]
gap.s<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.s<-cbind(temp$gap.S,gap.s)
}
gap.s.result<-NULL
for(i in 1:255){
  temp<-quantile(gap.s[i,],c(0.025,0.5,0.975))
  gap.s.result<-rbind(gap.s.result,temp)
}

gap.c<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.c<-cbind(temp$gap.C,gap.c)
}
gap.c.result<-NULL
for(i in 1:255){
  temp<-quantile(gap.c[i,],c(0.025,0.5,0.975))
  gap.c.result<-rbind(gap.c.result,temp)
}

gap.m<-NULL
for(i in namelist){
  #i<-namelist[1]
  temp<-read.csv(i)
  gap.m<-cbind(temp$gap.M,gap.m)
}
gap.m.result<-NULL
for(i in 1:255){
  temp<-quantile(gap.m[i,],c(0.025,0.5,0.975))
  gap.m.result<-rbind(gap.m.result,temp)
}
gap.m.result <- cbind(gap.m.result,as.character(travel.date.seq[1:255]))
gap.s.result <- cbind(gap.s.result,as.character(travel.date.seq[1:255]))
gap.c.result <- cbind(gap.c.result,as.character(travel.date.seq[1:255]))

gap.c.result[-(1:100),2] %>% as.numeric() %>% which.max() -> ind.gap
gap.c.result[ind.gap+100,]
gap.m.result[-(1:100),2] %>% as.numeric() %>% which.max() -> ind.gap
gap.m.result[ind.gap+100,]
gap.s.result[-(1:100),2] %>% as.numeric() %>% which.max() -> ind.gap
gap.s.result[ind.gap+100,]
##########################################################
com.dat<-read.csv("D:/RA-POLYU/lockdown/9-4/text_group_data.csv")
setwd("D:/RA-POLYU/lockdown/9-4/simdatacomb")
sim.gap.res <- read.csv("gap10.csv")

copost1<-ceiling(sum(com.dat$num[1:8])/8)
copost2<-ceiling(sum(com.dat$num[9:18])/10)
copost3<-ceiling(sum(com.dat$num[19:27])/9)

co.m1<-sim.gap.res[1,1]
co.m2<-sim.gap.res[2,1]
co.m3<-sim.gap.res[3,1]

co.s1<-sim.gap.res[4,1]+sim.gap.res[7,1]
co.s2<-sim.gap.res[5,1]+sim.gap.res[8,1]
co.s3<-sim.gap.res[6,1]+sim.gap.res[9,1]

'post1<-ceiling(sum(com.dat$num[86:92])/8)
post2<-ceiling(sum(com.dat$num[93:102])/10)
post3<-ceiling(sum(com.dat$num[103:110])/9)
'
post1<-ceiling(sum(com.dat$num[71])/8)
post2<-ceiling(sum(com.dat$num[72:77])/10)
post3<-ceiling(sum(com.dat$num[78:82])/9)

expand.grid(rate1=seq(0,0.03,length.out = 1000)[-1],
            rate2=seq(0,0.03,length.out = 1000)[-1],
            k=seq(0,0.5,length.out = 6)[-c(1)])->param
param$index<-c(1:length(param$rate1))
param <- filter(param,rate1>rate2)
tol.index<-length(param$rate1)
'logll<-function(param){
  temp<-log1p(dbinom(copost1,
                     co.s1+co.m1,
                     param[3]*param[1]+(1-param[3])*param[2]))+
    log1p(dbinom(copost2,
                 co.s2+co.m2,
                 param[3]*param[1]+(1-param[3])*param[2]))+
    log1p(dbinom(copost3,
                 co.s3+co.m3,
                 param[3]*param[1]+(1-param[3])*param[2]))
  print(param[4]/tol.index)
  return(temp)
}'
logll<-function(param){
  temp<-log1p(dbinom(ceiling(copost1*param[3]),
                     co.s1,
                     param[1]))+
    log1p(dbinom(ceiling(copost2*param[3]),
                 co.s2,
                 param[1]))+
    log1p(dbinom(ceiling(copost3*param[3]),
                 co.s3,
                 param[1]))+
    log1p(dbinom(ceiling(copost1*(1-param[3])),
                 co.m1,
                 param[2]))+
    log1p(dbinom(ceiling(copost2*(1-param[3])),
                 co.m2,
                 param[2]))+
    log1p(dbinom(ceiling(copost3*(1-param[3])),
                 co.m3,
                 param[2]))
  print(param[4]/tol.index)
  return(temp)
}
log.result<-apply(param,1,logll)

sim.result <- cbind(log(exp(log.result)-1),param)
for.ci <- filter(sim.result,sim.result[,1]<=(max(sim.result[,1])-0.5*qchisq(0.95,2)))

k<-param[which.max(log.result),3]
rate1<-param[which.max(log.result),1]
rate2<-param[which.max(log.result),2]
copost1*k/rate1
copost1*(1-k)/rate2
copost2*k/rate1
copost2*(1-k)/rate2
copost3*k/rate1
copost3*(1-k)/rate2



number1<-ceiling(post1*k/rate1)
number2<-ceiling(post2*k/rate1)
number3<-ceiling(post3*k/rate1)

number4<-ceiling(post1*(1-k)/rate2)
number5<-ceiling(post2*(1-k)/rate2)
number6<-ceiling(post3*(1-k)/rate2)

d0<-log(dbinom(ceiling(post1*k),
               number1,
               rate1))+
  log(dbinom(ceiling(post2*k),
             number2,
             rate1))+
  log(dbinom(ceiling(post3*k),
             number3,
             rate1))+
  log(dbinom(ceiling(post1*(1-k)),
             number4,
             rate2))+
  log(dbinom(ceiling(post2*(1-k)),
             number5,
             rate2))+
  log(dbinom(ceiling(post3*(1-k)),
             number6,
             rate2))
logCI<-function(x){
  temp<- -(log(dbinom(ceiling(post1*k),
                      #x,
                      number1,
                      rate1))+
             log(dbinom(ceiling(post2*k),
                        #x,
                        number2,
                        rate1))+
             log(dbinom(ceiling(post3*k),
                        #x,
                        number3,
                        rate1))+
             log(dbinom(ceiling(post1*(1-k)),
                        #x,
                        number4,
                        rate2))+
             log(dbinom(ceiling(post2*(1-k)),
                        x,
                        #number5,
                        rate2))+
             log(dbinom(ceiling(post3*(1-k)),
                        #x,
                        number6,
                        rate2)))+d0-0.5*qchisq(0.95,1)
  return(temp)
}

for(i in 0:1000000) if(logCI(i)*logCI(i+1)<0)print(i)

for(i in 1:500){
  print(log(dbinom(ceiling(post1*k),i,rate1)))
}


