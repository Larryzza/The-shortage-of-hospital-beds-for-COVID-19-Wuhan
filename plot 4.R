library(scales)
library(ggplot2)

#setwd("D:/RA-POLYU/lockdown/eva/eva")
source("function eva.R")

draw.function <- function(par=par1,num.days=108){
  t.max <- num.days
  rate=0.01
  a <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9]
  )
  
  rate=0.2
  b <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9]
  )
  #plot(colSums(b$res_China))
  rate=0.5
  c <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9]
  )
  rate=1 # no traffic restriction
  d <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9]
  )
  rate=0.01
  g <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
    t.K.switch=13  #ealy restriction on 1.11
  )
  rate=0.01
  h <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
    t.K.switch=53  #late restriction on 2.5
  )
  current<-colSums(a$onset_data[-51,]) 
  reduce_80<-colSums(b$onset_data[-51,])
  reduce_50<-colSums(c$onset_data[-51,])
  reduce_0<-colSums(d$onset_data[-51,])
  restrict_1.11<-colSums(g$onset_data[-51,])
  restrict_2.5<-colSums(h$onset_data[-51,])
  
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.11,restrict_2.5))
  total$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total)[1]<-"Policy"
  other_50_cities_new<-data.frame(total,type="1) Other 50 cities")
  
  
  current<-cumsum(colSums(a$onset_data[-51,])) 
  reduce_80<-cumsum(colSums(b$onset_data[-51,]))
  reduce_50<-cumsum(colSums(c$onset_data[-51,]))
  reduce_0<-cumsum(colSums(d$onset_data[-51,]))
  restrict_1.11<-cumsum(colSums(g$onset_data[-51,]))
  restrict_2.5<-cumsum(colSums(h$onset_data[-51,]))
  
  total2<-melt(data.frame(current=current/current-1,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.11=(restrict_1.11-current)/current,
                          restrict_2.5=(restrict_2.5-current)/current))
  
  ((restrict_1.11-current)/current)[52]
  ((restrict_2.5-current)/current)[52]
  ((reduce_80-current)/current)[52]
  ((reduce_50-current)/current)[52]
  ((reduce_0-current)/current)[52]
  
  (restrict_1.11-current)[52]
  (restrict_2.5-current)[52]
  (reduce_80-current)[52]
  (reduce_50-current)[52]
  (reduce_0-current)[52]
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  other_50_cities_total<-data.frame(total2,type="1) Other 50 cities")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.11,restrict_2.5))
  total3$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total3)[1]<-"Policy"
  other_50_cities_total3<-data.frame(total3,type="1) Other 50 cities")
  
  current<-a$onset_data[51,]
  reduce_80<-b$onset_data[51,]
  reduce_50<-c$onset_data[51,]
  reduce_0<-d$onset_data[51,]
  restrict_1.11<-g$onset_data[51,]
  restrict_2.5<-h$onset_data[51,]
  
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.11,restrict_2.5))
  total$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total)[1]<-"Policy"
  Wuhan_new<-data.frame(total,type="2) Wuhan")
  
  
  current<-cumsum(a$onset_data[51,]) 
  reduce_80<-cumsum(b$onset_data[51,])
  reduce_50<-cumsum(c$onset_data[51,])
  reduce_0<-cumsum(d$onset_data[51,])
  restrict_1.11<-cumsum(g$onset_data[51,])
  restrict_2.5<-cumsum(h$onset_data[51,])
  
  total2<-melt(data.frame(current=current/current-1,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.11=(restrict_1.11-current)/current,
                          restrict_2.5=(restrict_2.5-current)/current))
  
  ((restrict_1.11-current)/current)[52]
  ((restrict_2.5-current)/current)[52]
  ((reduce_80-current)/current)[52]
  ((reduce_50-current)/current)[52]
  ((reduce_0-current)/current)[52]
  
  (restrict_1.11-current)[52]
  (restrict_2.5-current)[52]
  (reduce_80-current)[52]
  (reduce_50-current)[52]
  (reduce_0-current)[52]
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  Wuhan_total<-data.frame(total2,type="2) Wuhan")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.11,restrict_2.5))
  total3$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total3)[1]<-"Policy"
  Wuhan_total3<-data.frame(total3,type="2) Wuhan")
  
  current<-colSums(a$onset_data) 
  reduce_80<-colSums(b$onset_data)
  reduce_50<-colSums(c$onset_data)
  reduce_0<-colSums(d$onset_data)
  restrict_1.11<-colSums(g$onset_data)
  restrict_2.5<-colSums(h$onset_data)
  
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.11,restrict_2.5))
  total$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total)[1]<-"Policy"
  Total_new<-data.frame(total,type="3) Total 51 cities")
  
  
  
  current<-cumsum(colSums(a$onset_data)) 
  reduce_80<-cumsum(colSums(b$onset_data))
  reduce_50<-cumsum(colSums(c$onset_data))
  reduce_0<-cumsum(colSums(d$onset_data))
  restrict_1.11<-cumsum(colSums(g$onset_data))
  restrict_2.5<-cumsum(colSums(h$onset_data))
  
  total2<-melt(data.frame(current=current/current-1,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.11=(restrict_1.11-current)/current,
                          restrict_2.5=(restrict_2.5-current)/current))
  
  ((restrict_1.11-current)/current)[52]
  ((restrict_2.5-current)/current)[52]
  ((reduce_80-current)/current)[52]
  ((reduce_50-current)/current)[52]
  ((reduce_0-current)/current)[52]
  
  (restrict_1.11-current)[52]
  (restrict_2.5-current)[52]
  (reduce_80-current)[52]
  (reduce_50-current)[52]
  (reduce_0-current)[52]
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  Total_total<-data.frame(total2,type="3) Total 51 cities")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.11,restrict_2.5))
  total3$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total3)[1]<-"Policy"
  Total_total3<-data.frame(total3,type="3) Total 51 cities")
  
  add_new<-rbind(other_50_cities_new,Wuhan_new,Total_new)
  ggplot ()+
    geom_line(data = add_new, aes(x=data, y=value,group=Policy,color=Policy)) +
    facet_wrap(~type,scales="free")+
    scale_colour_manual(values= c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                        labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                   "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
    labs(x= "",y = "Daily number")+ #,title = "effectiveness of different actions"
    scale_x_date(date_breaks = "15 days",date_labels = "%b-%d")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
          axis.text=element_text(size=12),axis.title.x=element_text(size=14),
          axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
          strip.text.x = element_text(size = 15))->pic_1
  
  add_total<-rbind(other_50_cities_total,Wuhan_total,Total_total)
  ggplot ()+
    geom_line(data = add_total, aes(x=data, y=value*100,group=Policy,color=Policy)) +
    facet_wrap(~type,scales="free")+
    scale_colour_manual(values=c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                        labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                   "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
    labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
    #scale_y_continuous(breaks = seq(-50,50,5))+
    scale_x_date(date_breaks = "15 days",date_labels = "%b-%d")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
          axis.text=element_text(size=12),axis.title.x=element_text(size=14),
          axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
          strip.text.x = element_text(size = 15))->pic_2
  
  add_total3<-rbind(other_50_cities_total3,Wuhan_total3,Total_total3)
  ggplot ()+
    geom_line(data = add_total3, aes(x=data, y=value,group=Policy,color=Policy)) +
    facet_wrap(~type,scales="free")+
    scale_colour_manual(values= c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                        labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                   "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
    labs(x= "",y = "Cumulative number")+ #,title = "effectiveness of different actions"
    scale_x_date(date_breaks = "15 days",date_labels = "%b-%d")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
          axis.text=element_text(size=12),axis.title.x=element_text(size=14),
          axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
          strip.text.x = element_text(size = 15))->pic_3
  return(list(add_new=add_new,
              add_total=add_total,
              add_total3=add_total3))
  }



ggarrange(pic_1,pic_2,pic_3,ncol=1,nrow=3,labels = letters[1:3])
ggsave(file=paste0("reduce.pdf"),width=15,height=13)

#############################################################################

#############################################################################

#############################################################################

t.max <- 108
rate=0.01
a <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], # within-city transmission rate
  gamma = fitted_val[3], # recovery rate
  I0W = fitted_val[4], # intiial infected at day 0 (1 Jan 2020)
  alpha = fitted_val[2], # incubation transition rate
  Trans = fitted_val[5], # transportation effect
  other = fitted_val[6], # other gov actions 
  d = fitted_val[7], # % confirmed cases to death 
  p = fitted_val[8]  # public reaction 
)
rate=0.2
b <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8]
)
#plot(colSums(b$res_China))
rate=0.5
c <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8]
)
rate=1 # no traffic restriction
d <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],
  d = fitted_val[7],
  p = fitted_val[8]
)
rate=0.01
g <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],
  d = fitted_val[7],
  p = fitted_val[8],
  t.K.switch=20  #ealy restriction on 1.11
)
rate=0.01
h <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4],
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],
  d = fitted_val[7],
  p = fitted_val[8],
  t.K.switch=44  #late restriction on 2.5
)
current<-colSums(a$onset_data[-51,]) 
reduce_80<-colSums(b$onset_data[-51,])
reduce_50<-colSums(c$onset_data[-51,])
reduce_0<-colSums(d$onset_data[-51,])
restrict_1.11<-colSums(g$onset_data[-51,])
restrict_2.5<-colSums(h$onset_data[-51,])

total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                        restrict_1.11,restrict_2.5))
total$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total)[1]<-"Policy"
other_50_cities_new<-data.frame(total,type="1) Other 50 cities")


current<-cumsum(colSums(a$onset_data[-51,])) 
reduce_80<-cumsum(colSums(b$onset_data[-51,]))
reduce_50<-cumsum(colSums(c$onset_data[-51,]))
reduce_0<-cumsum(colSums(d$onset_data[-51,]))
restrict_1.11<-cumsum(colSums(g$onset_data[-51,]))
restrict_2.5<-cumsum(colSums(h$onset_data[-51,]))

total2<-melt(data.frame(current=current/current-1,
                        reduce_80=(reduce_80-current)/current,
                        reduce_50=(reduce_50-current)/current,
                        reduce_0=(reduce_0-current)/current,
                        restrict_1.11=(restrict_1.11-current)/current,
                        restrict_2.5=(restrict_2.5-current)/current))

((restrict_1.11-current)/current)[108]
((restrict_2.5-current)/current)[108]
((reduce_80-current)/current)[108]
((reduce_50-current)/current)[108]
((reduce_0-current)/current)[108]

(restrict_1.11-current)[108]
(restrict_2.5-current)[108]
(reduce_80-current)[108]
(reduce_50-current)[108]
(reduce_0-current)[108]

total2$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total2)[1]<-"Policy"
other_50_cities_total<-data.frame(total2,type="1) Other 50 cities")

total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                         restrict_1.11,restrict_2.5))
total3$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total3)[1]<-"Policy"
other_50_cities_total3<-data.frame(total3,type="1) Other 50 cities")


current<-a$onset_data[51,]
reduce_80<-b$onset_data[51,]
reduce_50<-c$onset_data[51,]
reduce_0<-d$onset_data[51,]
restrict_1.11<-g$onset_data[51,]
restrict_2.5<-h$onset_data[51,]

#write.csv(data.frame(num=a$onset_data[51,c(1:108)],date=travel.date.seq[1:108]),"est_new_daily.csv")

total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                        restrict_1.11,restrict_2.5))
total$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total)[1]<-"Policy"
Wuhan_new<-data.frame(total,type="2) Wuhan")
ggplot ()+
  geom_line(data = total, aes(x=data, y=value,group=Policy,color=Policy)) +
  
  scale_colour_manual(values=c(hue_pal()(6)),
                      labels = c("current", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
  labs(x= "",y = "Newly reported cases")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "15 days")+
  theme_bw()+theme(legend.position = "none")->pic6
#theme(legend.position = "none")+

current<-cumsum(a$onset_data[51,]) 
reduce_80<-cumsum(b$onset_data[51,])
reduce_50<-cumsum(c$onset_data[51,])
reduce_0<-cumsum(d$onset_data[51,])
restrict_1.11<-cumsum(g$onset_data[51,])
restrict_2.5<-cumsum(h$onset_data[51,])

total2<-melt(data.frame(current=current/current-1,
                        reduce_80=(reduce_80-current)/current,
                        reduce_50=(reduce_50-current)/current,
                        reduce_0=(reduce_0-current)/current,
                        restrict_1.11=(restrict_1.11-current)/current,
                        restrict_2.5=(restrict_2.5-current)/current))

((restrict_1.11-current)/current)[108]
((restrict_2.5-current)/current)[108]
((reduce_80-current)/current)[108]
((reduce_50-current)/current)[108]
((reduce_0-current)/current)[108]

(restrict_1.11-current)[108]
(restrict_2.5-current)[108]
(reduce_80-current)[108]
(reduce_50-current)[108]
(reduce_0-current)[108]

total2$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total2)[1]<-"Policy"
Wuhan_total<-data.frame(total2,type="2) Wuhan")

total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                         restrict_1.11,restrict_2.5))
total3$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total3)[1]<-"Policy"
Wuhan_total3<-data.frame(total3,type="2) Wuhan")

current<-colSums(a$onset_data) 
reduce_80<-colSums(b$onset_data)
reduce_50<-colSums(c$onset_data)
reduce_0<-colSums(d$onset_data)
restrict_1.11<-colSums(g$onset_data)
restrict_2.5<-colSums(h$onset_data)

total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                        restrict_1.11,restrict_2.5))
total$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total)[1]<-"Policy"
Total_new<-data.frame(total,type="3) Total 51 cities")


current<-cumsum(colSums(a$onset_data)) 
reduce_80<-cumsum(colSums(b$onset_data))
reduce_50<-cumsum(colSums(c$onset_data))
reduce_0<-cumsum(colSums(d$onset_data))
restrict_1.11<-cumsum(colSums(g$onset_data))
restrict_2.5<-cumsum(colSums(h$onset_data))

total2<-melt(data.frame(current=current/current-1,
                        reduce_80=(reduce_80-current)/current,
                        reduce_50=(reduce_50-current)/current,
                        reduce_0=(reduce_0-current)/current,
                        restrict_1.11=(restrict_1.11-current)/current,
                        restrict_2.5=(restrict_2.5-current)/current))

((restrict_1.11-current)/current)[108]
((restrict_2.5-current)/current)[108]
((reduce_80-current)/current)[108]
((reduce_50-current)/current)[108]
((reduce_0-current)/current)[108]

(restrict_1.11-current)[108]
(restrict_2.5-current)[108]
(reduce_80-current)[108]
(reduce_50-current)[108]
(reduce_0-current)[108]

total2$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total2)[1]<-"Policy"
Total_total<-data.frame(total2,type="3) Total 51 cities")

total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                         restrict_1.11,restrict_2.5))
total3$data<-rep(travel.date.seq[1:length(current)],6)
colnames(total3)[1]<-"Policy"
Total_total3<-data.frame(total3,type="3) Total 51 cities")

add_new<-rbind(other_50_cities_new,Wuhan_new,Total_new)
ggplot ()+
  geom_line(data = add_new, aes(x=data, y=value,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values= c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
  labs(x= "",y = "Daily number")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->pic_1

add_total<-rbind(other_50_cities_total,Wuhan_total,Total_total)
ggplot ()+
  geom_line(data = add_total, aes(x=data, y=value*100,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values=c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
  labs(x= "",y = "Percentage change of Cumulative number")+ #,title = "effectiveness of different actions"
  #scale_y_continuous(breaks = seq(-50,50,5))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->pic_2

add_total3<-rbind(other_50_cities_total3,Wuhan_total3,Total_total3)
ggplot ()+
  geom_line(data = add_total3, aes(x=data, y=value,group=Policy,color=Policy)) +
  facet_wrap(~type,scales="free")+
  scale_colour_manual(values= c("#F8766D", "#B79F00", "#00BFC4", "#00BA38", "#619CFF", "#F564E3"),
                      labels = c("Real scenario", "reduce_80%", "reduce_50%",
                                 "reduce_0%","restrict_by_1.11","restrict_by_2.5"))+
  labs(x= "",y = "Cumulative number")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->pic_3

ggarrange(pic_1,pic_2,pic_3,ncol=1,nrow=3,labels = letters[1:3])

ggsave(file=paste0("reduce.long.pdf"),width=15,height=13)
########################

########################

########################
t.max <- 131
rate=0.01
a <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], # within-city transmission rate
  gamma = fitted_val[3], # recovery rate
  I0W = fitted_val[4], # intiial infected at day 0 (1 Jan 2020)
  alpha = fitted_val[2], # incubation transition rate
  Trans = fitted_val[5], # transportation effect
  other = fitted_val[6], # other gov actions 
  d = fitted_val[7], # % confirmed cases to death 
  p = fitted_val[8]  # public reaction 
)
rate=0.01
b <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8],
  t.K.switch2=51 #2-11
)
c <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8],
  t.K.switch2=70
)



keep_res<-colSums(a$onset_data)
Feb_11<-colSums(b$onset_data)
Mar_1<-colSums(c$onset_data)
total<- melt(data.frame(Feb_11,Mar_1,keep_res))
total$data<-rep(travel.date.seq[1:length(keep_res)],3)
((cumsum(Feb_11)-cumsum(keep_res))/cumsum(keep_res))[131]
((cumsum(Mar_1)-cumsum(keep_res))/cumsum(keep_res))[131]
colnames(total)[1]<-"Unrestrict_date"

total %>%
  ggplot( aes(x=data, y=value, group=Unrestrict_date, color=Unrestrict_date)) +
  labs(x= "",y = "Newly reported cases")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "1 month",date_labels = "%b-%d")+
  theme_bw()+
  #theme(legend.position = "none")+
  geom_line()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->pic7
#ggsave(file=paste0("reduce_stop.png"),width=5,height=5)

######################################
######################################

t.max <- 192
rate=0.01
a <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], # within-city transmission rate
  gamma = fitted_val[3], # recovery rate
  I0W = fitted_val[4], # intiial infected at day 0 (1 Jan 2020)
  alpha = fitted_val[2], # incubation transition rate
  Trans = fitted_val[5], # transportation effect
  other = fitted_val[6], # other gov actions 
  d = fitted_val[7], # % confirmed cases to death 
  p = fitted_val[8],# public reaction 
  #t.K.switch2=108
  )
b <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8],
  duration = 30,
  #t.K.switch2=108
)
c <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],  
  d = fitted_val[7],
  p = fitted_val[8],
  duration = 40,
  #t.K.switch2=108
)
d <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],
  d = fitted_val[7],
  p = fitted_val[8],
  duration = 50,
  #t.K.switch2=108
)
g <- restrict.epidemic.sim( 
  beta_1 = fitted_val[1], 
  gamma = fitted_val[3], 
  I0W = fitted_val[4], 
  alpha = fitted_val[2], 
  Trans = fitted_val[5],
  other = fitted_val[6],
  d = fitted_val[7],
  p = fitted_val[8],
  duration = 60,
  #t.K.switch2=108
)

#write.csv(data.frame(num=c$onset_data[51,c(1:210)],date=travel.date.seq[1:210]),"est_new_daily.csv")

current<-colSums(a$onset_data) 
`30days`<-colSums(b$onset_data)
`40days`<-colSums(c$onset_data)
`50days`<-colSums(d$onset_data)


total<- melt(data.frame(current,`30days`,`40days`))
total$data<-rep(travel.date.seq[1:length(current)],3)

colnames(total)[1]<-"Waning.weight"

ggplot ()+
  geom_line(data = total, aes(x=data, y=value, group=Waning.weight, color=Waning.weight)) +
  scale_colour_manual(values=c(hue_pal()(5)),
                      labels = c("current", "1/30","1/40"))+
  labs(x= "",y = "Daily number")+ #,title = "effectiveness of different actions"
  scale_x_date(date_breaks = "2 months",date_labels = "%b-%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))->pic8
#ggarrange(pic7,pic8,ncol=2,nrow=1,labels = letters[1:2])
ggsave(pic8,file=paste0("multi_wave.pdf"),width=13,height=7)

#######################################
#####################################


nee<-read.csv("wuhan.csv")
nee$date <- as.Date(nee$date)
colnames(nee)[2]<-"Source"
selee<-nee[which(nee$Source=="Selected"),]
nee<-nee[which(nee$Source!="Selected"),]

colnames(nee)[2]<-"Source"


ggplot ()+
  geom_point(data=nee, aes(x=date, y=sqrt(Wuhan), group=Source, color=Source)) +
  scale_color_manual(values = c("purple","red", "darkblue"))+
  labs(x= "",y = "Newly reported cases")+ #,title = "Overall cases fitting"
  geom_point(data = selee,aes(x=date,y=sqrt(Wuhan)),colour="darkred",size=3,
             alpha=0.8,shape=21)+
  scale_y_continuous(breaks=seq(0,40,10),labels = c(0,100,400,900,1600))+
  scale_x_date(date_breaks = "13 days",date_labels = "%b-%d")+
  geom_point()+
  geom_vline(aes(xintercept = 18278 ),linetype="dashed",color="black")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(size=12),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),legend.text=element_text(size=13),legend.title = element_text(size=13),
        strip.text.x = element_text(size = 15))
ggsave(file=paste0("Newly reported cases.pdf"),width=8,height=5)


'sele.nee<-read.csv("wuhanplot.csv")
sele.nee$date<-as.Date(sele.nee$date)
png(file = "Wuhan cases.png",width=600*3,height=500*3,res=72*4)
sele.nee %>%
  ggplot( aes(x=date)) + #, y=Wuhan, group=Source, color=Source))
  scale_color_manual(values = c("red", "darkblue"))+
  geom_point(aes(y=Wuhan, group=Source, color=Source))+
  labs(x= "Date",y = "Newly reported cases")+ #,title = "Overall cases fitting"
  scale_x_date(date_breaks = "2 days")+
  geom_point(aes(y=From)) + 
  geom_line(aes(y=From),arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed"), size = 1.3)+
  theme_bw()+'

  
dev.off()  



