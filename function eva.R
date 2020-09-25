# version 1.1, 2020-2-8

#########function 1 simulate travel population#############
simulate.travel<-function(travel,travel.ref){
  #travel<-travelout
  #travel.ref<-travelout.ref
  colnames(travel)[1]<-"Date"
  colnames(travel.ref)[1]<-"Date"
  travel.new.front <- NULL
  travel.new.back <- NULL
  for(i in 2:length(travel)){
    lm <- lm(travel[,i]~travel.ref[25:(length(travel$Date)+24),i]-1)
    temp <- travel.ref[(length(travel$Date)+25):length(travel.ref$Date),i]*lm$coefficients
    travel.new.back <- cbind(travel.new.back,temp)
    temp <- travel.ref[15:24,i]*lm$coefficients
    travel.new.front <- cbind(travel.new.front,temp)
  }
  travel.new.front <- data.frame(Date=NA,travel.new.front)
  travel.new.back <- data.frame(Date=NA,travel.new.back)
  colnames(travel.new.back) <- colnames(travel)
  colnames(travel.new.front) <- colnames(travel)
  travel <- rbind(travel.new.front,travel,travel.new.back)
  travel$Date<-as.Date(travel$Date)
  for(i in 26:length(travel$Date)){travel$Date[i]<-travel$Date[i-1]+1}
  for(i in 15:24){travel$Date[25-i]<-travel$Date[26-i]-1}
  travel.date.seq <- travel$Date
  travel <- travel[,-1]
  travel$Wuhan <- 0
  city.seq <- colnames(travel)
  m <- length(city.seq)
  return(list(travel,m,travel.date.seq))
}

#########function 2 epidemic situation#############

restrict.epidemic.sim <- function( beta_1, gamma, sigema, I0W,Trans,other,d,p,Delay_days=2,rate=0.01,
                                   travel_out=travel.out[[1]],travel_in=travel.in[[1]],
                                   t.K.switch=33,t.K.switch2=300,duration=Inf) {
  #set delay rate first
  Delay_rate <- matrix(1/2,nrow=m,ncol=300)
  Delay_wuhan <- c(rep(1/14,26),seq(1/14,1/Delay_days,length.out = 27)[-1]) # 2020-01-31
  Delay_rate[51,c(1:length(Delay_wuhan))] <- Delay_wuhan
  
  # presumed epidemic start
  I0 <- numeric(m)
  I0[which(city.seq=="Wuhan")] <- I0W
  I <- I0
  S <- N - I
  E <- 0
  Delay_confirm <- matrix(0,nrow=m,ncol=1)  
  R <- matrix(0,nrow=m,ncol=1)  
  P <- 0
  #death in wuhan
  D_Wuhan <- matrix(0,nrow=1,ncol=t.max+1)
  # number of confirmed cases (R), China
  res_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(res_China) <- city.seq
  res_China[51,1] <- I0W*gamma*Delay_rate[51,1]
  Delay_confirm[51,1] <- I0W*gamma - res_China[51,1]
  #onset data
  onset_data <- matrix(0,nrow=m,ncol=t.max+1)
  onset_data[51,1] <- I0W*gamma
  # number of new (not existing) infections, China
  I_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(I_China) <- city.seq 
  # number of current infections, China
  Icurr_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(Icurr_China) <- city.seq 
  #travel data
  K1<-matrix(0,nrow=m,ncol=m)
  colnames(K1)<-city.seq
  rownames(K1)<-city.seq
  for (k in 1:t.max){
    #k<-1
    if (!is.na(t.K.switch) & k>=t.K.switch & k<=t.K.switch2) {
      K1[,51]<-as.matrix(travel_in[k,])*rate
      K1[51,]<-as.matrix(travel_out[k,])*rate
    }else{
      K1[,51]<-as.matrix(travel_in[k,])
      K1[51,]<-as.matrix(travel_out[k,])
    }
    #get adjust beta
    if (k >= 20) {
      intensity <- p
      other <- other
    }else{
      intensity <- 0
      other <- 0
    }
    if (k >= 51) {
      Dur<-duration
    }else{
      Dur<-Inf
    }
    
    beta <- beta_1*
      ((I/N)+Trans*t(K1/N)%*%(I/N))*   #transport adjustment
      (1-other)*((1-(P/N[51]))^(intensity))   #gov/pub reaction
    
    S. <- (S+S*colSums(K1)/N-S*rowSums(K1)/N) - S*beta
    
    E. <- S*beta + (E+E*colSums(K1)/N-E*rowSums(K1)/N) - E*(sigema)
    
    I. <- E*(sigema) + (I+I*colSums(K1)/N-I*rowSums(K1)/N) - I*(gamma)  
    
    Delay_confirm. <- Delay_confirm + I*(gamma) - Delay_confirm*Delay_rate[,k+1]
    
    R. <- R + Delay_confirm*Delay_rate[,k+1]
    
    R.[51] <- R.[51] - Delay_confirm[51]*Delay_rate[51,k+1]*d
    
    P <- P + Delay_confirm[51]*Delay_rate[51,k+1]*d - P/Dur
    
    #P <- P + as.matrix(D)[,k+1]
    
    res_China[,k+1] <- Delay_confirm*Delay_rate[,k+1]
    I_China[,k+1] <- S*beta
    Icurr_China[,k+1] <- I.
    D_Wuhan[,k+1] <- Delay_confirm[51]*Delay_rate[51,k+1]*d
    onset_data[,k+1] <- I*(gamma)
    
    N <- N+colSums(K1)-rowSums(K1)
    S <- S.
    E <- E.
    I <- I.
    R <- R.
    Delay_confirm <- Delay_confirm.
  }
  #R0 <- beta/gamma
  #print(paste0("R0_est = ",R0)
  ##R0-CI comes from calculated beta_1 and gamma CI
  return( 
    list( 
      res_China = res_China, 
      I_China = I_China,
      Icurr_China =  Icurr_China,
      D_Wuhan = D_Wuhan,
      onset_data = onset_data
    ) 
  )
}

###########sim for CI
restrict.epidemic.sim.CI <- function(par) {
  travel_out <- travel.out[[1]]
  travel_in <- travel.in[[1]]
  t.K.switch <- 33
  t.K.switch2 <- 300
  duration <- Inf
  beta_1 <- par[1] # within-city transmission rate
  gamma <- par[3] # recovery rate
  sigema <- par[2] # incubation transition rate
  I0W <- par[4] # ascertainment rate in Wuhan
  Trans <- par[5]
  other <- par[6]
  d <- par[7]
  p <- par[8]
  Delay_days <- par[9]
  
  if(is.na(par[10])==F){
    rate <- par[10]
  }
  if(is.na(par[11])==F){
    t.K.switch <- par[11]
  }
  if(is.na(par[12])==F){
    duration <- par[12]
  }
  #set delay rate first
  Delay_rate <- matrix(1/2,nrow=m,ncol=300)
  Delay_wuhan <- c(rep(1/14,26),seq(1/14,1/Delay_days,length.out = 27)[-1]) # 2020-01-31
  Delay_rate[51,c(1:length(Delay_wuhan))] <- Delay_wuhan

  # presumed epidemic start
  I0 <- numeric(m)
  I0[which(city.seq=="Wuhan")] <- I0W
  I <- I0
  S <- N - I
  E <- 0
  Delay_confirm <- matrix(0,nrow=m,ncol=1)  
  R <- matrix(0,nrow=m,ncol=1)  
  P <- 0
  #death in wuhan
  D_Wuhan <- matrix(0,nrow=1,ncol=t.max+1)
  # number of confirmed cases (R), China
  res_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(res_China) <- city.seq
  res_China[51,1] <- I0W*gamma*Delay_rate[51,1]
  Delay_confirm[51,1] <- I0W*gamma - res_China[51,1]
  #onset data
  onset_data <- matrix(0,nrow=m,ncol=t.max+1)
  onset_data[51,1] <- I0W*gamma
  # number of new (not existing) infections, China
  I_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(I_China) <- city.seq 
  # number of current infections, China
  Icurr_China <- matrix(0,nrow=m,ncol=t.max+1)
  rownames(Icurr_China) <- city.seq 
  #travel data
  K1<-matrix(0,nrow=m,ncol=m)
  colnames(K1)<-city.seq
  rownames(K1)<-city.seq
  for (k in 1:t.max){
    #k<-1
    if (!is.na(t.K.switch) & k>=t.K.switch & k<=t.K.switch2) {
      K1[,51]<-as.matrix(travel_in[k,])*rate
      K1[51,]<-as.matrix(travel_out[k,])*rate
    }else{
      K1[,51]<-as.matrix(travel_in[k,])
      K1[51,]<-as.matrix(travel_out[k,])
    }
    #get adjust beta
    if (k >= 20) {
      intensity <- p
      other <- other
    }else{
      intensity <- 0
      other <- 0
    }
    if (k >= 51) {
      Dur<-duration
    }else{
      Dur<-Inf
    }
    
    beta <- beta_1*
      ((I/N)+Trans*t(K1/N)%*%(I/N))*   #transport adjustment
      (1-other)*((1-(P/N[51]))^(intensity))   #gov/pub reaction
    
    S. <- (S+S*colSums(K1)/N-S*rowSums(K1)/N) - S*beta
    
    E. <- S*beta + (E+E*colSums(K1)/N-E*rowSums(K1)/N) - E*(sigema)
    
    I. <- E*(sigema) + (I+I*colSums(K1)/N-I*rowSums(K1)/N) - I*(gamma)  
    
    Delay_confirm. <- Delay_confirm + I*(gamma) - Delay_confirm*Delay_rate[,k+1]
    
    R. <- R + Delay_confirm*Delay_rate[,k+1]
    
    R.[51] <- R.[51] - Delay_confirm[51]*Delay_rate[51,k+1]*d
    
    P <- P + Delay_confirm[51]*Delay_rate[51,k+1]*d - P/Dur
    
    #P <- P + as.matrix(D)[,k+1]
    
    res_China[,k+1] <- Delay_confirm*Delay_rate[,k+1]
    I_China[,k+1] <- S*beta
    Icurr_China[,k+1] <- I.
    D_Wuhan[,k+1] <- Delay_confirm[51]*Delay_rate[51,k+1]*d
    onset_data[,k+1] <- I*(gamma)
    
    N <- N+colSums(K1)-rowSums(K1)
    S <- S.
    E <- E.
    I <- I.
    R <- R.
    Delay_confirm <- Delay_confirm.
  }
  #R0 <- beta/gamma
  #print(paste0("R0_est = ",R0)
  ##R0-CI comes from calculated beta_1 and gamma CI
  return( 
    list( 
      res_China = res_China, 
      onset_data = onset_data
    ) 
  )
}


#########function 3 MLE#############
#par<-fitted_val
logLikelihood <- function(par) {
  beta_1 <- par[1] # within-city transmission rate
  gamma <- par[3] # recovery rate
  I0W <- par[4] # ascertainment rate in Wuhan
  sigema <- par[2] # incubation transition rate
  Trans <- par[5]
  other <- par[6]
  d <- par[7]
  p <- par[8]
  Delay_days <- par[9]
  sim <- restrict.epidemic.sim(beta_1, gamma, sigema,I0W,Trans,other,d,p,Delay_days)
  
  #print( lapply(obs, dim) )
  #print( lapply(sim, dim) )
  #print( paste( beta_1, gamma, phi, I0W ) )
  #browser()
  phi<-matrix(1,nrow=m,ncol=t.max+1)
  phi[,c(1:20)]<-0
  phi[51,]<-c(rep(0.018,13),seq(0.018,0.03,length.out = 15)[-1],
              seq(0.03,0.14,length.out = 6)[-1],
              seq(0.14,0.34,length.out = 17)[-1],
              seq(0.34,0.353,length.out = 11)[-1],
              rep(0.353,150))[1:(t.max+1)]*1.5
  
  #phi.d<-c(rep(0,20),rep(0.667,150))[1:(t.max+1)]
  # China
  sim$res_China <- phi*sim$res_China #  reported cases adjust
  #sim$D_Wuhan <- phi.d*sim$D_Wuhan #  death cases adjust
  sim$res_China[which(sim$res_China<=0)]<-0
  #sim$D_Wuhan[which(sim$D_Wuhan<=0)]<-0
  china <- dpois(as.matrix(obs[,1:(t.max+1)]), sim$res_China,log = T)
  #death <- dpois(as.matrix(D[1:(t.max+1)]),sim$D_Wuhan,log = T)
  #print(par[9]/length(fit.val[,1]))
  return(sum(china))
}


#########function 4 Calculate CI#############
find_CI <- function(comb.sim,par){
  ci.com <- NULL
  par[11]<-round(par[1]/par[3],2)
  mle_value_ref <- comb.sim$fit.resul[which.max(comb.sim$fit.resul)] - 0.5*qchisq(0.95,6)
  comb.sim_ref <- comb.sim[order(comb.sim$fit.resul,decreasing = T),-c(1:2)]
  for(k in c(1:5,8,9,11)){
    #k=1
    i <- 1
    temp <- comb.sim_ref[,k]
    ind <- 1
    ci.list <- par[k]
    while(i <= 1){
      if(temp[ind] > par[k]){
        ci.list<-c(ci.list,temp[ind])
        i <- i+1
        print(i)
      } 
      ind <- ind+1
      #print(ind)
    }
    ind <- 1
    while(i <= 2){
      if(temp[ind] < par[k]){
        ci.list<-c(ci.list,temp[ind])
        i <- i+1
        print(i)
      } 
      ind <- ind+1
      #print(ind)
    }
    ci.com <- rbind(ci.com,ci.list)
  }
  
  #ci.com <- cbind(ci.com,par[c(1:8)])
  return(ci.com)
}

########sim CI results function

sim.ci.results.fun <- function(ci.com,s.factor,days.num=51,type=0,citys=F){
  t.max<-days.num
  phi<-matrix(1,nrow=m,ncol=t.max+1)
  phi[,c(1:20)]<-0
  phi[51,]<-c(rep(0.018,13),seq(0.018,0.03,length.out = 15)[-1],
              seq(0.03,0.14,length.out = 6)[-1],
              seq(0.14,0.34,length.out = 17)[-1],
              seq(0.34,0.353,length.out = 11)[-1],
              rep(0.353,150))[1:(t.max+1)]*s.factor
  sim_test <- expand.grid(c(ci.com[1,3],ci.com[1,2]), #beta_1
                          c(ci.com[2,3],ci.com[2,2]), #sigema
                          c(ci.com[3,3],ci.com[3,2]), #gamma
                          c(ci.com[4,3],ci.com[4,2]), #I0W
                          c(ci.com[5,3],ci.com[5,2]), #Trans
                          0, #other
                          0.045, #d
                          c(ci.com[6,3],ci.com[6,2]), #p
                          c(ci.com[7,3],ci.com[7,2]))
  if(type==1){sim_test <- as.matrix(cbind(sim_test,0.01,53))}
  if(type==2){sim_test <- as.matrix(cbind(sim_test,0.01,13))}
  if(type==3){sim_test <- as.matrix(cbind(sim_test,0.2,33))}
  if(type==4){sim_test <- as.matrix(cbind(sim_test,0.5,33))}
  if(type==5){sim_test <- as.matrix(cbind(sim_test,1,33))}
  
  if(type==6){sim_test <- as.matrix(cbind(sim_test,0.01,33,30))}
  if(type==7){sim_test <- as.matrix(cbind(sim_test,0.01,33,40))}
  #sim_test <- sim_test[1:150000,]
  cl.cores <- detectCores(logical = T)
  cl <- makeCluster(cl.cores)
  clusterExport(cl, varlist=c("restrict.epidemic.sim","m","N","rate","obs","D",
                              "city.seq","t.max","travel.in","travel.out",
                              "sim_test"),envir=environment())
  clusterEvalQ(cl, library(tidyverse))
  
  system.time(sim.resul <- parApply(cl,sim_test,1,restrict.epidemic.sim.CI))
  stopCluster(cl)
  
  comb.sim.ci.res <- NULL
  comb.sim.ci.onset <- NULL
  if(citys==F){
    for(i in 1:length(sim.resul)){
      comb.sim.ci.res<-cbind(comb.sim.ci.res,sim.resul[[i]][["res_China"]][51,])
      comb.sim.ci.onset<-cbind(comb.sim.ci.onset,sim.resul[[i]][["onset_data"]][51,])
    }
    if(days.num==51){
      sim.ci.res <- cbind(apply(comb.sim.ci.res, 1, max),
                          apply(comb.sim.ci.res, 1, min))
      sim.ci.onset <- cbind(apply(comb.sim.ci.onset, 1, max),
                            apply(comb.sim.ci.onset, 1, min))
      sim.report <- phi[51,]*(sim.ci.res)
      
      sim.compa <- rbind(sim.ci.onset,sim.report,sim.ci.res)
      colnames(sim.compa) <- c("hi", "lo")
    }else{
      sim.compa <- cbind(apply(comb.sim.ci.onset, 1, max),
                         apply(comb.sim.ci.onset, 1, min))
      colnames(sim.compa) <- c("hi", "lo")
    }
  }else{
    for(i in 1:length(sim.resul)){
      comb.sim.ci.res<-cbind(comb.sim.ci.res,colSums(sim.resul[[i]][["res_China"]][-51,]))
      comb.sim.ci.onset<-cbind(comb.sim.ci.onset,colSums(sim.resul[[i]][["onset_data"]][-51,]))
      sim.compa <- cbind(apply(comb.sim.ci.onset, 1, max),
                         apply(comb.sim.ci.onset, 1, min))
      colnames(sim.compa) <- c("hi", "lo")
    }
  }
  return(data.frame(sim.compa))
  }

####prepare data for fig


compa.for.pic <- function(sim.compa=sim.compa.s1,par=par1,s.factor=1,name.char,citys=F){
  rate<-0.01
  #t.max <- 51 # 11 Feb 2020
  phi<-matrix(1,nrow=m,ncol=t.max+1)
  phi[,c(1:20)]<-0
  phi[51,]<-c(rep(0.018,13),seq(0.018,0.03,length.out = 15)[-1],
              seq(0.03,0.14,length.out = 6)[-1],
              seq(0.14,0.34,length.out = 17)[-1],
              seq(0.34,0.353,length.out = 11)[-1],
              rep(0.353,150))[1:(t.max+1)]*s.factor
  #sim.compa[c(53:104),] <- sim.compa[c(53:104),]*s.factor
  test.set <- restrict.epidemic.sim.CI(par)
  if(citys==F){
    if(t.max==51){
      est_real<-test.set$onset_data[51,]
      est_confirmed<-test.set$res_China[51,]
      est_report<-phi[51,]*(test.set$res_China[51,])
      report<-data.frame(value=as.matrix(obs)[51,],date=travel.date.seq[1:length(est_report)])
      
      compa<-melt(data.frame('estimates by onset date'=est_real,
                             'model fit to observed data'=est_report,
                             'estimates by diagnosis date'=est_confirmed))
      compa$date<-rep(travel.date.seq[1:length(est_report)],3)
      colnames(compa)[1]<-"Type"
      Total_fit<-data.frame(compa,type="1) Daily number")
      Total_report<-data.frame(report,type="1) Daily number")
      compa <- cbind(compa,sim.compa)
    }else{
      compa<-test.set$onset_data[51,]
    }
  }else{
    compa<-colSums(test.set$onset_data[-51,])
  }
  return(data.frame(compa,sim.compa,name.char))
}

###########draw
draw.function <- function(par=par1,num.days=108,name.char){
  t.max <- num.days
  
  a <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
  )
  
  b <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
    rate = 0.2
  )
  #plot(colSums(b$res_China))

  c <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
    rate = 0.5
  )
   # no traffic restriction
  d <- restrict.epidemic.sim( 
    beta_1 = par[1], # within-city transmission rate
    gamma = par[3], # recovery rate
    sigema = par[2], # incubation transition rate
    I0W = par[4], # ascertainment rate in Wuhan
    Trans = par[5],
    other = par[6],
    d = par[7],
    p = par[8],
    Delay_days = par[9],
    rate = 1
  )
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
  restrict_1.3<-colSums(g$onset_data[-51,])
  restrict_2.12<-colSums(h$onset_data[-51,])
  #print(b$onset_data-a$onset_data)
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.3,restrict_2.12))
  total$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total)[1]<-"Policy"
  other_50_cities_new<-data.frame(total,type="1) Other 50 cities")
  
  
  current<-cumsum(colSums(a$onset_data[-51,])) 
  reduce_80<-cumsum(colSums(b$onset_data[-51,]))
  reduce_50<-cumsum(colSums(c$onset_data[-51,]))
  reduce_0<-cumsum(colSums(d$onset_data[-51,]))
  restrict_1.3<-cumsum(colSums(g$onset_data[-51,]))
  restrict_2.12<-cumsum(colSums(h$onset_data[-51,]))
  
  total2<-melt(data.frame(current=0,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.3=(restrict_1.3-current)/current,
                          restrict_2.12=(restrict_2.12-current)/current))
  #print(reduce_80-current)
  
  print(c(((restrict_1.3-current)/current)[108],
          ((restrict_2.12-current)/current)[108],
          ((reduce_80-current)/current)[108],
          ((reduce_50-current)/current)[108],
          ((reduce_0-current)/current)[108]))
  
  print(c((restrict_1.3-current)[108],
          (restrict_2.12-current)[108],
          (reduce_80-current)[108],
          (reduce_50-current)[108],
          (reduce_0-current)[108]))
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  other_50_cities_total<-data.frame(total2,type="1) Other 50 cities")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.3,restrict_2.12))
  total3$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total3)[1]<-"Policy"
  other_50_cities_total3<-data.frame(total3,type="1) Other 50 cities")
  
  
  current<-a$onset_data[51,]
  reduce_80<-b$onset_data[51,]
  reduce_50<-c$onset_data[51,]
  reduce_0<-d$onset_data[51,]
  restrict_1.3<-g$onset_data[51,]
  restrict_2.12<-h$onset_data[51,]
  
  #write.csv(data.frame(num=a$onset_data[51,c(1:108)],date=travel.date.seq[1:108]),"est_new_daily.csv")
  
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.3,restrict_2.12))
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
  restrict_1.3<-cumsum(g$onset_data[51,])
  restrict_2.12<-cumsum(h$onset_data[51,])
  
  total2<-melt(data.frame(current=0,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.3=(restrict_1.3-current)/current,
                          restrict_2.12=(restrict_2.12-current)/current))
  
  print(c(((restrict_1.3-current)/current)[108],
          ((restrict_2.12-current)/current)[108],
          ((reduce_80-current)/current)[108],
          ((reduce_50-current)/current)[108],
          ((reduce_0-current)/current)[108]))
  
  print(c((restrict_1.3-current)[108],
          (restrict_2.12-current)[108],
          (reduce_80-current)[108],
          (reduce_50-current)[108],
          (reduce_0-current)[108]))
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  Wuhan_total<-data.frame(total2,type="2) Wuhan")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.3,restrict_2.12))
  total3$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total3)[1]<-"Policy"
  Wuhan_total3<-data.frame(total3,type="2) Wuhan")
  
  current<-colSums(a$onset_data) 
  reduce_80<-colSums(b$onset_data)
  reduce_50<-colSums(c$onset_data)
  reduce_0<-colSums(d$onset_data)
  restrict_1.3<-colSums(g$onset_data)
  restrict_2.12<-colSums(h$onset_data)
  
  total<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                          restrict_1.3,restrict_2.12))
  total$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total)[1]<-"Policy"
  Total_new<-data.frame(total,type="3) Total 51 cities")
  
  
  current<-cumsum(colSums(a$onset_data)) 
  reduce_80<-cumsum(colSums(b$onset_data))
  reduce_50<-cumsum(colSums(c$onset_data))
  reduce_0<-cumsum(colSums(d$onset_data))
  restrict_1.3<-cumsum(colSums(g$onset_data))
  restrict_2.12<-cumsum(colSums(h$onset_data))
  
  total2<-melt(data.frame(current=0,
                          reduce_80=(reduce_80-current)/current,
                          reduce_50=(reduce_50-current)/current,
                          reduce_0=(reduce_0-current)/current,
                          restrict_1.3=(restrict_1.3-current)/current,
                          restrict_2.12=(restrict_2.12-current)/current))
  
  print(c(((restrict_1.3-current)/current)[108],
          ((restrict_2.12-current)/current)[108],
          ((reduce_80-current)/current)[108],
          ((reduce_50-current)/current)[108],
          ((reduce_0-current)/current)[108]))
  
  print(c((restrict_1.3-current)[108],
          (restrict_2.12-current)[108],
          (reduce_80-current)[108],
          (reduce_50-current)[108],
          (reduce_0-current)[108]))
  
  total2$data<-rep(travel.date.seq[1:length(current)],6)
  colnames(total2)[1]<-"Policy"
  Total_total<-data.frame(total2,type="3) Total 51 cities")
  
  total3<- melt(data.frame(current,reduce_80,reduce_50,reduce_0,
                           restrict_1.3,restrict_2.12))
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


  #print(add_total$value)
  return(list(add_new=data.frame(add_new,name.char),
              add_total=data.frame(add_total,name.char),
              add_total3=data.frame(add_total3,name.char)))
}

############ generate data fot fig (dif traffic restriction)

traffic.sim <- function(city){
  test0<-sim.ci.results.fun(ci.com.s1,1,108,type=0,citys = city)
  test1<-sim.ci.results.fun(ci.com.s1,1,108,type=1,citys = city)
  test2<-sim.ci.results.fun(ci.com.s1,1,108,type=2,citys = city)
  test3<-sim.ci.results.fun(ci.com.s1,1,108,type=3,citys = city)
  test4<-sim.ci.results.fun(ci.com.s1,1,108,type=4,citys = city)
  test5<-sim.ci.results.fun(ci.com.s1,1,108,type=5,citys = city)
  
  test00<- data.frame(compa.for.pic(test0,par1[1:9],1,"scenario_baseline",citys = city),type="0")
  test11<- data.frame(compa.for.pic(test1,c(par1[1:9],0.01,53),1,"scenario_baseline",citys = city),type="1")
  test22<- data.frame(compa.for.pic(test2,c(par1[1:9],0.01,13),1,"scenario_baseline",citys = city),type="2")
  test33<- data.frame(compa.for.pic(test3,c(par1[1:9],0.2,33),1,"scenario_baseline",citys = city),type="3")
  test44<- data.frame(compa.for.pic(test4,c(par1[1:9],0.5,33),1,"scenario_baseline",citys = city),type="4")
  test55<- data.frame(compa.for.pic(test5,c(par1[1:9],1,33),1,"scenario_baseline",citys = city),type="5")
  
  comb.dif.si1 <- rbind(test00,test11,test22,test33,test44,test55)
  
  test0<-sim.ci.results.fun(ci.com.s2,1.5,108,type=0,citys = city)
  test1<-sim.ci.results.fun(ci.com.s2,1.5,108,type=1,citys = city)
  test2<-sim.ci.results.fun(ci.com.s2,1.5,108,type=2,citys = city)
  test3<-sim.ci.results.fun(ci.com.s2,1.5,108,type=3,citys = city)
  test4<-sim.ci.results.fun(ci.com.s2,1.5,108,type=4,citys = city)
  test5<-sim.ci.results.fun(ci.com.s2,1.5,108,type=5,citys = city)
  
  test00<- data.frame(compa.for.pic(test0,par2[1:9],1,"scenario_2",citys = city),type="0")
  test11<- data.frame(compa.for.pic(test1,c(par2[1:9],0.01,53),1,"scenario_2",citys = city),type="1")
  test22<- data.frame(compa.for.pic(test2,c(par2[1:9],0.01,13),1,"scenario_2",citys = city),type="2")
  test33<- data.frame(compa.for.pic(test3,c(par2[1:9],0.2,33),1,"scenario_2",citys = city),type="3")
  test44<- data.frame(compa.for.pic(test4,c(par2[1:9],0.5,33),1,"scenario_2",citys = city),type="4")
  test55<- data.frame(compa.for.pic(test5,c(par2[1:9],1,33),1,"scenario_2",citys = city),type="5")
  
  comb.dif.si2 <- rbind(test00,test11,test22,test33,test44,test55)
  
  test0<-sim.ci.results.fun(ci.com.s3,0.5,108,type=0,citys = city)
  test1<-sim.ci.results.fun(ci.com.s3,0.5,108,type=1,citys = city)
  test2<-sim.ci.results.fun(ci.com.s3,0.5,108,type=2,citys = city)
  test3<-sim.ci.results.fun(ci.com.s3,0.5,108,type=3,citys = city)
  test4<-sim.ci.results.fun(ci.com.s3,0.5,108,type=4,citys = city)
  test5<-sim.ci.results.fun(ci.com.s3,0.5,108,type=5,citys = city)
  
  test00<- data.frame(compa.for.pic(test0,par3[1:9],1,"scenario_3",citys = city),type="0")
  test11<- data.frame(compa.for.pic(test1,c(par3[1:9],0.01,53),1,"scenario_3",citys = city),type="1")
  test22<- data.frame(compa.for.pic(test2,c(par3[1:9],0.01,13),1,"scenario_3",citys = city),type="2")
  test33<- data.frame(compa.for.pic(test3,c(par3[1:9],0.2,33),1,"scenario_3",citys = city),type="3")
  test44<- data.frame(compa.for.pic(test4,c(par3[1:9],0.5,33),1,"scenario_3",citys = city),type="4")
  test55<- data.frame(compa.for.pic(test5,c(par3[1:9],1,33),1,"scenario_3",citys = city),type="5")
  
  comb.dif.si3 <- rbind(test00,test11,test22,test33,test44,test55)
  
  return(rbind(comb.dif.si1,comb.dif.si2,comb.dif.si3))
}

####generate par value comb

make.par.types <- function(par.input){
  par.types <- matrix(NA,8,12)
  for(i in 1:8){par.types[i,1:9] <- par.input[1:9]}
  par.types[,10] <- c(rep(0.01,3),0.2,0.5,1,rep(0.01,2))
  par.types[,11] <- c(33,53,13,rep(33,5))
  par.types[,12] <- c(rep(NA,6),30,40)
  return(par.types)
}

############## for city scenarios table

cities.result  <- function(input=comb.dif.wuhan){
  org1<-filter(input,type==0,name.char=="scenario_baseline")
  org2<-filter(input,type==0,name.char=="scenario_2")
  org3<-filter(input,type==0,name.char=="scenario_3")
  com.res1 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_baseline")
    temp<-paste0(round(sum(temp$compa)-sum(org1$compa))," (",
                 round(sum(temp$lo)-sum(org1$lo)),", ",
                 round(sum(temp$hi)-sum(org1$hi)),")")
    com.res1 <- cbind(com.res1,temp)
  }
  com.res2 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_2")
    temp<-paste0(round(sum(temp$compa)-sum(org2$compa))," (",
                 round(sum(temp$lo)-sum(org2$lo)),", ",
                 round(sum(temp$hi)-sum(org2$hi)),")")
    com.res2 <- cbind(com.res2,temp)
  }
  com.res3 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_3")
    temp<-paste0(round(sum(temp$compa)-sum(org3$compa))," (",
                 round(sum(temp$lo)-sum(org3$lo)),", ",
                 round(sum(temp$hi)-sum(org3$hi)),")")
    com.res3 <- cbind(com.res3,temp)
  }
  com.res<-rbind(com.res1,com.res2,com.res3)
  return(com.res)
}

cities.result.percentage  <- function(input=comb.dif.wuhan){
  org1<-filter(input,type==0,name.char=="scenario_baseline")
  org2<-filter(input,type==0,name.char=="scenario_2")
  org3<-filter(input,type==0,name.char=="scenario_3")
  com.res1 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_baseline")
    temp<-paste0(round((sum(temp$compa)-sum(org1$compa))/sum(org1$compa),3)*100,"% (",
                 round((sum(temp$lo)-sum(org1$lo))/sum(org1$compa),3)*100,"%, ",
                 round((sum(temp$hi)-sum(org1$hi))/sum(org1$compa),3)*100,"%)")
    com.res1 <- cbind(com.res1,temp)
  }
  com.res2 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_2")
    temp<-paste0(round((sum(temp$compa)-sum(org2$compa))/sum(org2$compa),3)*100,"% (",
                 round((sum(temp$lo)-sum(org2$lo))/sum(org2$compa),3)*100,"%, ",
                 round((sum(temp$hi)-sum(org2$hi))/sum(org2$compa),3)*100,"%)")
    com.res2 <- cbind(com.res2,temp)
  }
  com.res3 <- NULL
  for(n in 1:5){
    #n=0
    temp<-filter(input,type==n,name.char=="scenario_3")
    temp<-paste0(round((sum(temp$compa)-sum(org3$compa))/sum(org3$compa),3)*100,"% (",
                 round((sum(temp$lo)-sum(org3$lo))/sum(org3$compa),3)*100,"%, ",
                 round((sum(temp$hi)-sum(org3$hi))/sum(org3$compa),3)*100,"%)")
    com.res3 <- cbind(com.res3,temp)
  }
  com.res<-rbind(com.res1,com.res2,com.res3)
  return(com.res)
}

#######################


sim.ci.results.fun.dif.sce <- function(ci.com=ci.com.s3,s.factor=1,days.num=51,type=0,citys=F){
  t.max<-days.num
  phi<-matrix(1,nrow=m,ncol=t.max+1)
  phi[,c(1:20)]<-0
  phi[51,]<-c(rep(0.018,13),seq(0.018,0.03,length.out = 15)[-1],
              seq(0.03,0.14,length.out = 6)[-1],
              seq(0.14,0.34,length.out = 17)[-1],
              seq(0.34,0.353,length.out = 11)[-1],
              rep(0.353,150))[1:(t.max+1)]*s.factor
  sim_test <- expand.grid(c(ci.com[1,3],ci.com[1,2]), #beta_1
                          c(ci.com[2,3],ci.com[2,2]), #sigema
                          c(ci.com[3,3],ci.com[3,2]), #gamma
                          c(ci.com[4,3],ci.com[4,2]), #I0W
                          c(ci.com[5,3],ci.com[5,2]), #Trans
                          0, #other
                          0.045, #d
                          c(ci.com[6,3],ci.com[6,2]), #p
                          c(ci.com[7,3],ci.com[7,2]))
  
  sim_test_org <- sim_test
  
  if(type==1){sim_test <- as.matrix(cbind(sim_test,0.01,53))}
  if(type==2){sim_test <- as.matrix(cbind(sim_test,0.01,13))}
  if(type==3){sim_test <- as.matrix(cbind(sim_test,0.2,33))}
  if(type==4){sim_test <- as.matrix(cbind(sim_test,0.5,33))}
  if(type==5){sim_test <- as.matrix(cbind(sim_test,1,33))}
  
  if(type==6){sim_test <- as.matrix(cbind(sim_test,0.01,33,30))}
  if(type==7){sim_test <- as.matrix(cbind(sim_test,0.01,33,40))}
  
  
  #sim_test <- sim_test[1:150000,]
  cl.cores <- detectCores(logical = T)
  cl <- makeCluster(cl.cores)
  clusterExport(cl, varlist=c("restrict.epidemic.sim","m","N","rate","obs","D",
                              "city.seq","t.max","travel.in","travel.out",
                              "sim_test"),envir=environment())
  clusterEvalQ(cl, library(tidyverse))
  
  system.time(sim.resul <- parApply(cl,sim_test,1,restrict.epidemic.sim.CI))
  stopCluster(cl)
  
  #for org
  cl.cores <- detectCores(logical = T)
  cl <- makeCluster(cl.cores)
  clusterExport(cl, varlist=c("restrict.epidemic.sim","m","N","rate","obs","D",
                              "city.seq","t.max","travel.in","travel.out",
                              "sim_test"),envir=environment())
  clusterEvalQ(cl, library(tidyverse))
  
  system.time(sim.resul.org <- parApply(cl,sim_test_org,1,restrict.epidemic.sim.CI))
  stopCluster(cl)
  
  comb.sim.ci.res <- NULL
  comb.sim.ci.onset <- NULL
  comb.sim.ci.res.per <- NULL
  comb.sim.ci.onset.per <- NULL
  if(citys==F){
    for(i in 1:length(sim.resul)){
      #i=1
      comb.sim.ci.res<-cbind(comb.sim.ci.res,(cumsum(sim.resul[[i]][["res_China"]][51,])-
                                                cumsum(sim.resul.org[[i]][["res_China"]][51,])))
      comb.sim.ci.onset<-cbind(comb.sim.ci.onset,(cumsum(sim.resul[[i]][["onset_data"]][51,]-
                                                    sim.resul.org[[i]][["onset_data"]][51,])))
      comb.sim.ci.res.per<-cbind(comb.sim.ci.res.per,(cumsum(sim.resul[[i]][["res_China"]][51,])-
                                                    cumsum(sim.resul.org[[i]][["res_China"]][51,]))/
                                   cumsum(sim.resul.org[[i]][["res_China"]][51,]))
      comb.sim.ci.onset.per<-cbind(comb.sim.ci.onset.per,(cumsum(sim.resul[[i]][["onset_data"]][51,])-
                                                        cumsum(sim.resul.org[[i]][["onset_data"]][51,]))/
                                                    cumsum(sim.resul.org[[i]][["onset_data"]][51,]))
      sim.compa <- cbind(apply(comb.sim.ci.onset, 1, mean),
                         apply(comb.sim.ci.onset, 1, max),
                         apply(comb.sim.ci.onset, 1, min),
                         apply(comb.sim.ci.onset.per, 1, mean),
                         apply(comb.sim.ci.onset.per, 1, max),
                         apply(comb.sim.ci.onset.per, 1, min))
      colnames(sim.compa) <- rep(c("mean","hi", "lo"),2)
    }
  }else if(citys==T){
    for(i in 1:length(sim.resul)){
      comb.sim.ci.res<-cbind(comb.sim.ci.res,(cumsum(colSums(sim.resul[[i]][["res_China"]][-51,]))-
                                                cumsum(colSums(sim.resul.org[[i]][["res_China"]][-51,]))))
      comb.sim.ci.onset<-cbind(comb.sim.ci.onset,(cumsum(colSums(sim.resul[[i]][["onset_data"]][-51,]))-
                                                    cumsum(colSums(sim.resul.org[[i]][["onset_data"]][-51,]))))
      comb.sim.ci.res.per<-cbind(comb.sim.ci.res.per,(cumsum(colSums(sim.resul[[i]][["res_China"]][-51,]))-
                                                        cumsum(colSums(sim.resul.org[[i]][["res_China"]][-51,])))/
                                   cumsum(colSums(sim.resul.org[[i]][["res_China"]][-51,])))
      comb.sim.ci.onset.per<-cbind(comb.sim.ci.onset.per,(cumsum(colSums(sim.resul[[i]][["onset_data"]][-51,]))-
                                                            cumsum(colSums(sim.resul.org[[i]][["onset_data"]][-51,])))/
                                     cumsum(colSums(sim.resul.org[[i]][["onset_data"]][-51,])))
      sim.compa <- cbind(apply(comb.sim.ci.onset, 1, mean),
                         apply(comb.sim.ci.onset, 1, max),
                         apply(comb.sim.ci.onset, 1, min),
                         apply(comb.sim.ci.onset.per, 1, mean),
                         apply(comb.sim.ci.onset.per, 1, max),
                         apply(comb.sim.ci.onset.per, 1, min))
      colnames(sim.compa) <- rep(c("mean","hi", "lo"),2)
    }
  }else{
    for(i in 1:length(sim.resul)){
      comb.sim.ci.res<-cbind(comb.sim.ci.res,(cumsum(colSums(sim.resul[[i]][["res_China"]]))-
                                                cumsum(colSums(sim.resul.org[[i]][["res_China"]]))))
      comb.sim.ci.onset<-cbind(comb.sim.ci.onset,(cumsum(colSums(sim.resul[[i]][["onset_data"]]))-
                                                    cumsum(colSums(sim.resul.org[[i]][["onset_data"]]))))
      comb.sim.ci.res.per<-cbind(comb.sim.ci.res.per,(cumsum(colSums(sim.resul[[i]][["res_China"]]))-
                                                        cumsum(colSums(sim.resul.org[[i]][["res_China"]])))/
                                   cumsum(colSums(sim.resul.org[[i]][["res_China"]])))
      comb.sim.ci.onset.per<-cbind(comb.sim.ci.onset.per,(cumsum(colSums(sim.resul[[i]][["onset_data"]]))-
                                                            cumsum(colSums(sim.resul.org[[i]][["onset_data"]])))/
                                     cumsum(colSums(sim.resul.org[[i]][["onset_data"]])))
      sim.compa <- cbind(apply(comb.sim.ci.onset, 1, mean),
                         apply(comb.sim.ci.onset, 1, max),
                         apply(comb.sim.ci.onset, 1, min),
                         apply(comb.sim.ci.onset.per, 1, mean),
                         apply(comb.sim.ci.onset.per, 1, max),
                         apply(comb.sim.ci.onset.per, 1, min))
      colnames(sim.compa) <- rep(c("mean","hi", "lo"),2)
    }
  }
  return(data.frame(sim.compa))
}

