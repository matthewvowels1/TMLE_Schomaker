# install.packages('igraph')
# install.packages('stringr')
# install.packages('assertthat')
# install.packages('R6')
# install.packages('data.table')
# install.packages('https://cran.r-project.org/src/contrib/Archive/simcausal/simcausal_0.5.5.tar.gz', repos=NULL, type='source')
# 
# library(igraph)
# library(stringr)
# library(assertthat)
# library(R6)
# library(data.table)
# library(simcausal)

rnorm_trunc <- function(n, mean, sd, minval = 0, maxval = 10000) {
  out <- rnorm(n = n, mean = mean, sd = sd)
  minval <- minval[1]
  maxval <- maxval[1]
  out[out <= minval] <- minval
  out[out >= maxval] <- maxval
  out
}

custom_runif <- function(n, var, min_low, max_low, min_high, max_high, thresh_low, thresh_high) {
  ifelse(var == thresh_low | var == thresh_high,
         out <- runif(n=n, min=ifelse(var==thresh_low, min_low, min_high), max=ifelse(var==thresh_low, max_low, max_high)),
         out <- rnorm(n=n, mean=var, sd=0))
  out
}

D <- DAG.empty() + 
  node("V1", t=0, distr="rbern", prob= (4392/5826)) +
  node("V2", t=0,  distr="rbern", prob = V1[t] * (2222/4392) + (1 - V1[t]) * (758/1434)) +
  node("V3", t=0, distr="runif", min=1, max=5) +
  node("L1.1st", t=0, distr = "rnorm_trunc", mean=V1[t]*650 + (1-V1[t])*720, sd=V1[t]*350 + (1-V1[t])*400, minval=0, maxval=10000)+
  node("L1.2nd", t=0, distr = "custom_runif", var=L1.1st[t], min_low=0, max_low=50, min_high=5000, max_high=10000, thresh_low=0, thresh_high=10000)+
  node("L1tilde", t=0, distr="rnorm", mean= (L1.2nd[t] - 671.7468)/(10*352.2788)+1, sd=0)+
  node("L2.1st", t=0, distr="rnorm_trunc", mean=0.16 + 0.05*(L1.2nd[t] - 650)/650, sd=0.07, minval=0.06, maxval=0.8)+
  node("L2.2nd", t=0, distr="custom_runif", var=L2.1st[t], min_low=0.03, max_low=0.09, min_high=0.7, max_high=0.8, thresh_low=0.06, thresh_high=0.8)+
  node("L2tilde", t=0, distr="rnorm", mean=((L2.2nd[t] - 0.1648594)/10*0.06980332)+1, sd=0)+
  node("L3.1st", t=0, distr="rnorm_trunc", mean= ifelse(V1[t]==1, -1.65 + 0.1*V3[t] + 0.05*(L1.2nd[t]-650)/650 + 0.05*(L2.2nd[t] - 16)/16,  -2.05 + 0.1*V3[t] + 0.05*(L1.2nd[t]-650)/650 + 0.05*(L2.2nd[t] - 16)/16), sd=1, minval=-5, maxval=5)+
  node("L3.2nd", t=0, distr="custom_runif", var=L3.1st[t], min_low=-10, max_low=3, min_high=3, max_high=10, thresh_low=-5, thresh_high=5)+
  node("A", t=0, distr="rbern", prob=0)+
  node("C", t=0, distr="rbern", prob=0)+
  node("Y.1st", t=0, distr="rnorm_trunc", mean=-2.6 + 0.1*(as.numeric(V3[t]>2)) + 0.3*(as.numeric(V1[t]==0)) + (L3.2nd[t]+1.45), sd=1.1, minval=-5, maxval=5)+
  node("Y.2nd", t=0, distr="custom_runif", var=Y.1st[t], min_low=-10, max_low=3, min_high=3, max_high=10, thresh_low=-5, thresh_high=5)+
  node("L1.1st", t=1:12, distr="rnorm_trunc", mean=ifelse(t>0 & t<5, 13*log(t*(1034-662)/8) +L1.2nd[t-1]+2*L2.2nd[t-1]+2*L3.2nd[t-1]+2.5*A[t-1], ifelse(
    t>4 & t<9, 4*log(t*(1034-662)/8)+L1.2nd[t-1]+2*L2.2nd[t-1]+2*L3.2nd[t-1]+2.5*A[t-1], ifelse(
      t>8, L1.2nd[t-1]+2*L2.2nd[t-1]+2*L3.2nd[t-1]+2.5*A[t-1] ,999))), sd=50, minval=0, maxval=10000)+
  node("L1.2nd", t=1:12, distr = "custom_runif", var=L1.1st[t], min_low=0, max_low=50, min_high=5000, max_high=10000, thresh_low=0, thresh_high=10000)+  
  node("L2.1st", t=1:12, distr="rnorm_trunc", mean=L2.2nd[t-1] + 0.0003*(L1.2nd[t] - L1.2nd[t-1])+0.0005*L3.2nd[t-1]+0.0005*A[t-1]*L1tilde[0], sd=0.02, minval=0.06, maxval=0.8)+
  node("L2.2nd", t=1:12, distr="custom_runif", var=L2.1st[t], min_low=0.03, max_low=0.09, min_high=0.7, max_high=0.8, thresh_low=0.06, thresh_high=0.8)+
  node("L3.1st", t=1:12, distr="rnorm_trunc", mean=L3.2nd[t-1] + 0.0017*(L1.2nd[t] - L1.2nd[t-1])+0.2*(L2.2nd[t]-L2.2nd[t-1])+0.005*A[t-1]*L2tilde[0], sd=0.5, minval=-5, maxval=5)+
  node("L3.2nd", t=1:12, distr="custom_runif", var=L3.1st[t], min_low=-10, max_low=3, min_high=3, max_high=10, thresh_low=-5, thresh_high=5)+
  node("A", t=1:12, distr="rbern", prob=A[t-1]*1 + (1-A[t-1])*(1/(1+exp(-(-2.4+0.015*(750-L1.2nd[t])+5*(0.2-L2.2nd[t])-0.8*L3.2nd[t]+0.8*t)))))+
  node("C", t=1:12, distr="rbern", prob=1/(1+exp(-(-6+0.01*(750-L1.2nd[t])+(0.2-L2.2nd[t])-0.65*L3.2nd[t]-A[t])))) +
  node("Y.1st", t=1:12, distr="rnorm_trunc", mean=Y.2nd[t-1] + 0.00005*(L1.2nd[t]-L1.2nd[t-1]) - 0.000001*((L1.2nd[t]-L1.2nd[t-1])*sqrt(L1tilde[0]))^2 + 0.01*(L2.2nd[t]-L2.2nd[t-1]) -
         0.0001*((L2.2nd[t]-L2.2nd[t-1])*sqrt(L2tilde[0]))^2 + 0.07*((L3.2nd[t]-L3.2nd[t-1])*(L3.2nd[0]+1.5135))-0.001*((L3.2nd[t]-L3.2nd[t-1])*(L3.2nd[0]+1.5135))^2 +
         0.005*A[t] + 0.075*A[t-1] + 0.05*A[t]*A[t-1], sd=0.01, minval=-5, maxval=5)+
  node("Y.2nd", t=1:12, distr="custom_runif", var=Y.1st[t], min_low=-10, max_low=3, min_high=3, max_high=10, thresh_low=-5, thresh_high=5)
D <- set.DAG(D)

D = D + action("A1", nodes=node("A", t=0:12, distr="rbern", prob=1))
D = D + action("A0", nodes=node("A", t=0:12, distr="rbern", prob=0))
plotDAG(D)
Odat = sim(DAG=D, n=500000, rndseed=123)
Odatint = sim(DAG=D, actions=c("A1","A0"), n=1000, rndseed=123)
write.csv(Odat, '/media/matthewvowels/Storage_SSD/simcausal/obs_data.csv')
write.csv(Odatint[1], '/media/matthewvowels/Storage_SSD/simcausal/counterfactual_data_A1.csv')
write.csv(Odatint[2], '/media/matthewvowels/Storage_SSD/simcausal/counterfactual_data_A0.csv')
