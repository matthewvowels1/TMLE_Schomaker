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
  out[out < minval] <- minval
  out[out > maxval] <- maxval
  out
  }

D <- DAG.empty() + 
  node("V1", t=0, distr="rbern", prob= (4392/5826)) +
  node("V2", t=0,  distr="rbern", prob = V1[t] * (2222/4392) + (1 - V1[t]) * (758/1434)) +
  node("V3", t=0, distr="runif", min=1, max=5) +
  node("L1", t=0, distr = "rnorm_trunc", mean=V1[t]*650 + (1-V1[t])*720, sd=V1[t]*350 + (1-V1[t])*400, minval=0, maxval=10000)+
  node("L1tilde", t=0, distr="rnorm", mean= (L1[t] - 671.7468)/(10*352.2788)+1, sd=0)+
  node("L2", t=0, distr="rnorm_trunc", mean=0.16 + 0.05*(L1[t] - 650)/650, sd=0.07, minval=0.06, maxval=0.8)+
  node("L2tilde", t=0, distr="rnorm", mean=((L2[t] - 0.1648594)/10*0.06980332)+1, sd=0)+
  node("L3", t=0, distr="rnorm_trunc", mean= ifelse(V1[t]==1, -1.65 + 0.1*V3[t] + 0.05*(L1[t]-650)/650 + 0.05*(L2[t] - 16)/16,  -2.05 + 0.1*V3[t] + 0.05*(L1[t]-650)/650 + 0.05*(L2[t] - 16)/16), sd=1, minval=-5, maxval=5)+
  node("A", t=0, distr="rbern", prob=0)+
  node("C", t=0, distr="rbern", prob=0)+
  node("Y", t=0, distr="rnorm_trunc", mean=-2.6 + 0.1*(as.numeric(V3[t]>2)) + 0.3*(as.numeric(V1[t]==0)) + (L3[t]+1.45), sd=1.1, minval=-5, maxval=5)+
  node("L1", t=1:12, distr="rnorm_trunc", mean=ifelse(t>0 & t<5, 13*log(t*(1034-662)/8) +L1[t-1]+2*L2[t-1]+2*L3[t-1]+2.5*A[t-1], ifelse(
    t>4 & t<9, 4*log(t*(1034-662)/8)+L1[t-1]+2*L2[t-1]+2*L3[t-1]+2.5*A[t-1], ifelse(
      t>8, L1[t-1]+2*L2[t-1]+2*L3[t-1]+2.5*A[t-1] ,999))), sd=50, minval=0, maxval=10000)+
  node("L2", t=1:12, distr="rnorm_trunc", mean=L2[t-1] + 0.0003*(L1[t] - L1[t-1])+0.0005*L3[t-1]+0.0005*A[t-1]*L1tilde[0], sd=0.02, minval=0.06, maxval=0.8)+
  node("L3", t=1:12, distr="rnorm_trunc", mean=L3[t-1] + 0.0017*(L1[t] - L1[t-1])+0.2*(L2[t]-L2[t-1])+0.005*A[t-1]*L2tilde[0], sd=0.5, minval=-5, maxval=5)+
  node("A", t=1:12, distr="rbern", prob=A[t-1]*1 + (1-A[t-1])*(1/(1+exp(-(-2.4+0.015*(750-L1[t])+5*(0.2-L2[t])-0.8*L3[t]+0.8*t)))))+
  node("C", t=1:12, distr="rbern", prob=1/(1+exp(-(-6+0.01*(750-L1[t])+(0.2-L2[t])-0.65*L3[t]-A[t])))) +
  node("Y", t=1:12, distr="rnorm_trunc", mean=Y[t-1] + 0.00005*(L1[t]-L1[t-1]) - 0.000001*((L1[t]-L1[t-1])*sqrt(L1tilde[0]))^2 + 0.01*(L2[t]-L2[t-1]) -
         0.0001*((L2[t]-L2[t-1])*sqrt(L2tilde[0]))^2 + 0.07*((L3[t]-L3[t-1])*(L3[0]+1.5135))-0.001*((L3[t]-L3[t-1])*(L3[0]+1.5135))^2 +
         0.005*A[t] + 0.075*A[t-1] + 0.05*A[t]*A[t-1], sd=0.01, minval=-5, maxval=5)
D <- set.DAG(D)
plotDAG(D)

Odat = sim(DAG=D, n=1000, rndseed=123)