## ----setup, include = FALSE----------------------------------------------------
knitr::opts_chunk$set(cache = TRUE, 
                      echo = TRUE, 
                      message = FALSE, 
                      warning = FALSE,
                      fig.height=6, 
                      fig.width = 1.777777*6,
                      tidy = FALSE, 
                      comment = NA, 
                      highlight = TRUE, 
                      prompt = FALSE, 
                      crop = TRUE,
                      comment = "#>",
                      collapse = TRUE)
knitr::opts_knit$set(width = 60)
library(tidyverse)
library(reshape2)
theme_set(theme_light(base_size = 16))
make_latex_decorator <- function(output, otherwise) {
  function() {
      if (knitr:::is_latex_output()) output else otherwise
  }
}
insert_pause <- make_latex_decorator(". . .", "\n")
insert_slide_break <- make_latex_decorator("----", "\n")
insert_inc_bullet <- make_latex_decorator("> *", "*")
insert_html_math <- make_latex_decorator("", "$$")


## ----  out.width = '35%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/BPop_Winbugs.jpg')


## ----  out.width = '90%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/SCR_book.png')


## ----  out.width = '65%', fig.align='center',echo=FALSE------------------------
knitr::include_graphics('img/EncounterHistory.png')


## ------------------------------------------------------------------------------

# Simulating Bernoulli trials
# Simulate random encounter events with p = 0.25 for an individual
# Outcome y = 1 means "captured" and y=0 means "not captured"
p <- 0.25
K <- 4 # sample occasions
# one encounter history
set.seed(1987)
rbinom(n=K, size=1, prob=p)


## ---- eval=FALSE---------------------------------------------------------------
#> model{
#>   p ~ dunif(0,1)
#>   for (i in 1:N){
#>     y[i] ~ dbin(p,K)
#>   }
#> }


## ----  out.width = '65%', fig.align='center',echo=FALSE------------------------
knitr::include_graphics('img/EncounterHistory.png')


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
library(scrbook)
data(beardata)


## ----  out.width = '30%', fig.align='center',echo=FALSE------------------------
knitr::include_graphics('img/Fortdrumarea.png')


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
#> cat("
#> model {
#> psi ~ dunif(0, 1)
#> p ~ dunif(0,1)
#> for (i in 1:M){
#>    z[i] ~ dbern(psi)
#> 
#>    for(k in 1:K){
#>      tmp[i,k] <- p*z[i]
#>      y[i,k] ~ dbin(tmp[i,k],1)
#>       }
#>      }
#> N <- sum(z[1:M])
#> }
#> ",file="code/modelM0.txt")


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
cat("
model {
psi ~ dunif(0, 1) # DA parameter
p ~ dunif(0,1) # prior distribution
for (i in 1:M){
   z[i] ~ dbern(psi) # binary DA latent variables which indicates if individual i is
                     # a member of the population - Abundance is just the sum of 
                     # these binary latent variables
   for(k in 1:K){
     tmp[i,k] <- p*z[i]
     y[i,k] ~ dbin(tmp[i,k],1) # likelihood
      }
     }
N <- sum(z[1:M])
}
",file="code/modelM0.txt")


## ------------------------------------------------------------------------------
M = 175 # number of all individuals (encountered and DA)
nind <- dim(beardata$bearArray)[1] # number of encounter histories (individuals)
ntraps <- dim(beardata$bearArray)[2] # number of traps
K <- dim(beardata$bearArray)[3] # number of occasions

# How many "all zero" encounter histories are there?
nz <- M-nind

nz


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# Fill up an array with zeros
Yaug <- array(0, dim=c(M,ntraps,K))

# Store the real data into the first nind slots
Yaug[1:nind,,] <- beardata$bearArray

# Because traditional CR models ignore space create a 2-d matrix 
# "individuals x occasions"  of 0/1 data where 1 = "captured" 0 = "not captured"

y <- apply(Yaug,c(1,3),sum) # summarize by ind * occ
y[y>1] <- 1                 # make sure that multiple encounters do not occur


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
set.seed(2020)
data <- list(y=y,M=M,K=K)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
params <- c("psi","p","N")


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
zst = c(rep(1,nind),rbinom(M-nind, 1, .5))
inits =  function(){list(z=zst, psi=runif(1), p=runif(1))}


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
#> # Package rjags
#> library(rjags)
#> jm <- jags.model("code/modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
#> fit0j <- coda.samples(jm, params, n.iter=1000)
#> 
#> # Package jagsUI
#> library(jagsUI)
#> fit0j = jags(data, inits, params, model.file="code/modelM0.txt",n.chains=3,
#>               n.iter=2000, n.burnin=1000, n.thin=1)


## ---- eval=TRUE, echo=FALSE, include=FALSE-------------------------------------
library(rjags)
jm <- jags.model("code/modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
fit0j <- coda.samples(jm, params, n.iter=1000)


## ---- eval=FALSE, echo=FALSE---------------------------------------------------
#> summary(fit0j)


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_M0.png')


## ---- eval=TRUE, echo=FALSE----------------------------------------------------
plot(fit0j[,c("N","psi")])


## ---- eval=FALSE, echo=FALSE---------------------------------------------------
#> library(scrbook)
#> data(beardata)
#> cat("
#> model {
#> psi ~ dunif(0, 1)
#> p ~ dunif(0,1)
#> for (i in 1:M){
#>    z[i] ~ dbern(psi)
#>    for(k in 1:K){
#>      tmp[i,k] <- p*z[i]
#>      y[i,k] ~ dbin(tmp[i,k],1)
#>       }
#>      }
#> N<-sum(z[1:M])
#> }
#> ",file="code/modelM0.txt")
#> M = 400
#> nind <- dim(beardata$bearArray)[1]
#> ntraps <- dim(beardata$bearArray)[2]
#> K <- dim(beardata$bearArray)[3]
#> Yaug <- array(0, dim=c(M,ntraps,K))
#> Yaug[1:nind,,] <- beardata$bearArray
#> y <- apply(Yaug,c(1,3),sum)
#> y[y>1] <- 1
#> set.seed(2013)
#> data <- list(y=y,M=M,K=K)
#> params <- c("psi","p","N")
#> zst = c(rep(1,nind),rbinom(M-nind, 1, .5))
#> inits =  function(){list(z=zst, psi=runif(1), p=runif(1))}
#> jm <- jags.model("code/modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
#> fit0j <- coda.samples(jm, params, n.iter=1000)
#> summary(fit0j)
#> plot(fit0j[,"N"])


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_M0_50.png')


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_M0_400.png')


## ------------------------------------------------------------------------------
M0.lik <-function(parameters){
  p <- plogis(parameters[1])
  n0 <- exp(parameters[2])
  N <- n + n0
  loglik.part1 <- lgamma(N+1) - lgamma(n0+1)
  loglik.part2 <- matrix(NA, n, K)
  for(i in 1:n){
    for(k in 1:K){
      loglik.part2[i,k] <- sum( Y[i,k]*log(p) + (1-Y[i,k])*log(1-p) )
    }
  }
  loglik.part3 <- n0 * sum(rep(log(1-p),K))
  -1 * (loglik.part1 + sum(loglik.part2) + loglik.part3)
  }


## ------------------------------------------------------------------------------
# Set up the data

Y <- apply(beardata$bearArray,c(1,3),max)
n <- nrow(Y)
K <- ncol(Y)

# fit the model
fm.M0 <- nlm(M0.lik,rep(0,2),hessian=TRUE)

# estimated p
(phat <- plogis(fm.M0$est[1]))

# estimated N
(Nhat <- n+exp(fm.M0$est[2]))

# SE of Nhat
Nhat_SE <- sqrt((exp(fm.M0$est[2])^2)*solve(fm.M0$hess)[2,2])

# confidence intervals for Nhat
(Nhat + c(-1.96, 1.96)*Nhat_SE)


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/Pics_SECR.png')


## ----  out.width = '80%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/StateProcess.png')


## ----  out.width = '80%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/ObsProcess.png')


## ---- eval=TRUE,echo=FALSE-----------------------------------------------------
plot(function(d) 0.7*exp(-d^2/(2*20^2)), cex.lab=1.3,
     from=0, to=100, ylim=c(0,1), main="",
     xlab="Distance (m) between activity center and trap",
     ylab="Detection probability", lwd=2)
plot(function(d) 0.7*exp(-d^2/(2*30^2)), 0, 100, add=TRUE, col="blue",
     lwd=2)
plot(function(d) 0.7*exp(-d^2/(2*40^2)), 0, 100, add=TRUE, col="orange",
     lwd=2)
legend(50, 1, c("p0=0.7, sigma=20", "p0=0.7, sigma=30", "p0=0.7, sigma=40"),
       lty=1, lwd=2, col=c("black", "blue", "orange"))


## ----  out.width = '50%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/EDF.png')


## ----  out.width = '50%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/TDF.png')


## ---- eval=TRUE, echo=FALSE----------------------------------------------------
encounters <- read.csv("dat/encounterData.csv")
encounters[1:4,] # First 4 rows of data


## ---- eval=TRUE, echo=FALSE----------------------------------------------------
traps <- read.csv("dat/trapData.csv", row.names=1)
traps[1:4,] # First 4 rows of data


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
nocapTraps <- setdiff(rownames(traps), encounters$trapID)
nocapTraps
levels(encounters$trapID) <- c(levels(encounters$trapID), nocapTraps)
levels(encounters$trapID) # All trapIDs should be here now

y3D <- table(encounters$animalID, encounters$trapID,
             encounters$occasion)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
y3D[1:4,1:10,1] ## Data on first 4 ind at first 10 trap on k=1

all(rownames(traps)==colnames(y3D))  ## Not good
y3D <- y3D[,rownames(traps),]        ## Re-order
all(rownames(traps)==colnames(y3D))  ## Good


## ---- eval=TRUE, echo=FALSE----------------------------------------------------
cat("
model {
p0 ~ dunif(0, 1) # baseline encounter probability
sigma ~ dunif(0, 2) # scale parameter of encounter function
psi ~ dunif(0, 1) # DA parameter: E(N) = M*psi

for(i in 1:M) {
z[i] ~ dbern(psi) # Is individual real?
s[i,1] ~ dunif(xlim[1], xlim[2]) # x-coordinate of activity center
s[i,2] ~ dunif(ylim[1], ylim[2]) # y-coordinate

for(j in 1:J) {
# dist between activity center and trap
d[i,j] <- sqrt((s[i,1] - x[j,1])^2 + (s[i,2] - x[j,2])^2)
p[i,j] <- p0*exp(-d[i,j]^2/(2*sigma^2)) # capture prob at trap j

for(k in 1:K) {
y[i,j,k] ~ dbern(p[i,j]*z[i]) # model for data
}
}
}
N <- sum(z) # realized abundance
EN <- M*psi # expected abundance
A <- (xlim[2]-xlim[1])*(ylim[2]-ylim[1]) # area of state-space
D <- N/A # realized density
ED <- EN/A # expected density
}
",file="code/SECR0.txt")


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
#> cat("
#> model {
#> p0 ~ dunif(0, 1) # baseline encounter probability
#> sigma ~ dunif(0, 2) # scale parameter of encounter function
#> psi ~ dunif(0, 1) # DA parameter: E(N) = M*psi
#> 
#> for(i in 1:M) {
#> z[i] ~ dbern(psi) # Is individual real?
#> s[i,1] ~ dunif(xlim[1], xlim[2]) # x-coordinate of activity center
#> s[i,2] ~ dunif(ylim[1], ylim[2]) # y-coordinate
#> 
#> for(j in 1:J) {
#> # dist between activity center and trap
#> d[i,j] <- sqrt((s[i,1] - x[j,1])^2 + (s[i,2] - x[j,2])^2)
#> p[i,j] <- p0*exp(-d[i,j]^2/(2*sigma^2)) # capture prob at trap j
#> 
#> ...
#> 
#> ",file="code/SECR0.txt")


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
#> cat("
#> ...
#> 
#> for(k in 1:K) {
#> y[i,j,k] ~ dbern(p[i,j]*z[i]) # model for data
#> }
#> }
#> }
#> 
#> N <- sum(z) # realized abundance
#> EN <- M*psi # expected abundance
#> A <- (xlim[2]-xlim[1])*(ylim[2]-ylim[1]) # area of state-space
#> D <- N/A # realized density
#> ED <- EN/A # expected density
#> }
#> ",file="code/SECR0.txt")


## ------------------------------------------------------------------------------
M <- 50 # number of all individuals (encountered and DA)
J <- dim(y3D)[2] # number of traps
K <- dim(y3D)[3] # number of occasions
n0 <- nrow(y3D) # number of encounter histories, i.e. encountered individuals


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# Fill up an array with zeros
yz <- array(0, c(M, J, K))

# Store the real data into the first nind slots
yz[1:n0,,] <- y3D


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
set.seed(2020)
jd <- list(y=yz, J=J, K=K, M=M, x=traps, xlim=c(0,1), ylim=c(0,1))


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
jp <- c("N", "p0", "sigma")


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
ji <- function() list(z=rep(1,M), p0=runif(1), sigma=runif(1))


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# Package rjags
library(rjags)
jm <- jags.model("code/SECR0.txt", jd, ji, n.chains=1, n.adapt=1000)
jc <- coda.samples(jm, jp, 1000)


## ---- eval=FALSE, echo=FALSE---------------------------------------------------
#> summary(jc)


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_SECR0.png')


## ---- eval=TRUE, echo=FALSE----------------------------------------------------
plot(jc)


## ----  out.width = '40%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/dFAD.jpg')


## ----  out.width = '80%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/datPS.png')


## ----  out.width = '40%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/VMS.png')


## ----  out.width = '80%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/dFADdensity.png')


## ---- eval=FALSE, echo=FALSE---------------------------------------------------
#> knitr::purl('BayesianCR_LGuery.Rmd')

