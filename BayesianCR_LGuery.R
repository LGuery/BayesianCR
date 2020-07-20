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


## ---- eval=FALSE---------------------------------------------------------------
#> model{
#>   p ~ dunif(0,1)
#>   for (i in 1:N){
#>     y[i] ~ dbin(p,K)
#>   }
#> }


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
library(scrbook)
data(beardata)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
cat("
model {
psi ~ dunif(0, 1) # DA parameter
p ~ dunif(0,1) # prior distribution
for (i in 1:M){
   z[i] ~ dbern(psi) # DA latent variables
   for(k in 1:K){
     tmp[i,k] <- p*z[i]
     y[i,k] ~ dbin(tmp[i,k],1)
      }
     }
N<-sum(z[1:M])
}
",file="modelM0.txt")


## ------------------------------------------------------------------------------
M = 175 # number of all individuals (encountered and DA)
nind <- dim(beardata$bearArray)[1] # number of encounter histories, i.e. encountered individuals
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

# Because traditional CR models ignore space
# create a 2-d matrix "individuals x occasions" 
# of 0/1 data where 1 = "captured" 0 = "not captured"
y <- apply(Yaug,c(1,3),sum) # summarize by ind * occ
y[y>1] <- 1                 # make sure that multiple encounters do not occur


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
set.seed(2013)
data <- list(y=y,M=M,K=K)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
params <- c("psi","p","N")


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
zst = c(rep(1,nind),rbinom(M-nind, 1, .5))
inits =  function(){list(z=zst, psi=runif(1), p=runif(1))}


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
#> # Package rjags
#> library(rjags)
#> jm <- jags.model("modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
#> fit0j <- coda.samples(jm, params, n.iter=1000)
#> 
#> # Package jagsUI
#> library(jagsUI)
#> fit0j = jags(data, inits, params, model.file="modelM0.txt",n.chains=3,
#>               n.iter=2000, n.burnin=1000, n.thin=1)


## ---- eval=TRUE, echo=FALSE, include=FALSE-------------------------------------
library(rjags)
jm <- jags.model("modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
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
#> ",file="modelM0.txt")
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
#> jm <- jags.model("modelM0.txt", data=data, inits=inits, n.chains=3, n.adapt=1000)
#> fit0j <- coda.samples(jm, params, n.iter=1000)
#> summary(fit0j)
#> plot(fit0j[,"N"])


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_M0_50.png')


## ----  out.width = '70%', fig.align = 'center', echo = FALSE-------------------
knitr::include_graphics('img/summary_M0_400.png')


## ---- eval=FALSE---------------------------------------------------------------
#> knitr::purl('BayesianCR_LGuery.Rmd')

