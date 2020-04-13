library(rjags)
library(runjags)
#load the data 
setwd("~/BDA/assignment2")
Avalanches_part2<-read.csv("Avalanches_part2.csv", 
                           header = TRUE, sep=";",fileEncoding="UTF-8-BOM")

#(a)
#Transform
Avalanches_part2$Snow_total <- Avalanches_part2$Snow_total/100
Avalanches_part2$Snow_days <- Avalanches_part2$Snow_days/14
head(Avalanches_part2)

#exploratory analysis
Avalanches_part2$Death_frac <- Avalanches_part2$Deaths/Avalanches_part2$Hit

#
attach(Avalanches_part2)
par(mfrow =c(1,3))
boxplot(split(Death_frac, Season),main="Death Proportion vs Season")
boxplot(split(Snow_days, Season),main="Snow Days vs Season")
boxplot(split(Snow_total, Season),main="Snow Total vs Season")
detach(Avalanches_part2)
par(mfrow =c(1,1))

#
attach(Avalanches_part2)
boxplot(split(Snow_days,Geo_space),xlab="Geo space", ylab = "Snow days  (fortnight)")
boxplot(split(Snow_total,Geo_space),xlab="Geo space", ylab = "Snow total  (meter)")
boxplot(split(Snow_days,Rec.station),xlab="Record station", ylab = "Snow days  (fortnight)")
boxplot(split(Snow_total,Rec.station),xlab="Record station", ylab = "Snow total  (meter)")
detach(Avalanches_part2)
#(b)
#Data and inits
Avalanches2_b.data <- list(J = 3, n = 43, N = Avalanches_part2$Hit, 
                         y = Avalanches_part2$Deaths, 
                         Season = Avalanches_part2$Season,
                         Snow_total = Avalanches_part2$Snow_total,
                         Snow_days = Avalanches_part2$Snow_days,
                         Geo_space = Avalanches_part2$Geo_space) 

Avalanches2_b.inits <- function(){ 
  list(beta0 = rnorm(1, 0, 10), 
       beta_season = rnorm(1, 0, 10), 
       beta_days = rnorm(1, 0, 10),
       beta_total = rnorm(1, 0, 10),
       sigma.epsilon=runif(0, 10)) }

#model
Avalanches2_b.model <- "model{ 
#likelihood
for(i in n) 
{ y[i] ~ dbin(p[i],N[i])
logit(p[i]) <- beta0 + beta_season*(Season[i] - mean(Season[])) + 
beta_days*(Snow_days[i] - mean(Snow_days[])) + 
beta_total*(Snow_total[i] - mean(Snow_total[])) + epsilon[Geo_space[i]]
}

#### priors for epsilon per Geo_space
for(j in 1:J){ epsilon[j] ~ dnorm(0, sigma.epsilon)} 

#### priors for beta
beta0 ~dnorm(0, 0.01)
beta_season ~dnorm(0, 0.01)
beta_days ~dnorm(0, 0.01)
beta_total ~dnorm(0, 0.01)

# Hyperpriors 
sigma.epsilon ~ dunif(0, 10) 

} "

#run jags
Avalanches2_b.hier.A <- jags.model(file = textConnection(Avalanches2_b.model), 
                                   data = Avalanches2_b.data, 
                                   inits = Avalanches2_b.inits, 
                                   n.chains=3, quiet = TRUE) 

update(Avalanches2_b.hier.A, n.iter=5000) 
Avalanches2_b.hier.B <- coda.samples(Avalanches2_b.hier.A, 
                                     variable.names=c("beta0","beta_season","beta_days","beta_total","epsilon"), 
                                     n.iter=50000)

## convergence
#gelman.plot(Avalanches2_b.hier.B)
gelman.diag(Avalanches2_b.hier.B) 
summary(Avalanches2_b.hier.B)

## discuss the posterior estimates obtained
fit_b = as.data.frame(combine.mcmc(Avalanches2_b.hier.B))

mean(1/ (1 + exp(-fit_b$beta0)))
mean(fit_b$beta_days > 0)
#mean(fit_b$beta_days > 0)/mean(fit_b$beta_days <= 0)
mean(fit_b$beta_season > 0)

mean(fit_b$beta_total > 0)
mean(fit_b$`epsilon[1]` > 0)
mean(fit_b$`epsilon[2]` > 0)
mean(fit_b$`epsilon[3]` > 0)


#(c)
#Data and inits
Avalanches2_c.data <- list(J = 3, n = 43, N = Avalanches_part2$Hit, 
                           y = Avalanches_part2$Deaths, 
                           Season = Avalanches_part2$Season,
                           Snow_total = Avalanches_part2$Snow_total,
                           Geo_space = Avalanches_part2$Geo_space) 

Avalanches2_c.inits <- function(){ 
  list(beta0 = rnorm(1, 0, 10), 
       beta_season = rnorm(1, 0, 10), 
       beta_total = rnorm(1, 0, 10),
       sigma.epsilon=runif(0, 10)) }

Avalanches2_c.model <- "model{ 
#likelihood
for(i in n) 
{ y[i] ~ dbin(p[i],N[i])
logit(p[i]) <- beta0 + beta_season*(Season[i] - mean(Season[])) + 
beta_total*(Snow_total[i] - mean(Snow_total[])) + epsilon[Geo_space[i]]
}

#### priors for epsilon per Geo_space
for(j in 1:J){ epsilon[j] ~ dnorm(0, sigma.epsilon)} 

#### priors for beta
beta0 ~dnorm(0, 0.01)
beta_season ~dnorm(0, 0.01)
beta_total ~dnorm(0, 0.01)

# Hyperpriors 
sigma.epsilon ~ dunif(0, 10) 

} "

Avalanches2_c.hier.A <- jags.model(file = textConnection(Avalanches2_c.model), 
                                   data = Avalanches2_c.data, 
                                   inits = Avalanches2_c.inits, 
                                   n.chains=3, quiet = TRUE) 

update(Avalanches2_c.hier.A, n.iter=5000) 
Avalanches2_c.hier.B <- coda.samples(Avalanches2_c.hier.A, 
                                     variable.names=c("beta0","beta_season","beta_total","epsilon"), 
                                     n.iter=50000)

#
## discuss the posterior estimates obtained
fit_c = as.data.frame(combine.mcmc(Avalanches2_c.hier.B))

#mean(1/(1 + exp(-fit_c$beta0)))

mean(fit_c$beta_season>0)
mean(fit_c$beta_total > 0)
mean(fit_c$`epsilon[1]` > 0)
mean(fit_c$`epsilon[2]` > 0)
mean(fit_c$`epsilon[3]` > 0)

#(d)
ilogit <- function(x) {return(1/(1 + exp(-x)))} 

p_rec1_2015 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
  fit_c$beta_total*(7.55 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[1]`)

p_rec8_2015 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                       fit_c$beta_total*(3.28 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[2]`)

p_rec10_2015 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                       fit_c$beta_total*(2.91 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[3]`)

p_rec1_2018 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                       fit_c$beta_total*(7.42 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[1]`)

p_rec8_2018 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                       fit_c$beta_total*(6.05 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[2]`)

p_rec10_2018 <- ilogit(fit_c$beta0 + fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                       fit_c$beta_total*(4.39 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[3]`)

#posterior expected value
mean(p_rec1_2015)
mean(p_rec8_2015)
mean(p_rec10_2015)

mean(p_rec1_2018)
mean(p_rec8_2018)
mean(p_rec10_2018)

#95 credible interval
quantile(p_rec1_2015, c(0.025, 0.975))
quantile(p_rec8_2015, c(0.025, 0.975))
quantile(p_rec10_2015, c(0.025, 0.975))

quantile(p_rec1_2018, c(0.025, 0.975))
quantile(p_rec8_2018, c(0.025, 0.975))
quantile(p_rec10_2018, c(0.025, 0.975))



#>60%
z_rec1_2015 <- rbinom(n = length(p_rec1_2015), size = 100, prob = p_rec1_2015)
sum(z_rec1_2015 > 60)/length(p_rec1_2015)

z_rec8_2015 <- rbinom(n = length(p_rec8_2015), size = 100, prob = p_rec8_2015)
sum(z_rec8_2015 > 60)/length(p_rec8_2015)

z_rec10_2015 <- rbinom(n = length(p_rec10_2015), size = 100, prob = p_rec10_2015)
sum(z_rec10_2015 > 60)/length(p_rec10_2015)

z_rec1_2018 <- rbinom(n = length(p_rec1_2018), size = 100, prob = p_rec1_2018)
sum(z_rec1_2018 > 60)/length(p_rec1_2018)

z_rec8_2018 <- rbinom(n = length(p_rec8_2018), size = 100, prob = p_rec8_2018)
sum(z_rec8_2018 > 60)/length(p_rec8_2018)

z_rec10_2018 <- rbinom(n = length(p_rec10_2018), size = 100, prob = p_rec10_2018)
sum(z_rec10_2018 > 60)/length(p_rec10_2018)



#(e)
#Data and inits
Avalanches2_e.data <- list(J = 11, n = 43, N = Avalanches_part2$Hit, 
                           y = Avalanches_part2$Deaths, 
                           Season = Avalanches_part2$Season,
                           Snow_total = Avalanches_part2$Snow_total,
                           Rec.station = Avalanches_part2$Rec.station) 

Avalanches2_e.inits <- function(){ 
  list(beta0 = rnorm(1, 0, 10), 
       beta_season = rnorm(1, 0, 10), 
       beta_total = rnorm(1, 0, 10),
       sigma.epsilon=runif(0, 10)) }

Avalanches2_e.model <- "model{ 
#likelihood
for(i in n) 
{ y[i] ~ dbin(p[i],N[i])
logit(p[i]) <- beta0 + beta_season*(Season[i] - mean(Season[])) + 
beta_total*(Snow_total[i] - mean(Snow_total[])) + epsilon[Rec.station[i]]
}

#### priors for epsilon per Geo_space
for(j in 1:J){ epsilon[j] ~ dnorm(0, sigma.epsilon)} 

#### priors for beta
beta0 ~dnorm(0, 0.01)
beta_season ~dnorm(0, 0.01)
beta_total ~dnorm(0, 0.01)


# Hyperpriors 
sigma.epsilon ~ dunif(0, 10) 

} "

Avalanches2_e.hier.A <- jags.model(file = textConnection(Avalanches2_e.model), 
                                   data = Avalanches2_e.data, 
                                   inits = Avalanches2_e.inits, 
                                   n.chains=3, quiet = TRUE) 

update(Avalanches2_e.hier.A, n.iter=5000) 
Avalanches2_e.hier.B <- coda.samples(Avalanches2_e.hier.A, 
                                     variable.names=c("beta0","beta_season","beta_total","epsilon"), 
                                     n.iter=50000)


##DIC
DIC.e <- dic.samples(model = Avalanches2_e.hier.A, n.iter = 20000, type="pD")
DIC.c <- dic.samples(model = Avalanches2_c.hier.A, n.iter = 20000, type = "pD")
diffdic(DIC.e,DIC.c)


##poserior expected value
fit_e <- as.data.frame(combine.mcmc(Avalanches2_e.hier.B))

p_rec1_2015e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                        fit_e$beta_total*(7.55 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[1]`)

p_rec8_2015e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                        fit_e$beta_total*(3.28 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[8]`)

p_rec10_2015e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta_total*(2.91 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[10]`)

p_rec1_2018e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                        fit_e$beta_total*(7.42 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[1]`)

p_rec8_2018e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                        fit_e$beta_total*(6.05 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[8]`)

p_rec10_2018e <- ilogit(fit_e$beta0 + fit_e$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta_total*(4.39 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`epsilon[10]`)

#posterior expected value
mean(p_rec1_2015e)
mean(p_rec8_2015e)
mean(p_rec10_2015e)

mean(p_rec1_2018e)
mean(p_rec8_2018e)
mean(p_rec10_2018e)

#95 credible interval
quantile(p_rec1_2015e, c(0.025, 0.975))
quantile(p_rec8_2015e, c(0.025, 0.975))
quantile(p_rec10_2015e, c(0.025, 0.975))

quantile(p_rec1_2018e, c(0.025, 0.975))
quantile(p_rec8_2018e, c(0.025, 0.975))
quantile(p_rec10_2018e, c(0.025, 0.975))

