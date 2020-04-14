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

##(b)
# data block # includes hyperparameters

avalanches.hier.data.b <- list(n = nrow(Avalanches_part2), 
                               avalanches = Avalanches_part2$Deaths, 
                               hit = Avalanches_part2$Hit,
                               season =  Avalanches_part2$Season, 
                               total = Avalanches_part2$Snow_total, 
                               days = Avalanches_part2$Snow_days, 
                               j.geo = Avalanches_part2$Geo_space, 
                               J = max(Avalanches_part2$Geo_space), 
                               sigma.ub = 10, sigma.alpha.ub = 10, tau.mu.alpha = 0.1, tau.beta = 0.1)


# Initial values

avalanches.hier.inits.b <- function() {
  list( mu.alpha = rnorm(1,0,1/sqrt(0.1)),
        beta1 = rnorm(1,0,1/sqrt(0.1)),
        beta2 = rnorm(1,0,1/sqrt(0.1)),
        beta3 = rnorm(1,0,1/sqrt(0.1)),
        sigma.alpha = runif(1,0,10)
  )
}

# Model
avalanches.hier.model.b <- "model {
    ### likelihood
    for(i in 1:n) {
      avalanches[i] ~ dbin(mu[i],hit[i])
      logit(mu[i]) <- alpha[j.geo[i]] + beta1*(season[i]-mean(season[])) + beta2*(total[i]-mean(total[])) + beta3*(days[i]-mean(days[]))
    }
      
      
    #### priors for independent intercepts per geo space
    for(j in 1:J){ 
      alpha[j] ~ dnorm(mu.alpha, tau.alpha) 
    } 
    
    #### Hyperpriors for mu.alpha and tau.alpha
    mu.alpha ~ dnorm(0, tau.mu.alpha) 
    tau.alpha <- 1/pow(sigma.alpha,2) 
    sigma.alpha ~ dunif(0, sigma.alpha.ub)
    
    #### priors for slope
    
    beta1 ~ dnorm(0,tau.beta)
    beta2 ~ dnorm(0,tau.beta)
    beta3 ~ dnorm(0,tau.beta)
}"



avalanches.hier.res.b.A <- jags.model(file=textConnection(avalanches.hier.model.b),
                                      data=avalanches.hier.data.b,inits=avalanches.hier.inits.b, n.chains=3, quiet = TRUE)
update(avalanches.hier.res.A, n.iter= 10000) 
avalanches.hier.res.b.B <- coda.samples(avalanches.hier.res.b.A,
                                        variable.names=c("alpha","mu.alpha","sigma.alpha","beta1","beta2","beta3"), 
                                        n.iter=100000)


# summary
summary(avalanches.hier.res.b.B)

## convergence
#gelman.plot(avalanches.hier.res.b.B)
gelman.diag(avalanches.hier.res.b.B) 

## discuss the posterior estimates obtained
fit_b = as.data.frame(combine.mcmc(avalanches.hier.res.b.B))

#1season 2total 3day
mean(fit_b$beta3 > 0)
#mean(fit_b$beta3 > 0)/mean(fit_b$beta3 <= 0)
mean(fit_b$beta2 > 0)
mean(fit_b$beta1 > 0)

mean(fit_b$`alpha[1]` > 0)
mean(fit_b$`alpha[2]` > 0)
mean(fit_b$`alpha[3]` > 0)

#(c)
avalanches.hier.data.c <- list(n = nrow(Avalanches_part2), 
                               avalanches = Avalanches_part2$Deaths, 
                               hit = Avalanches_part2$Hit,
                               season =  Avalanches_part2$Season, 
                               total = Avalanches_part2$Snow_total, 
                               j.geo = Avalanches_part2$Geo_space, 
                               J = max(Avalanches_part2$Geo_space), 
                               sigma.ub = 10, sigma.alpha.ub = 10, tau.mu.alpha = 0.1, tau.beta = 0.1)


# Initial values

avalanches.hier.inits.c <- function() {
  list( mu.alpha = rnorm(1,0,1/sqrt(0.1)),
        beta1 = rnorm(1,0,1/sqrt(0.1)),
        beta2 = rnorm(1,0,1/sqrt(0.1)),
        sigma.alpha = runif(1,0,10)
  )
}

# Model
avalanches.hier.model.c <- "model {
    ### likelihood
    for(i in 1:n) {
      avalanches[i] ~ dbin(mu[i],hit[i])
      logit(mu[i]) <- alpha[j.geo[i]] + beta1*(season[i]-mean(season[])) + beta2*(total[i]-mean(total[]))
    }
      
      
    #### priors for independent intercepts per geo space
    for(j in 1:J){ 
      alpha[j] ~ dnorm(mu.alpha, tau.alpha) 
    } 
    
    #### Hyperpriors for mu.alpha and tau.alpha
    mu.alpha ~ dnorm(0, tau.mu.alpha) 
    tau.alpha <- 1/pow(sigma.alpha,2) 
    sigma.alpha ~ dunif(0, sigma.alpha.ub)
    
    #### priors for slope
    
    beta1 ~ dnorm(0,tau.beta)
    beta2 ~ dnorm(0,tau.beta)
}"


avalanches.hier.res.c.A <- jags.model(file=textConnection(avalanches.hier.model.c),
                                      data=avalanches.hier.data.c,
                                      inits=avalanches.hier.inits.c, n.chains=3, quiet = TRUE)
update(avalanches.hier.res.c.A, n.iter= 10000) 
avalanches.hier.res.c.B <- coda.samples(avalanches.hier.res.c.A,
                                        variable.names=c("alpha","mu.alpha","sigma.alpha","beta1","beta2"), 
                                        n.iter=100000)


# summary
summary(avalanches.hier.res.c.B)

## convergence
#gelman.plot(avalanches.hier.res.c.B)
gelman.diag(avalanches.hier.res.c.B) 

## discuss the posterior estimates obtained
fit_c = as.data.frame(combine.mcmc(avalanches.hier.res.c.B))

#1season 2total

mean(fit_c$beta2 > 0)
mean(fit_c$beta1 > 0)

mean(fit_c$`alpha[1]` > 0)
mean(fit_c$`alpha[2]` > 0)
mean(fit_c$`alpha[3]` > 0)


#(d)
colnames(fit_c)[1:5] <- c("epsilon[1]", "epsilon[2]", "epsilon[3]", "beta_season", "beta_total")
ilogit <- function(x) {return(1/(1 + exp(-x)))} 

p_rec1_2015 <- ilogit(fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                        fit_c$beta_total*(7.55 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[1]`)

p_rec8_2015 <- ilogit(fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                        fit_c$beta_total*(3.28 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[2]`)

p_rec10_2015 <- ilogit(fit_c$beta_season*(2015 - mean(Avalanches_part2[,"Season"])) + 
                         fit_c$beta_total*(2.91 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[3]`)

p_rec1_2018 <- ilogit(fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                        fit_c$beta_total*(7.42 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[1]`)

p_rec8_2018 <- ilogit(fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
                        fit_c$beta_total*(6.05 -  mean(Avalanches_part2[,"Snow_total"])) + fit_c$`epsilon[2]`)

p_rec10_2018 <- ilogit(fit_c$beta_season*(2018 - mean(Avalanches_part2[,"Season"])) + 
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
avalanches.hier.data.e <- list(n = nrow(Avalanches_part2), 
                               avalanches = Avalanches_part2$Deaths, 
                               hit = Avalanches_part2$Hit,
                               season =  Avalanches_part2$Season, 
                               total = Avalanches_part2$Snow_total, 
                               j.rec = Avalanches_part2$Rec.station, 
                               J = max(Avalanches_part2$Rec.station), 
                               sigma.ub = 10, sigma.alpha.ub = 10, tau.mu.alpha = 0.1, tau.beta = 0.1)


# Initial values
avalanches.hier.inits.e <- function() {
  list( mu.alpha = rnorm(1,0,1/sqrt(0.1)),
        beta1 = rnorm(1,0,1/sqrt(0.1)),
        beta2 = rnorm(1,0,1/sqrt(0.1)),
        sigma.alpha = runif(1,0,10)
  )
}

# Model
avalanches.hier.model.e <- "model {
    ### likelihood
    for(i in 1:n) {
      avalanches[i] ~ dbin(mu[i],hit[i])
      logit(mu[i]) <- alpha[j.rec[i]] + beta1*(season[i]-mean(season[])) + beta2*(total[i]-mean(total[]))
    }
      
      
    #### priors for independent intercepts per geo space
    for(j in 1:J){ 
      alpha[j] ~ dnorm(mu.alpha, tau.alpha) 
    } 
    
    #### Hyperpriors for mu.alpha and tau.alpha
    mu.alpha ~ dnorm(0, tau.mu.alpha) 
    tau.alpha <- 1/pow(sigma.alpha,2) 
    sigma.alpha ~ dunif(0, sigma.alpha.ub)
    
    #### priors for slope
    
    beta1 ~ dnorm(0,tau.beta)
    beta2 ~ dnorm(0,tau.beta)
}"


avalanches.hier.res.e.A <- jags.model(file=textConnection(avalanches.hier.model.e),
                                      data=avalanches.hier.data.e,
                                      inits=avalanches.hier.inits.e, n.chains=3, quiet = TRUE)
update(avalanches.hier.res.c.A, n.iter= 10000) 
avalanches.hier.res.e.B <- coda.samples(avalanches.hier.res.e.A,
                                        variable.names=c("alpha","mu.alpha","sigma.alpha","beta1","beta2"), 
                                        n.iter=100000)


# summary
summary(avalanches.hier.res.e.B)

## convergence
#gelman.plot(avalanches.hier.res.e.B)
gelman.diag(avalanches.hier.res.e.B) 

##
##DIC
DIC.e <- dic.samples(model = avalanches.hier.res.e.A, n.iter = 20000, type="pD")
DIC.c <- dic.samples(model = avalanches.hier.res.c.A, n.iter = 20000, type = "pD")
diffdic(DIC.e,DIC.c)

##poserior expected value
fit_e <- as.data.frame(combine.mcmc(avalanches.hier.res.e.B))


p_rec1_2015e <- ilogit(fit_e$beta1*(2015 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta2*(7.55 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[1]`)

p_rec8_2015e <- ilogit(fit_e$beta1*(2015 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta2*(3.28 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[8]`)

p_rec10_2015e <- ilogit(fit_e$beta1*(2015 - mean(Avalanches_part2[,"Season"])) + 
                          fit_e$beta2*(2.91 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[10]`)

p_rec1_2018e <- ilogit(fit_e$beta1*(2018 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta2*(7.42 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[1]`)

p_rec8_2018e <- ilogit(fit_e$beta1*(2018 - mean(Avalanches_part2[,"Season"])) + 
                         fit_e$beta2*(6.05 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[8]`)

p_rec10_2018e <- ilogit(fit_e$beta1*(2018 - mean(Avalanches_part2[,"Season"])) + 
                          fit_e$beta2*(4.39 -  mean(Avalanches_part2[,"Snow_total"])) + fit_e$`alpha[10]`)

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

#>60%
z_rec1_2015e <- rbinom(n = length(p_rec1_2015e), size = 100, prob = p_rec1_2015e)
sum(z_rec1_2015e > 60)/length(p_rec1_2015e)

z_rec8_2015e <- rbinom(n = length(p_rec8_2015e), size = 100, prob = p_rec8_2015e)
sum(z_rec8_2015 > 60)/length(p_rec8_2015e)

z_rec10_2015e <- rbinom(n = length(p_rec10_2015e), size = 100, prob = p_rec10_2015e)
sum(z_rec10_2015e > 60)/length(p_rec10_2015e)

z_rec1_2018e <- rbinom(n = length(p_rec1_2018e), size = 100, prob = p_rec1_2018e)
sum(z_rec1_2018e > 60)/length(p_rec1_2018e)

z_rec8_2018e <- rbinom(n = length(p_rec8_2018e), size = 100, prob = p_rec8_2018e)
sum(z_rec8_2018e > 60)/length(p_rec8_2018e)

z_rec10_2018e <- rbinom(n = length(p_rec10_2018e), size = 100, prob = p_rec10_2018e)
sum(z_rec10_2018e > 60)/length(p_rec10_2018e)

