#Problem 1
library(rjags)
library(runjags)
#load the data 
setwd("~/BDA/assignment2")
Avalanches<-read.csv("Avalanches.csv",header = TRUE, sep=";",fileEncoding="UTF-8-BOM")
sum(is.na(Avalanches))

#(a)
#add two dummy variables
Avalanches$EADS1 <- rep(0,34)
Avalanches$EADS2 <- rep(0,34)
Avalanches$EADS1[9:18] <- rep(1,10)
Avalanches$EADS2[19:34] <- rep(1,16)

#print rows relative to 1986, 1994, 2004
subset(Avalanches, Season == 1986 | Season == 1994 | Season == 2004)

#exploratory analysis
#split the data
Period1 <- subset(Avalanches, Season %in% seq(1986, 1993))
Period2 <- subset(Avalanches, Season %in% seq(1994, 2003))
Period3 <- subset(Avalanches, Season %in% seq(2004, 2019))

#plot
par(mfrow = c(1,3))
plot(Period1[,c('Rep.events','Deaths')], main="1986~1993", 
     xlab="Rep.events", ylab="Deaths", xlim = c(0,17), ylim = c(0,12), 
     pch=19, cex=0.8, col = 'blue')
abline(lm(Period1$Deaths ~ Period1$Rep.events), col = "red")
text(Period1[,c('Rep.events','Deaths')], labels=Period1$Season, cex= 0.7, pos = 3)

plot(Period2[,c('Rep.events','Deaths')], main="1994~2003", 
     xlab="Rep.events", ylab="Deaths", xlim = c(0,17), ylim = c(0,12), 
     pch=19, cex=0.8, col = 'blue')
abline(lm(Period2$Deaths ~ Period2$Rep.events), col = "red")
text(Period2[,c('Rep.events','Deaths')], labels=Period2$Season, cex= 0.7, pos = 3)

plot(Period3[,c('Rep.events','Deaths')], main="2004~2019", 
     xlab="Rep.events", ylab="Deaths", xlim = c(0,17), ylim = c(0,12), pch=19, cex=0.8, col = 'blue')
abline(lm(Period3$Deaths ~ Period3$Rep.events), col = "red")
text(Period3[,c('Rep.events','Deaths')], labels=Period3$Season, cex= 0.7, pos = 3)
par(mfrow = c(1,1))


#relation coefficients
#cor(Period1$Rep.events, Period1$Deaths)  #0.8467379
#cor(Period2$Rep.events, Period2$Deaths)  #0.8570033
#cor(Period3$Rep.events, Period3$Deaths)  #0.6022222

#(b)
#lecture 4 Section 8

#Data block
n <- length(Avalanches$Season) 
avalanches.mult.data <- list(n=n, Deaths = Avalanches$Deaths, 
                             Rep.events = Avalanches$Rep.events, 
                             EADS1 = Avalanches$EADS1, 
                             EADS2 = Avalanches$EADS2)

#initial data
num.chains <- 3
avalanches.inits <- function(){
  list(beta0 = rnorm(0,2), 
       beta.rep.events = rnorm(0,2), 
       beta.EADS1 = rnorm(0,2),
       beta.EADS2 = rnorm(0,2)) }


#Model Statement
avalanches.mult.model <- "model { 
                  #Hyperparameters 
                
              beta.mu.0 <- 0 
              beta.tau.0 <- 0.001  #wide normal priors
              
                  # prior 
              beta0 ~ dnorm(beta.mu.0,beta.tau.0) 
              beta.rep.events ~ dnorm(beta.mu.0,beta.tau.0) 
              beta.EADS1 ~ dnorm(beta.mu.0,beta.tau.0) 
              beta.EADS2 ~ dnorm(beta.mu.0,beta.tau.0) 

                  #Likelihood 
              for(i in 1:n) {
              log(mu[i]) <- beta0+beta.rep.events*(Rep.events[i]-mean(Rep.events[])) + 
              beta.EADS1*EADS1[i] + beta.EADS2*EADS2[i]
              
              Deaths[i] ~ dpois(mu[i])
              Deaths.pred[i] ~ dpois(mu[i])}
              }" 

#Call from R to JAGS

# Run JAGS to the completion of the "adaption" stage 
results.avalanches.mult.A <- jags.model(file = textConnection(avalanches.mult.model),
                                        data = avalanches.mult.data, 
                                        inits = avalanches.inits, 
                                        n.chains = num.chains, quiet = TRUE)
# Burn-in of 5000 iterations 
burnin <- 5000
inference.length <- 10000 
update(results.avalanches.mult.A, n.iter = burnin)

# Longer run for making inferences, assuming chains have converged
results.avalanches.mult.B <- coda.samples(results.avalanches.mult.A, 
                                          variable.names=c("beta0","beta.rep.events","beta.EADS1","beta.EADS2","Deaths.pred"), 
                                          n.iter = inference.length)

# Convergence checks
#par(mfrow = c(4,2))
#plot(results.avalanches.mult.B)
#par(mfrow = c(1,1))

#gelman.plot(results.avalanches.mult.B)
gelman.diag(results.avalanches.mult.B) 

#par(mfrow = c(2,2))
#autocorr.plot(results.avalanches.mult.B[[1]][,"beta0"],main="Intercept") 
#autocorr.plot(results.avalanches.mult.B[[1]][,"beta.rep.events"],main="Rep.events") 
#autocorr.plot(results.avalanches.mult.B[[1]][,"beta.EADS1"],main="EADS1")
#autocorr.plot(results.avalanches.mult.B[[1]][,"beta.EADS2"],main="EADS2")
#par(mfrow = c(1,1))

#effn.b0 <- effectiveSize(results.avalanches.mult.B[[1]][,"beta0"]) 
#effn.b.rep.events <- effectiveSize(results.avalanches.mult.B[[1]][,"beta.rep.events"]) 
#effn.b.EADS1<- effectiveSize(results.avalanches.mult.B[[1]][,"beta.EADS1"]) 
#effn.b.EADS2 <- effectiveSize(results.avalanches.mult.B[[1]][,"beta.EADS2"]) 
summary(results.avalanches.mult.B)

fit.avalanches <- as.data.frame(combine.mcmc(results.avalanches.mult.B))

#interpret
mean(exp(fit.avalanches$beta0))
mean(exp(fit.avalanches$beta.rep.events))
mean(exp(fit.avalanches$beta.EADS1))
mean(exp(fit.avalanches$beta.EADS2))


#(c - I)
mustar <- exp(fit.avalanches$beta0 + 
                fit.avalanches$beta.rep.events*(20-mean(Avalanches$Rep.events)) + 
                fit.avalanches$beta.EADS2) 
ystar <- rpois(length(mustar), mustar)
barplot(table(ystar), main = "Frequency of observing deaths number",
        xlab = "observing deaths number", ylab = "Frequency")
abline(v = 15, col = "red")
mean(ystar < 15)
#0.1879667

#(c - II)
#before intro
mustar.before.intro <- exp(fit.avalanches$beta0 + 
                fit.avalanches$beta.rep.events*(1-mean(Avalanches$Rep.events))) 
ystar.before.intro <- rpois(length(mustar.before.intro), mustar.before.intro)
p.before.intro <- mean(ystar.before.intro > 1)
#0.3739667

#after intro
mustar.after.intro <- exp(fit.avalanches$beta0 + 
                          fit.avalanches$beta.rep.events*(1-mean(Avalanches$Rep.events))+
                          fit.avalanches$beta.EADS1) 
ystar.after.intro <- rpois(length(mustar.after.intro), mustar.after.intro)
p.after.intro <- mean(ystar.after.intro > 1)
#0.3141667

#after daily pub
mustar.after.pub <- exp(fit.avalanches$beta0 + 
                            fit.avalanches$beta.rep.events*(1-mean(Avalanches$Rep.events))+
                            fit.avalanches$beta.EADS2) 
ystar.after.pub <- rpois(length(mustar.after.pub), mustar.after.pub)
p.after.pub <- mean(ystar.after.pub > 1)
#0.1048333

par(mfrow = c(1,3))
barplot(table(ystar.before.intro), main = "Before Introduction",
        xlab = "observing deaths number", ylab = "Frequency")
abline(v = 1, col = "red")
barplot(table(ystar.after.intro), main = "After Introduction",
        xlab = "observing deaths number", ylab = "Frequency")
abline(v = 1, col = "red")
barplot(table(ystar.after.pub),  main = "Daily Publication",
        xlab = "observing deaths number", ylab = "Frequency")
abline(v = 1, col = "red")
par(mfrow = c(1,1))

#(d)
par(mfrow = c(1,2))
curve(dnorm(x,0,sqrt(4/25)), xlim = c(-1, 1), 
      ylab = "", main = "beta ~ Normal(0 , sigma)")
abline(v = c(log(0.25)/5, log(4)/5), col = "red")
curve(dlnorm(x,0,2), xlim = c(0,8), 
      ylab = "", main = "phi ~ LogNormal(0 , 5sigma)")
abline(v = c(0.25,4), col = "red")
par(mfrow = c(1,1))

# sample 10000 data
sample_norm <- rnorm(10000, 0, 2/5)
sample_lnorm <- rlnorm(10000, 0, 2)

#area between lines
sum(sample_norm> -log(4)/5 & sample_norm < log(4)/5)/10000
sum(sample_lnorm> 0.25 & sample_lnorm < 4)/10000

#(e)
# Data block 
avalanches.hier.data <- list(n=n, Deaths = Avalanches$Deaths, 
                             Rep.events = Avalanches$Rep.events, 
                             EADS1 = Avalanches$EADS1, 
                             EADS2 = Avalanches$EADS2)

# Initial values 
num.chains <- 3
avalanches.hier.inits <- function(){
  list(beta0 = rnorm(0,2), 
       beta.rep.events = rnorm(0,2), 
       beta.EADS1 = rnorm(0,2),
       beta.EADS2 = rnorm(0,2),
       theta.sigma = runif(0,10)) }

#Model Statement
avalanches.hier.model <- "model { 

              #Likelihood 
              for(i in 1:n) {
              Deaths[i] ~ dpois(mu[i])
              Deaths.pred[i] ~ dpois(mu[i])
              log(mu[i]) <- beta0 + beta.rep.events*(Rep.events[i] - mean(Rep.events[])) + 
              beta.EADS1*EADS1[i] + beta.EADS2*EADS2[i] - theta[i]
              
              theta[i] ~ dlnorm(0, theta.tau)}
                  
               # prior 
              beta0 ~ dnorm(0, 0.01) 
              beta.rep.events ~ dnorm(0, 0.01) 
              beta.EADS1 ~ dnorm(0, 0.01) 
              beta.EADS2 ~ dnorm(0, 0.01) 
              
              
              #Hyperpriors for theta
              theta.tau <- pow(theta.sigma,-2) 
              theta.sigma ~ dunif(0, 10)
              }" 

# Inference 
avalanches.hier.res.A <- jags.model(file = textConnection(avalanches.hier.model), 
                                    data = avalanches.hier.data, 
                                    inits = avalanches.hier.inits, 
                                    n.chains = num.chains, quiet = TRUE) 
burnin <- 5000
inference.length <- 50000 

update(avalanches.hier.res.A, n.iter = burnin) 
avalanches.hier.res.B <- coda.samples(avalanches.hier.res.A, 
                                      variable.names=c("theta.sigma", "beta0","beta.rep.events","beta.EADS1","beta.EADS2","Deaths.pred","theta"), 
                                      n.iter  = inference.length)

#convergence
gelman.diag(avalanches.hier.res.B) 

#interpret
summary(avalanches.hier.res.B)
fit.avalanches.hier <- as.data.frame(combine.mcmc(avalanches.hier.res.B))

#interpret
mean(exp(fit.avalanches.hier$beta0))
mean(exp(fit.avalanches.hier$beta.rep.events))
mean(exp(fit.avalanches.hier$beta.EADS1))
mean(exp(fit.avalanches.hier$beta.EADS2))

#mean(exp(fit.avalanches.hier$`theta[34]`))
post.theta <- data.frame(avalanches.hier.res.B[[1]][,39:72])
post.plot.theta <- data.frame()

for (i in 1:34){
  mean <- mean(post.theta[,i])
  lb <- quantile(post.theta[,i], 0.05)
  up <- quantile(post.theta[,i], 0.95)
  post.plot.theta <- rbind(post.plot.theta, c(mean, lb, up))
}

post.plot.theta <- cbind(post.plot.theta, Avalanches$Season)
colnames(post.plot.theta) <- c("Mean", "lb", "up", "Year")

plot(post.plot.theta[,c("Year", "Mean")], main="Posterior means and 90% CI for theta[i]",
     xlab="Year", ylab="Mean or 90% CI", ylim = c(0,10),pch=19, cex=0.5, col = "black")
for (i in 1:34){
  segments(post.plot.theta[i, "Year"], post.plot.theta[i, "lb"], 
           post.plot.theta[i, "Year"], post.plot.theta[i, "up"], col = "gray", lty = 3)
}


# compare
#pred
post.b <- data.frame(results.avalanches.mult.B[[1]][,1:34])
post.e <- data.frame(avalanches.hier.res.B[[1]][,1:34])

Deaths.pred.b <-data.frame(colMeans(post.b))
sum((Avalanches$Deaths -  Deaths.pred.b)^2)/34

Deaths.pred.e <-data.frame(colMeans(post.e))
sum((Avalanches$Deaths -  Deaths.pred.e)^2)/34

#plot
par(mfrow = c(1,2))
plot(Avalanches$Deaths, Deaths.pred.b[,1], pch=19, cex=0.5, col = "black",
     xlab = "Deaths values fitted in (b)", ylab = "Real Deaths value")
plot(Avalanches$Deaths, Deaths.pred.e[,1], pch=19, cex=0.5, col = "black",
     xlab = "Deaths values fitted in (e)", ylab = "Real Deaths value")
par(mfrow = c(1,1))

#summary(results.avalanches.mult.B)
#summary(avalanches.hier.res.B)




#(f)
post.b <- data.frame(results.avalanches.mult.B[[1]][,1:34])
post.e <- data.frame(avalanches.hier.res.B[[1]][,1:34])

post.plot.b <- data.frame()
post.plot.e <- data.frame()

for (i in 1:34){
  mean <- Deaths.pred.b[i,1]
  lb <- quantile(post.b[,i], 0.05)
  up <- quantile(post.b[,i], 0.95)
  post.plot.b <- rbind(post.plot.b, c(mean, lb, up))
}

for (i in 1:34){
  mean <- Deaths.pred.e[i,1]
  lb <- quantile(post.e[,i], 0.05)
  up <- quantile(post.e[,i], 0.95)
  post.plot.e <- rbind(post.plot.e, c(mean, lb, up))
}

post.plot.b <- cbind(post.plot.b, Avalanches$Season)
post.plot.e <- cbind(post.plot.e, Avalanches$Season)
colnames(post.plot.b) <- c("Mean", "lb", "up", "Year")
colnames(post.plot.e) <- c("Mean", "lb", "up", "Year")

#DIC
DIC.b <- dic.samples(model = results.avalanches.mult.A, n.iter=20000,type="pD")
DIC.e <- dic.samples(model = avalanches.hier.res.A, n.iter=20000,type="pD")
diffdic(DIC.b,DIC.e)

#b
plot(post.plot.b[,c("Year", "Mean")], main="Posterior means and 90% CI for (b)",
     xlab="Year", ylab="Mean or 90% CI", ylim = c(-2,18), pch=19, cex=0.5, col = "blue")

for (i in 1:34){
  segments(post.plot.b[i, "Year"], post.plot.b[i, "lb"], 
           post.plot.b[i, "Year"], post.plot.b[i, "up"], col = "gray", lty = 3)
}
points(Avalanches$Season, Avalanches$Deaths, col="red", pch=2, cex = 0.5)
legend(1986, 17, legend=c("Mean", "90% CI", "Real value"), col=c("blue", "red", "gray"),
       fill = c("blue", "red", "gray"), cex = 0.4)


#e
plot(post.plot.e[,c("Year", "Mean")], main="Posterior means and 90% CI for (e)",
     xlab="Year", ylab="Mean or 90% CI", ylim = c(-2,18), pch=19, cex=0.5, col = "blue")
for (i in 1:34){
  segments(post.plot.e[i, "Year"], post.plot.b[i, "lb"], 
           post.plot.e[i, "Year"], post.plot.b[i, "up"], col = "gray", lty = 3)
}
points(Avalanches$Season, Avalanches$Deaths, col="red", pch=2, cex = 0.5)
legend(1986, 17, legend=c("Mean", "90% CI", "Real value"), col=c("blue", "red", "gray"),
       fill = c("blue", "red", "gray"), cex = 0.4)
