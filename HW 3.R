setwd("/Users/dxk-Ben/Desktop/Econ 617/HW 3")
library(stargazer)
library(ggplot2)

####################### Q2 
install.packages("np")
library(np)
options(np.messages=FALSE) 
data(Italy)
# (a) rule of thumb bw, Gaussian Kernel
gdp701 <- npudens(dat = Italy$gdp[Italy$year==1970], bwmethod = "normal-reference")
summary(gdp701) # bw = 1.958663
plot(gdp701, main = "Incomes 1970, rule of thumb bw", xlab = "income")
gdp981 <- npudens(dat = Italy$gdp[Italy$year==1998], bwmethod = "normal-reference")
summary(gdp981) # bw = 3.629324
plot(gdp981,main = "Incomes 1998, rule of thumb bw", xlab = "income")

# (b) LSCV bw, Gaussian kernel
gdp702 <- npudens(dat = Italy$gdp[Italy$year==1970], bwmethod = "cv.ls")
summary(gdp702) # bw = 1.740289
plot(gdp702, main = "Incomes 1970, lscv bw", xlab = "income")
gdp982 <- npudens(dat = Italy$gdp[Italy$year==1998], bwmethod = "cv.ls")
summary(gdp982) # bw = 2.517203
plot(gdp982, main = "Incomes 1998, lscv bw", xlab = "income")

# (c)
gdp703 <- npudens(dat = Italy$gdp[Italy$year==1970], bwmethod = "cv.ls", ckertype = "epanechnikov")
summary(gdp703) # bw = 1.562067
plot(gdp703, main = "Incomes 1970, lscv, Epan", xlab = "income")
gdp983 <- npudens(dat = Italy$gdp[Italy$year==1998], bwmethod = "cv.ls", ckertype = "epanechnikov")
summary(gdp983) # bw = 1.988077
plot(gdp983, main = "Incomes 1998, lscv, Epan", xlab = "income")

#################### Q3
data("cps71")
# (a) (b)
cps71$age2 <- (cps71$age)^2
ols <- lm(logwage ~ age + age2, data = cps71)
nplc <- npreg(logwage ~ age, data = cps71, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
npll <- npreg(logwage ~ age, data = cps71, regtype = "ll", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls") 
plot(cps71$age,fitted(ols), type = "l", 
     main = "ols black,lc blue,ll red", xlab = "Age", ylab = "logwage",
     xlim = c(20,70), ylim = c(12.5,14))
par(new=T)
plot(cps71$age, fitted(nplc),type="l", col = "blue", xlab = "", ylab = "", xlim = c(20,70), ylim = c(12.5,14))
par(new=T)
plot(cps71$age, fitted(npll), type="l", col = "red", xlab = "", ylab = "", xlim = c(20,70), ylim = c(12.5,14))
     # The dip present in the resulting nonparametric estimates

# (c)
plot(nplc, plot.errors.method="asymptotic", main="lc, 95% CB, asymptotic")
plot(nplc, plot.errors.method="bootstrap", main="lc, 95% CB, bootstrap")
plot(npll, plot.errors.method="asymptotic", main="ll, 95% CB, asymptotic")
plot(npll, plot.errors.method="bootstrap", main="ll, 95% CB, bootstrap")
 # The dip is significant 

# (d)
ols1 <- lm(logwage ~ age + age2, data = cps71, x=T, y=T)
npcmstest(model = ols1, xdat = cps71$age, ydat = cps71$logwage, 
          regtype = "lc", bwmethod = "cv.ls")  # reject!

# (e)
# Generalized Cross Validtion for power series
hat_k <- NA
for (K in 1:6){
  
  P_X <- matrix(, nrow = 205, ncol = K)
  
  for (j in 1:K){
    P_X[,j] <- (cps71$age-mean(cps71$age))^(j-1)
  }
  
  g_hat_ser <- P_X %*% solve(t(P_X) %*% P_X) %*% (t(P_X) %*% cps71$logwage)
  
  hat_k[K] <- (1/205)*sum((cps71$logwage-g_hat_ser)^2)/(1-K/205)^2
}
hat_k # K = 6 is has the lowest loss
#### if I run K from 1:10, I'll be getting a warning
#### "system is computationally singular". But I checked K = 7, K = 8 and K = 9 manually.

K <- 6
P_X <- matrix(, nrow = 205, ncol = K)
for (j in 1:K){
  P_X[,j] <- (cps71$age-mean(cps71$age))^(j-1)
}

g_hat_ser <- P_X %*% solve(t(P_X) %*% P_X) %*% (t(P_X) %*% cps71$logwage)
plot_data = data.frame(cps71$age,cps71$logwage,g_hat_ser)

ggplot(plot_data) +
  geom_line(aes(x = cps71$age, y = g_hat_ser, linetype = "estimation", colour = "estimation"),size=1) + 
  geom_point(aes(x = cps71$age, y = cps71$logwage, colour = "Ture"),size=0.1) + 
  theme(legend.position="right") +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_colour_manual(values = c("blue","red","black")) +  labs(x = "age") +
  labs(y = "Logwage,g(age)")

# (f)
K <- 6
P_X <- matrix(, nrow = 205, ncol = K)
for (j in 1:K){
  P_X[,j] <- (cps71$age-mean(cps71$age))^(j-1)
}

g_hat_ser <- P_X %*% solve(t(P_X) %*% P_X) %*% (t(P_X) %*% cps71$logwage)

np <- npreg(logwage ~ age, data = cps71, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
hat_e <- resid(np)
hat_v <- g_hat_ser - fitted(np)
# test statistic
hat_I <- mean(hat_e %*% hat_v) # 2.695856

################## Q4
data <- readxl::read_xls("/Users/dxk-Ben/Desktop/Econ 617/HW 3/Gasoline.xls")
gas <- data[which(data$AGE>=20 & data$PRICE>0.4 & data$FUELTYPE<=3),]
gas$dist <- as.numeric(log(gas$DIST*gas$VEH))
gas$price <-  as.numeric(log(gas$PRICE))
gas$income <-  as.numeric(log(gas$INCOME))
gas$driver <-  as.numeric(log(gas$DRIVER))
gas$hhsize <-  as.numeric(log(gas$HHSIZE))
gas$age <-  as.numeric(log(gas$AGE))
# (a)
ols <- lm(dist ~ price + income + age + driver + hhsize - 1, data = gas)
stargazer(ols)
# (b)
nwy <- npreg(dist ~ age, data = gas, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
ey <- fitted(nwy)
nwx1 <- npreg(price ~ age, data = gas, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
ex1 <- fitted(nwx1)
nwx2 <- npreg(income ~ age, data = gas, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
ex2 <- fitted(nwx2)
nwx3 <- npreg(driver ~ age, data = gas, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
ex3 <- fitted(nwx3)
nwx4 <- npreg(hhsize ~ age, data = gas, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
ex4 <- fitted(nwx4)

st2 <- lm(ey ~ ex1 + ex2 + ex3 + ex4 - 1)
beta <- summary(st2)[["coefficients"]][c(1:4),1]

y <- gas$dist - (gas$price*beta[1] + gas$income*beta[2] + gas$driver*beta[3] 
                 + gas$hhsize*beta[4])
gage <- npreg(y ~ gas$age, regtype = "lc", ckertype="gaussian", ckerorder=2, bwmethod = "cv.ls")
plot(gage, xlab = "Log Age", ylab = "Log Distance")

# (c)
# cross validation
hat_k <- NA
for (K in 1:10){
  
  P_X <- matrix(, nrow = 6230, ncol = K+4)
  P_X[,K+1] <- gas$price
  P_X[,K+2] <- gas$income
  P_X[,K+3] <- gas$driver
  P_X[,K+4] <- gas$hhsize
  for (j in 1:K){
    P_X[,j] <- (gas$age-mean(gas$age))^(j-1)
  }
  W1 <- P_X[,c(1:K)] 
  hat_y <- W1 %*% solve(t(W1) %*% W1) %*% (t(W1) %*% gas$dist)
  hat_k[K] <- (1/6230)*sum((gas$dist-hat_y)^2)/(1-K/6230)^2
}
hat_k # the results inplies using K = 10 is optimal

K <- 10
P_X <- matrix(, nrow = 6230, ncol = K+4)
P_X[,K+1] <- gas$price
P_X[,K+2] <- gas$income
P_X[,K+3] <- gas$driver
P_X[,K+4] <- gas$hhsize
for (j in 1:K){
  P_X[,j] <- (gas$age-mean(gas$age))^(j-1)
}

beta <- solve(t(P_X) %*% P_X) %*% (t(P_X) %*% gas$dist)
gamma <- beta[1:K,] # parameters for the series estimation of AGE
W1 <- P_X[,c(1:K)] 
g_age <-  W1 %*% gamma
hat_y <- W1 %*% solve(t(W1) %*% W1) %*% (t(W1) %*% gas$dist)
y1 <- gas$dist - P_X[,(K+1):(K+4)] %*% beta[(K+1):(K+4),]

plot_data = data.frame(gas$age,g_age,hat_y,y1)

# plotting g_age
ggplot(plot_data) +
  geom_line(aes(x = gas$age, y = g_age, linetype = "g_hat(log AGE)"),size=1) +
  theme(legend.position="right") +
  scale_colour_manual(values = "black") +  labs(x = "AGE") +
  labs(y = "g_hat(log AGE)")

# plotting true and estimated value
ggplot(plot_data) +
  geom_point(aes(x = gas$AGE, y = y1, colour = "true"),size=1) +
  geom_line(aes(x = gas$AGE, y = hat_y, linetype = "estimation", colour = "estimation"),size=1) +
  theme(legend.position="right") +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_colour_manual(values = c("blue","red","black")) +  labs(x = "AGE") +
  labs(y = "Dist,Dist_hat")

############### Q5 
q5 <- read.table("/Users/dxk-Ben/Desktop/Econ 617/HW 3/Q5data.txt",header = T)

y <- q5$FoodShr
x <- q5$ltexp
z <- q5$nadults + 0.8*q5$nkids

# (a)
bw <- npindexbw(formula=y~x+z, method="ichimura")
m1 <- npindex(bw, gradients=TRUE) # bw = 0.1273196

hat_g <- fitted(m1)
hat_v <- x + m1$beta[2]*z

plot_data = data.frame(hat_g,hat_v,y)

ggplot(plot_data) +
  geom_point(aes(x = hat_v, y = y, colour = "hat_v"),size=1) +
  geom_line(aes(x = hat_v, y = hat_g, linetype = "hat_g", colour = "hat_g"),size=1) +
  theme(legend.position="right") +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_colour_manual(values = c("blue","red","black")) +  labs(x = "hat_v") +
  labs(y = "hat_g,y")

# (b)
m2 <- npindex(y ~ x + z, ckertype="gaussian",bw=c(0.5,0.4), gradients=TRUE)

hat_delta <- -2/length(y)*cbind(sum(y*m2$grad[,1]),sum(y*m2$grad[,2]))
tilde_beta <- hat_delta[2]/hat_delta[1]

tilde_v <- x + tilde_beta*z
m3 <- npreg(y ~ tilde_v, regtype = "lc", ckertype="gaussian",bwmethod= "cv.ls")
tilde_g <- fitted(m3)

plot_data = data.frame(tilde_g,tilde_v,y)

ggplot(plot_data) +
  geom_point(aes(x = tilde_v, y = y, colour = "tilde_v"),size=1) +
  geom_line(aes(x = tilde_v, y = tilde_g, linetype = "tilde_g", colour = "tilde_g"),size=1) +
  theme(legend.position="right") +
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_colour_manual(values = c("blue","red","black")) +  labs(x = "tilde_v") +
  labs(y = "tilde_g,y")


########### just to check what does it like
