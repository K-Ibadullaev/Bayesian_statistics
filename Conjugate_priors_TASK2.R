library(dplyr)
library(magrittr)
library(extraDistr)


# load data
eqk = read.csv("earthquakes_USGS.csv", skip=2)
#check
head(eqk)
names(eqk) 
summary(eqk)


# filter data for Northwestern Europe and Mediterranean
# Northwestern Europe
North_Eur = eqk[(eqk$region>=532 & eqk$region<=549 & (eqk$magnitude>6)),]
head(North_Eur)
nrow(North_Eur)
sum(is.null(North_Eur))
summary(North_Eur)



# Mediterranean
Mediterr = eqk[eqk$region>=376 & eqk$region<=401& (eqk$magnitude>6),] 
head(Mediterr )
nrow(Mediterr)
summary(Mediterr)
sum(is.null(Mediterr))


# set prior parameters
a0 = 0
b0 = 0

# number of success events for each region

# Northwestern Europe
kE = nrow(North_Eur)

# Mediterranean
kM =  nrow(Mediterr)

# number of years 
nperiods = 2007 - 1900 +9/12

# return period
T = 1 /nperiods
round(T, 3)

# updating hyperparameters for each region 

# Northwestern Europe
xbar_kE = kE / nperiods
a_kE = a0 + (xbar_kE*nperiods)
b_kE = b0 + nperiods

# Mediterranean
xbar_kM = kM / nperiods
a_kM = a0 + (xbar_kM*nperiods)
b_kM = b0 + nperiods

# probability of absence of earthquake next year

# Northwestern Europe
f_kE =  0 # failure
n_kE = a_kE/nperiods # number of success
prob_kE = 1/(1+b_kE) # probability of of single success outcome
p_0_kE = dnbinom(f_kE, size = n_kE, prob = prob_kE )
p_0_kE


# Mediterranean
f_kM =  0 # failure
n_kM = a_kM/nperiods # number of success
prob_kM = 1/(1+b_kM) # probability of of single success outcome
p_0_kM = dnbinom(f_kM, size = n_kM, prob = prob_kM )
p_0_kM

# Model a conjugate prior for each region by inverse gamma distribution
theta =  seq(0,1000, by=1)

# conjugate prior Northwestern Europe
conj_pr_kE = dinvgamma(theta, a_kE, b_kE)
max_kE = which.max(conj_pr_kE)
conj_pr_kE[max_kE] # or simply use max()
theta[max_kE]
lambda_kE = 1/theta[max_kE]
lambda_kE
# The highest probability is 0.5% occuring at return period 54 or lambda 0.185

# conjugate prior Mediterranean
conj_pr_kM = dinvgamma(theta, a_kM, b_kM)
max_kM = which.max(conj_pr_kM)
max_kM

conj_pr_kM[max_kM] # or simply use max()
theta[max_kM]
lambda_kM = 1/theta[max_kM]
lambda_kM
# The highest probability is 29.59% occuring at return period 5 or lambda 0.2

# plot
par(mfrow=c(1,2))

plot(theta, conj_pr_kE, main = "Conjugate prior modeled by
inverse gamma distribution for  Northwestern Europe", ylab="density",
     xlab="return period",    type = "l",col="green" )
points(theta[max_kE],conj_pr_kE[max_kE], col=2, pch =19 )
abline(h=conj_pr_kE[max_kE], lty=3)
abline(v=theta[max_kE], lty=3)


plot(theta, conj_pr_kM, main = "Conjugate prior modeled by
inverse gamma distribution for  Mediterranean", ylab="density",
xlab="return period",    type = "l", col="blue", xlim =c(0, 200))
points(theta[max_kM],conj_pr_kM[max_kM], col=2, pch =19 )
abline(h=conj_pr_kM[max_kM], lty=3)
abline(v=theta[max_kM], lty=3)
