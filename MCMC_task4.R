# setup
n = 30
ln_mean = 3.442
ln_var = 2.182

# define MC steps
iters = 5000

# exponentiate var
vr = exp(ln_var)

# construct an empty matrix of results for iterations
# init y0 and tau - Markov chain sequence
y = rep(0.0, iters)
tau = rep(0.0, iters)

# set the value of the first element
tau[1] = round(1 / (2*vr),3)

# Gibbs sampling

i = 2 # or use for{} but end with i=2:(iters - 1)
while (i<iters) {
  mu = ln_mean
  sigma = sqrt(1/(2*n*tau[i-1]))
  y[i] = rnorm(1,mu, sigma )
  
  SSQ = ((n-1)* ln_var + n*(y[i] - ln_mean)**2)
  a = n/2
  r = SSQ
  tau[i] = rgamma(n=1, shape = a, scale = r )
  i=i+1
}
# Describe tau and y
summary(tau)
summary(y)


# Visualization

par(mfrow=c(2,1))
plot(y, xlab =" index", ylab = "Y",col=2,type = "l",
     main = "Probability of Y\n before Burn-in and Thinning")
plot(tau, xlab =" index", ylab = "Tau",col=2,type = "l",
     main = "Probability of Tau\n before Burn-in and Thinning")

# Burn-in and thinning 
# Burn-in effect by removing the first k elements
# thinning effect with 20 steps
k=iters/25 +1
step_sz = 20
tk = seq(from= k, to=iters, by=step_sz)
tk

tau_tk = tau[tk]
y_tk = y[tk]


# Visualize
par(mfrow=c(2,1))
plot(y_tk, xlab =" index", ylab = "Y",col=2,type = "l",
     main = "Probability of Y\n after Burn-in and Thinning")
plot(tau_tk, xlab =" index", ylab = "Tau",col=2,type = "l",
     main = "Probability of Tau\n after Burn-in and Thinning")

# Comparing figures we can see that the influence of the initial value and the periodicity of the realisations have been greatly removed.

# E[X] and Var[X]

# Confidence intervals for MC sample
sigma_sqr = 1 / (2 * tau)

E_x = exp(y + 0.5 * sigma_sqr)
Var_x = E_x^2 *(exp(sigma_sqr) - 1)

quantiles = c(0.025, 0.05, 0.95, 0.975)

# Confidence intervals for E[X]
q_E_x = quantile(E_x,quantiles)
(q_E_x)
# Confidence intervals for Var[X]
q_Var_x = quantile(Var_x,quantiles)
(q_Var_x)
