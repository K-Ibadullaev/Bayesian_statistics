
set.seed(0)
# parameters for g(x)
alpha_gx = 0.5
beta_gx = 0.5

# parameters for f(x)
alpha_fx = 3
beta_fx = 7

# Create sample of uniform distribution
u = runif(5000)

# generate weights
w = dbeta(u, shape1 = alpha_fx, shape2 = beta_fx)/dbeta(u, shape1 = alpha_gx, shape2 = beta_gx)
w = w / sum(w)

# Mean estimate and true mean
mu_E = u %*% w
mu_t = alpha_fx / (alpha_fx + beta_fx)
c(mu_E, mu_t)


# Variance estimate and true variance
var_E = (u-c(mu_E))^2 %*% w   
var_t = (alpha_fx * beta_fx)/((alpha_fx + beta_fx)^2*(alpha_fx + beta_fx+1))   # true
c(var_E, var_t)
# density
dd = density(u, weights = w, from=0, to=1)

par(mfrow=c(2,1))
#Plot KDE
plot(dd, xlab = "Weights", ylab="Density",
main = "Importance sampling(II) of Ilmenite grains in
(supposed) Rutiles in a heavy mineral concentrate.", lty=1,col=4)

# Plot MC
lines(dd$x, dbeta(dd$x, shape1 = alpha_fx, shape2 = beta_fx), lty=3,col=2)
legend("topright", col = c(4,2),lty = c(1,3), c("KDE", "Monte Carlo") )

# One can observe some discrepancies between distributions, 
# more significant than in case with f(x)
# Also setting of seed as well as parameters alpha and beta affect 
# on difference between the curves

un = runif(5000)
wf = dbeta(un, shape1 = alpha_fx, shape2 = beta_fx)
wf = wf/sum(wf)
(mn=un %*% wf)
ddf = density(un, weights = wf, from=0, to=1)
plot(ddf, xlab = "Weights", ylab="Density",
     main = "Importance sampling (I) of Ilmenite grains in
(supposed) Rutiles in a heavy mineral concentrate.", lty=1,col=4)
lines(ddf$x, dbeta(ddf$x, shape1 = 3, shape2 = 7), col=2)
legend("topright", col = c(4,2),lty = c(1,3), c("KDE", "Monte Carlo") )

