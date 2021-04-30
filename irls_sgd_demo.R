# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Simulate data
source("sim_data1.R")
# Load common functions
source("functions.R")

### Demonstrate the convergence of the S-IRLS-SGD algorithm for a logistic model

# Calculate the true model using glm.fit (i.e. Full IRLS)
true_glm_res_l <- glm.fit(million_x, million_y_l, family=binomial())

# Calculate the estimated model using S-IRLS-SGD
set.seed(123)
irlssgd_res_l <- irls.sgd(million_x, million_y_l, binomial(),
            irls.control=list(subs=0.0005, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.0005, maxit=500, alpha=0.05, decay=0.99, histfreq=10), T)

# Create a plot showing the convergence of S-IRLS-SGD
par(mfrow=c(4,4),oma = c(0,0,2,0))
for (i in 1:16) {
  multiplot(cbind(c(irlssgd_res_l$irls_hist[,i], irlssgd_res_l$sgd_hist[,i]), true_glm_res$coefficients[i]),
            frame.plot=F,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
  abline(v=length(irlssgd_res_l$irls_hist[,i]))
}
mtext("Convergence of parameter estimates in S-IRLS-SGD for a logistic model", outer = TRUE, cex = 1.5)

### Demonstrate the convergence of the S-IRLS-SGD algorithm for a gaussian model

# Calculate the true model using glm.fit (i.e. Full IRLS)
true_glm_res_g <- glm.fit(million_x[,1:8], million_y_g, family=gaussian())

# Calculate the estimated model using S-IRLS-SGD
set.seed(123)
irlssgd_res_g <- irls.sgd(million_x[,1:8], million_y_g, gaussian(),
            irls.control=list(subs=0.001, maxit=20, tol=1e-7, cooling = c(1,0.9,0.75), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.001, maxit=250, alpha=0.001, decay=0.99, histfreq=10), T)

# Create a plot showing the convergence of S-IRLS-SGD
par(mfrow=c(2,4),oma = c(0,0,2,0))
for (i in 1:8) {
  multiplot(cbind(c(irlssgd_res_g$irls_hist[,i], irlssgd_res_g$sgd_hist[,i]), true_glm_res_g$coefficients[i]),
            frame.plot=F,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
  abline(v=length(irlssgd_res_g$irls_hist[,i]))
}
mtext("Convergence of parameter estimates in S-IRLS-SGD for a Gaussian model", outer = TRUE, cex = 1.5)