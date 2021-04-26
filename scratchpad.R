# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26







system.time(irlssgd_res_g <- irls.sgd(million_x, million_y_g, gaussian(),
            irls.control=list(subs=0.0005, maxit=12, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.0005, maxit=500, alpha=0.0025, decay=0.99, histfreq=1), T))

system.time(true_glm_res_g <- glm.fit(million_x, million_y_g, family=gaussian()))

dff <- as.data.frame(cbind(million_y_g,million_x))

glmfit <- glm(V1 ~ .-1, data=dff, family=gaussian())

# Create a plot showing the convergence of S-IRLS-SGD
par(mfrow=c(4,4),oma = c(0, 0, 2, 0))
for (i in 1:16) {
  multiplot(cbind(c(irlssgd_res_g$irls_hist[,i], irlssgd_res_g$sgd_hist[,i]), true_glm_res_g$coefficients[i]),
            frame.plot=F,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
  abline(v=length(irlssgd_res_g$irls_hist[,i]))
}
mtext("Convergence of parameter estimates in S-IRLS-SGD", outer = TRUE, cex = 1.5)

# Calculate the full model set using regular glm (SLOW!)
full_10K <- vector("list", full_model_count)
progress <- 0
for (i in 1:full_model_count) {
  modelvector <- as.logical(c(T,intToBits(i)[1:15]))
  loglik <- logistic.loglik.aic(million_y_l[1:10000], million_x[1:10000,], modelvector, NULL, NULL)
  full_10K[[i]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
  if (i %% floor(full_model_count/40) == 0) progress <- print.progressbar(progress, 40)
}

loglik <- logistic.loglik.aic.irlssgd(million_y_l[1:10000], million_x[1:10000,], rep(T,16), NULL, list(subs = 0.03))