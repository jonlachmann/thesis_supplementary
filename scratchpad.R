# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26


kmdata2 <- cbind(1,kmdata[,-1])
kmdata2[,2] <- troot(kmdata2[,5])
kmdata2[,2] <- kmdata2[,8] #*kmdata2[,5]*kmdata2[,8]


gaussian.loglik2(kmdata[,1], kmdata2, c(T,T,F,F,T,F,F,F,F,F,F), complex=list(width=rep(0,2)), list(r=1/223))
gaussian.loglik3(kmdata[,1], kmdata2, c(T,T,F,F,F,F,F,F,F,F,F), complex=list(width=rep(0,2)), list(r=1/223))



gaussian.loglik2 <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- fastglm(as.matrix(x[,model]), y, family=gaussian())})
  ret <- (-(mod$deviance -2*log(params$r)*sum(complex$width)))/2
  return(ret)
}

gaussian.loglik3 <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- glm.fit(as.matrix(x[,model]), y)})
  ret <- (-(mod$deviance -2*log(params$r)*sum(complex$width)))/2
  return(ret)
}




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

sgd_log <- function (data=NULL, ctrl=list(start, alpha, decay, subs, maxit, histfreq)) {
  # Initialize coefficients from start or randomly
  if (is.null(ctrl$start)) x <- rnorm(ncol(data)-1)
  else x <- ctrl$start
  nvars <- length(x)

  # Set up default values if not set
  if (is.null(ctrl$alpha)) ctrl$alpha <- 0.0005
  if (is.null(ctrl$decay)) ctrl$decay <- 0.999
  if (is.null(ctrl$subs)) ctrl$subs <- 1
  if (is.null(ctrl$maxit)) ctrl$maxit <- 1000
  if (is.null(ctrl$histfreq)) ctrl$histfreq <- 50

  # Get observation count and set subsample size
  if (!is.null(data)) {
    nobs <- nrow(data)
    sub_size <- nobs*ctrl$subs
  }

  # Set decay
  if (is.null(ctrl$decay)) ctrl$decay <- (maxit-2)/maxit

  # Set up matrix for history
  xhist <- matrix(NA, ctrl$maxit/ctrl$histfreq+1, nvars)
  xhist[1,] <- x

  # Run (S)GD
  for (i in 1:ctrl$maxit) {
    if (ctrl$subs != 1) sub_idx <- sample(1:nobs, sub_size, replace=T)
    else sub_idx <- 1:nobs
    ctrl$alpha <- ctrl$alpha * ctrl$decay

    eta <- family$linkinv(data[sub_idx,-1]%*%x)
    grad <- (t(data[sub_idx,-1])%*%(eta-data[sub_idx,1])/sub_size)

    if (!is.null(data)) x <- x - ctrl$alpha * grad
    else x <- x - ctrl$alpha * gradient(x)
    if (i %% ctrl$histfreq == 0) xhist[i/ctrl$histfreq+1,] <- x
  }
  return(list(x=x, xhist=xhist))
}

sgd_log(cbind(million_y_l, million_x), ctrl=list(subs=0.01, maxit=500, alpha=0.05, decay=0.99, histfreq=10))