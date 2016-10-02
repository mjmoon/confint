#############################################################
# confint - visualize confindence interval by michael moon  #
#                                                           #
# Confidence interval calculation and plotting              #
#############################################################
nciinc <- NULL
rciinc <- NULL
# plot sampling distribution
plotdist <- function(input){
  par(mar = c(2,0,0,0))
  fillcol <- apply(col2rgb(palette()[1])/255, 2, function(x) rgb(x[1], x[2], x[3], 0.5))
  sigma <- 1
  mu <- 0
  xlims <- c(-1, 1)
  x <- seq(-sigma, sigma, sigma/1000)
  s <- sigma/sqrt(input$nsamp)
  cilvlq <- -qnorm((1 - input$cilvl)/2)
  # plot density line
  plot(x, dnorm(x, mu, s), 
       xlim = xlims, ylim = c(0, dnorm(mu, mu, s)),
       type = "l", axes = FALSE, ylab = "", xlab = "")
  # shade confidence interval
  x <- seq(mu - cilvlq*s, mu + cilvlq*s, sigma/1000)
  polygon(x, dnorm(x, mu, s), border = NA, col = fillcol)
  polygon(c(rep(min(x),2), rep(max(x),2)), 
          c(0, dnorm(min(x), mu, s), dnorm(max(x), mu, s), 0), 
          border = NA, col = fillcol)
  abline(v = mu, col = 3, lty = 2, lwd = 2)
  # label
  text(mu, dnorm(mu, mu, s)*0.5,
       input$cilvl, cex = 1.8, font = 2)
  text(xlims[1], 0, expression(paste("N(", mu, ", ", sigma^"2", "/n)", sep = "")),
       adj = c(0,-0.2))
  axis(1, at = c(-sigma, mu, sigma),
       labels =c(expression(paste("-",sigma, sep = "")),
                 expression(mu),
                 expression(sigma))
       )
}
# plot simulated confidence intervals
plotci <- function(niter, nsamp, cilvl){
  par(mar = c(0,0,0,0))
  mu <- 0
  sigma <- 1
  cilvlq <- -qnorm((1 - cilvl)/2)
  xlims <- c(-1, 1)
  # simulate data and statistics
  dt <- matrix(rnorm(niter*nsamp, mu, sigma), 
               nrow = niter)
  sts <- data.frame(xbar = apply(dt, 1, mean), 
                    cis = t(apply(dt, 1, 
                                  function(x) 
                                    mean(x) + c(-1, 1)*cilvlq*sd(x)/sqrt(nsamp))))
  names(sts) <- c("xbar", "lci", "uci")
  sts$incmu <- mu >= sts$lci & mu <= sts$uci
  # store frequency of ci including mu
  nciinc <<- cbind(sum(sts$incmu), nciinc)
  rciinc <<- cbind(round(sum(sts$incmu)/niter, 2), rciinc)
  # plot sample mean and confidence intervals
  plot(0, type = 'n',
       xlim = xlims, ylim = c(0, niter + 1), 
       axes = FALSE, ylab = "", xlab = "")
  abline(v = mu, col = 3, lty = 2, lwd = 2)
  segments(sts$lci, 1:niter, sts$uci, 1:niter,
           ifelse(sts$incmu, 1, 2))
  points(sts$xbar, 1:niter, pch = 16, col = ifelse(sts$incmu, 1, 2), cex = 1.2)
}
# plot mu-in-ci rates
plotciinc <- function(cilvl, rciinc_){
  par(mar = c(1,3,1,1))
  plot(1:length(rciinc_), rciinc_, col = 1, type = "b", pch = 19,
       ylim = c(0,1), xlim = c(0, 10), ylab = "", xlab = "", axes = FALSE, 
       cex = 0.7, lwd = 2)
  abline(h = cilvl, lty = 2, col = 2, lwd = 1)
  axis(2, at = c(0,1), line = 0, las = 1, cex = 0.3)
  par(xpd = TRUE)
  legend(5, 0.4, legend = c("Simulated Confidence Levels", 
                           "Specified Confidence Level"),
         lty = c(1, 2), col = c(1, 2), pch = c(19, NA), 
         lwd = 2, xjust = 0.5, yjust = 1, bty = 'n', cex = 0.8)
  par(xpd = FALSE)
}