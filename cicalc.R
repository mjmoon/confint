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
  ## Normal distribution ##
  if(input$dist == 1){
    cilvlq <- -qnorm((1 - input$cilvl)/2)
    fillcol <- apply(col2rgb(palette()[1])/255, 2, function(x) rgb(x[1], x[2], x[3], 0.5))
    s <- input$parm2/sqrt(input$nsamp)
    # plot density line
    # xlims <- c(input$parm1 - 5*s, input$parm1 + 5*s)
    # x <- seq(xlims[1], xlims[2], s/50)
    xlims <- c(input$parm1 - input$parm2, input$parm1 + input$parm2)
    x <- seq(xlims[1], xlims[2], input$parm2/1000)
    plot(x, dnorm(x, input$parm1, s), 
         xlim = xlims, ylim = c(0, dnorm(input$parm1, input$parm1, s)),
         type = "l", axes = FALSE, ylab = "", xlab = "")
    # shade confidence interval
    # x <- seq(input$parm1 - cilvlq*s, input$parm1 + cilvlq*s, s/50)
    x <- seq(input$parm1 - cilvlq*s, input$parm1 + cilvlq*s, input$parm2/1000)
    polygon(x, dnorm(x, input$parm1, s), 
            border = NA, col = fillcol)
    polygon(c(rep(min(x),2), rep(max(x),2)), 
            c(0, dnorm(min(x), input$parm1, s), dnorm(max(x), input$parm1, s), 0),
            border = NA, col = fillcol)
    abline(v = input$parm1, col = 3, lty = 2, lwd = 2)
    # label
    text(input$parm1, dnorm(input$parm1, input$parm1, s)*0.5,
         input$cilvl, cex = 1.8, font = 2)
    text(xlims[1], 0, expression(paste("N(", mu, ", ", sigma^"2", "/n)", sep = "")),
         cex = 1.2, adj = c(0,-0.2))
    axis(1, at = round(c(xlims, input$parm1),2))
  }
}
# plot simulated confidence intervals
plotcint <- function(input){
  par(mar = c(0,0,0,0))
  ## Normal distribution ##
  if(input$dist == 1){
    cilvlq <- -qnorm((1 - input$cilvl)/2)
    s <- input$parm2/sqrt(input$nsamp)
    # simulate data and statistics
    dt <- matrix(rnorm(input$niter*input$nsamp, input$parm1, input$parm2), 
                    nrow = input$niter)
    sts <- data.frame(xbar = apply(dt, 1, mean), 
                      cis = t(apply(dt, 1, 
                                    function(x) 
                                      mean(x) + c(-1, 1)*cilvlq*sd(x)/sqrt(input$nsamp))))
    names(sts) <- c("xbar", "lci", "uci")
    sts$incmu <- input$parm1 >= sts$lci & input$parm1 <= sts$uci
    
    nciinc <<- cbind(sum(sts$incmu), nciinc)
    rciinc <<- cbind(sum(sts$incmu)/input$niter, rciinc)
    # plot sample mean and confidence intervals
    # xlims <- c(input$parm1 - 5*s, input$parm1 + 5*s)
    xlims <- c(input$parm1 - input$parm2, input$parm1 + input$parm2)
    plot(0, type = 'n',
         xlim = xlims, ylim = c(0, input$niter + 1), 
         axes = FALSE, ylab = "", xlab = "")
    abline(v = input$parm1, col = 3, lty = 2, lwd = 2)
    segments(sts$lci, 1:input$niter, sts$uci, 1:input$niter,
             ifelse(sts$incmu, 1, 2))
    points(sts$xbar, 1:input$niter, pch = 18, col = ifelse(sts$incmu, 1, 2))
  }
}
# plot mu-in-ci rates
plotciinc <- function(input){
  par(mar = c(1,3,1,1))
  plot(1:length(rciinc), rciinc, col = 1, type = "b", pch = 19,
       ylim = c(0,1), xlim = c(0, 10), ylab = "", xlab = "", axes = FALSE, 
       cex = 0.7, lwd = 2)
  abline(h = input$cilvl, lty = 2, col = 2, lwd = 1)
  axis(2, at = c(0,1), line = 0, las = 1, cex = 0.3)
  par(xpd = TRUE)
  legend(5, 0.4, legend = c("Simulated Confidence Levels", 
                           "Specified Confidence Level"),
         lty = c(1, 2), col = c(1, 2), pch = c(19, NA), 
         lwd = 2, xjust = 0.5, yjust = 1, bty = 'n', cex = 0.8)
  par(xpd = FALSE)
}