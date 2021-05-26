
plot.Markowitz <-
  function(x, plot.assets=FALSE, ...)
  {
    if (!plot.assets) {
      y.lim=c(0,max(x$er))
      x.lim=c(0,max(x$sd))
      plot(x$sd,x$er,type="b",xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER", 
           main="Efficient Frontier", ...)
    }
    else {
      call = x$call
      mu.vals = eval(call$er)
      sd.vals = sqrt( diag( eval(call$cov.mat) ) )
      y.lim = range(c(0,mu.vals,x$er))
      x.lim = range(c(0,sd.vals,x$sd))
      plot(x$sd,x$er,type="b", xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER", 
           main="Efficient Frontier", ...)
      text(sd.vals, mu.vals, labels=names(mu.vals))
    }
    invisible()
  }
