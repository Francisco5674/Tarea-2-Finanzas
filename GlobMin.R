
globalMin.portfolio <-
  function(er, cov.mat, shorts=TRUE)
  {
    call <- match.call()
    
    #
    # check for valid inputs
    #
    asset.names <- names(er)
    er <- as.vector(er) # assign names if none exist
    cov.mat <- as.matrix(cov.mat)
    N <- length(er)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semi-definite
    
    #
    # compute global minimum portfolio
    #
    if(shorts==TRUE){
      cov.mat.inv <- solve(cov.mat)
      one.vec <- rep(1,N)
      w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
      w.gmin <- as.vector(w.gmin)
    } else if(shorts==FALSE){
      Dmat <- 2*cov.mat
      dvec <- rep.int(0, N)
      Amat <- cbind(rep(1,N), diag(1,N))
      bvec <- c(1, rep(0,N))
      result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.gmin <- round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(w.gmin) <- asset.names
    er.gmin <- crossprod(w.gmin,er)
    sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
    gmin.port <- list("call" = call,
                      "er" = as.vector(er.gmin),
                      "sd" = as.vector(sd.gmin),
                      "weights" = w.gmin)
    class(gmin.port) <- "portfolio"
    gmin.port
  }

