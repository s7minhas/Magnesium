weib.lik <-
function(theta,y,X){
	beta <- theta[1:ncol(X)]
	p <- theta[ncol(X)+1]
	d <- y[,1]
	ti <- y[,2]
	ly <- y[,3]
	t0 <- y[,4]
	lambda <- exp(-X%*%beta)
	alpha <- exp(-p)
    ln.ft  <- log(alpha) + (alpha)*log(lambda) + (alpha-1)*log(ti) - (lambda*ti)^alpha
    ln.st  <- -(lambda*ti)^alpha
    ln.st0 <- -(lambda*t0)^alpha
    cens <- ifelse((d==1) & (ly==0) | (d==0) , ln.st - ln.st0 , 0)
    nocens <- ifelse((d==1) & (ly==1) , ln.ft - ln.st0 ,0)
	logl<-sum(cens+nocens)
	return(-logl)
}
