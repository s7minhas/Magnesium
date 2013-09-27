spweib.lik <-
function(theta,y,X,Z){
	rx <- ncol(X)
	rz <- ncol(Z)
	beta <- theta[1:rx]
	gamma <- theta[(rx+1):(rx+rz)]
	p <- theta[rx+rz+1]
	d <- y[,1]
	ti <- y[,2]
	ly <- y[,3]
	t0 <- y[,4]
    lambda <- exp(-X%*%beta)
    alpha <- exp(-p)
    pr1 <- plogis(Z%*%gamma)
    pr0 <- plogis(Z%*%gamma, lower.tail=F)
    ln.ft <- log(alpha) + (alpha)*log(lambda) + (alpha-1)*log(ti) - (lambda*ti)^alpha
    st <- (exp(-(lambda*ti)^alpha))
    st0 <- (exp(-(lambda*t0)^alpha))
    nocens <- ifelse((d==1) & (ly==1), log(pr1) + ln.ft - log(pr0 + (pr1 * st0)), 0)
    cens <- ifelse((d==1) & (ly==0) | (d==0), log(pr0 + (pr1 * st)) - log(pr0 + (pr1 * st0)), 0)
    logl <- sum(cens+nocens)
	return(-logl)
}
