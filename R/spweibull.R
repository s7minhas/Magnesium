spweibull <-
function(y, X, Z, y.test, X.test, Z.test, iter, sims, rhb, rhg) {
	require(corpcor) # make.positive.definite
	X <- as.matrix(cbind(1, X))
	Z <- as.matrix(cbind(1, Z))
	X.test <- as.matrix(cbind(1, X.test))
	Z.test <- as.matrix(cbind(1, Z.test))
	base <- optim(c(rep(1, ncol(X)+1)), weib.lik, method="BFGS", control=list(maxit=iter), hessian=T, y=y, X=X)
	x.inits <- base$par[1:ncol(X)]
	a.init <- base$par[ncol(X)+1]
	est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib.lik, method="BFGS", control=list(trace=T, maxit=iter), hessian=T, y=y, X=X, Z=Z)
	if (est$convergence!=0) stop("Model did not converge")
	coeff <- est$par
	vcv <- solve(est$hessian)
	vcv <- make.positive.definite(vcv)
	se <- sqrt(diag(vcv))
	zstat <- coeff/se
	pval <- 2*(1-pnorm(abs(zstat)))
	results <- cbind(coeff, se, round(zstat,3), round(pval,3))
	rownames(results) <- c("Constant (duration)", rhb, "Constant (cure)", rhg, "log(alpha)")
	print(results)
	output <- list(coeff = coeff, vcv = vcv, results = results)
	invisible(return(output))
}
