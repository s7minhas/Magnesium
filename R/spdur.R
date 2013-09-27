spdur <-
function(formula, cure=formula2, data=NULL, test=NULL, last=NULL, distr="", re="", t.0="t.0", iter=NULL, sims=NULL) {

	if (is.null(data)) stop("No data provided")
	if (is.null(last)) stop("Must specify censoring variable")
	if (distr=="") stop("Must specify distribution")

	a <- as.character(formula)
	b <- as.character(cure)

	lhb <- a[2]
	rhb <- strsplit(a[3], split=" + ", fixed=T)[[1]]

	lhg <- b[2]
	rhg <- strsplit(b[3], split=" + ", fixed=T)[[1]]

	X <- data[rhb]
	Z <- data[rhg]
	Y <- cbind(data[lhg], data[lhb], last, data[t.0])

	X.test <- test[rhb]
	Z.test <- test[rhg]
	Y.test <- cbind(test[lhg], test[lhb])

	if (is.null(iter)) iter<-100
	if (is.null(sims)) sims<-1000

    if (distr=="weibull") {
        model<-spweibull(Y, X, Z, Y.test, X.test, Z.test, iter, sims, rhb, rhg)
        }

    if (distr=="loglog") {
        model<-sploglog(Y, X, Z, Y.test, X.test, Z.test, iter, sims, rhb, rhg)
        }
	output <- list(model=model, formula=formula, cure=cure, distr=distr, last=last, data=data, test=test)
    invisible((output))
}
