library(RPostgreSQL)
library(RJSONIO)

midjaSquare <- function(x) {
	x * x
}

midjaJson <- function(inputjsonstr) {
  inputjson <- fromJSON(inputjsonstr)
}


midjaNewRegression <- function(inputjsonstr) {
  timestart <- proc.time()

  inputjson <- fromJSON(inputjsonstr)
  dataset <- inputjson$dataset
  depVar <- inputjson$depVar
  indepVars <- inputjson$indepVars
  plotfilesdir <- inputjson$plotfilesdir
  unit_codes <- inputjson$unit_codes
  unit_type <- inputjson$unit_type

  # check nargs()
  if(is.null(dataset) || is.null(depVar) || is.null(indepVars)) {
    stop("must specify dataset, depVar indepVars!")
  }

  # check argument types
  if(!is.character(dataset) || !is.character(depVar) || !is.vector(indepVars) || !is.character(indepVars)) {
    stop("incorrect arguments specified!")
  }

  # SQL injection? check for ';' characters?
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user="postgres", password="1234321", dbname="cartodb_dev_user_6917ba11-1bf9-43b7-8929-a0585a8667e6_db", host="203.101.234.216", port=5432)

  #query <- paste("select", paste(xvar, yvar, "ra_name", sep=","), "from", dataset, "where iloc_code in(", paste0("'", iloc_codes, "'",collapse=",") , ")")
  query <- paste("select", paste(depVar, paste(indepVars, collapse=","), sep=","), "from", dataset, "where", paste(unit_type, "_code", sep=""), "in(", paste0("'", unit_codes, "'",collapse=",") , ")")
  #query <- paste("select", paste(depVar, paste(indepVars, collapse=","), sep=","), "from", dataset)
  res <- dbSendQuery(con, statement=query)
  rows <- fetch(res, n=-1)

  # disconnect from db, RPostgreSQL module does not support connection
  # pooling yet :-(
  dbDisconnect(con)

  fmla <- as.formula(paste(depVar, "~", paste(indepVars, collapse="+")))
  fit <- lm(fmla, data=rows)
  rsq <- summary(fit)$adj.r.squared

  # generate a formula from the model
  coffs <- fit$coefficients
  lmfmla <- as.formula(paste0(depVar, " ~ ", coffs[1], paste(sprintf("%+f*%s", coffs[-1], names(coffs[-1])), collapse="")))

  # convert formula to string and use the appropriate parts to contruct final equation
  chrtmp <- as.character(lmfmla)
  lmfmlastr <- paste(chrtmp[2], "=", chrtmp[3])

  # retrieve r-squared
  #lmsummary <- summary(fit)
  #adj_rsq <- lmsummary$adj.r.squared

	#plotfilesdir <- "/tmp/rplots"
	fname <- tempfile(tmpdir=plotfilesdir)
	pdffile <- paste(fname, ".pdf", sep="")
	pngfile <- paste(fname, ".png", sep="")
	ppi <- 100

	if(length(indepVars) == 1) {
		# y ~ x, generate a plot with a line
		cat("scatter plot with fitted line...\n")

		pdf(pdffile)
		plot(rows[[indepVars]], rows[[depVar]], xlab=indepVars, ylab=depVar)
		abline(fit, col="dark blue")
    title(main="Line fitting")
		dev.off()

		png(pngfile, width=6*ppi, height=6*ppi, res=ppi)
		plot(rows[[indepVars]], rows[[depVar]], xlab=indepVars, ylab=depVar)
		abline(fit, col="dark blue")
    title(main="Line fitting")
		dev.off()
	} else {
		# y ~ x1+x2+x3... , generate a plot of the R-squared value
		cat("bar plot...\n")
		ymax <- if(abs(1-rsq) < 0.5){ 1 }else{ rsq+0.1 }

		pdf(pdffile)
		bplot <- barplot(rsq, ylim=c(0,ymax), xlab=paste("Dependent variable:",depVar), ylab="Adjusted R-square")
		title(main="Goodness of fit")
		text(bplot, rsq, toString(signif(rsq,digits=3)), pos=3)
		dev.off()

		png(pngfile, width=6*ppi, height=6*ppi, res=ppi)
		bplot <- barplot(rsq, ylim=c(0,ymax), xlab=paste("Dependent variable:",depVar), ylab="Adjusted R-square")
		title(main="Goodness of fit")
		text(bplot, rsq, toString(signif(rsq,digits=3)), pos=3)
		dev.off()
	}

	#return(list(rows, fit, pdffile, pngfile))
	c(pdffile, pngfile)

  timeend <- proc.time()

  outlist <- list(pdf=basename(pdffile), png=basename(pngfile), equation=lmfmlastr, adj_rsquared=rsq, elapsed_time=(timeend-timestart)[[3]])
  outjson <- toJSON(outlist, collapse="")
}


midjaRegression <- function(depVar, indepVars) {
	# check nargs()
	if(nargs() != 2) {
		stop("must specify depVar and indepVars!")
	}

	# check argument types
	if(!is.character(depVar) || !is.vector(indepVars) || !is.character(indepVars)) {
		stop("incorrect arguments specified!")
	}

	# SQL injection? check for ';' characters?
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, user="postgres", password="foo", dbname="cartodb_dev_user_6917ba11-1bf9-43b7-8929-a0585a8667e6_db", host="localhost", port=5432)

	query <- paste("select", paste(depVar, paste(indepVars, collapse=","), sep=","), "from iloc_merged_dataset")
	res <- dbSendQuery(con, statement=query)
	rows <- fetch(res, n=-1)

	# disconnect from db, RPostgreSQL module does not support connection
	# pooling yet :-(
	dbDisconnect(con)

	fmla <- as.formula(paste(depVar, "~", paste(indepVars, collapse="+")))
	fit <- lm(fmla, data=rows)

	plotfilesdir <- "/tmp/rplots"
	fname <- tempfile(tmpdir=plotfilesdir)
	pdffile <- paste(fname, ".pdf", sep="")
	pngfile <- paste(fname, ".png", sep="")
	ppi <- 100

	if(length(indepVars) == 1) {
		# y ~ x, generate a plot with a line
		cat("scatter plot with fitted line...\n")

		pdf(pdffile)
		plot(rows[[indepVars]], rows[[depVar]])
		abline(fit, col="dark blue")
		dev.off()

		png(pngfile, width=6*ppi, height=6*ppi, res=ppi)
		plot(rows[[indepVars]], rows[[depVar]])
		abline(fit, col="dark blue")
		dev.off()
	} else {
		# y ~ x1+x2+x3... , generate a plot of the R-squared value
		cat("bar plot...\n")
		rsq <- summary(fit)$adj.r.squared
		ymax <- if(abs(1-rsq) < 0.5){ 1 }else{ rsq+0.1 }

		pdf(pdffile)
		bplot <- barplot(rsq, ylim=c(0,ymax), xlab=paste("Dependent variable:",depVar), ylab="Adjusted R-square")
		title(main="Goodness of fit")
		text(bplot, rsq, toString(signif(rsq,digits=3)), pos=3)
		dev.off()

		png(pngfile, width=6*ppi, height=6*ppi, res=ppi)
		bplot <- barplot(rsq, ylim=c(0,ymax), xlab=paste("Dependent variable:",depVar), ylab="Adjusted R-square")
		title(main="Goodness of fit")
		text(bplot, rsq, toString(signif(rsq,digits=3)), pos=3)
		dev.off()
	}

	#return(list(rows, fit, pdffile, pngfile))
	c(pdffile, pngfile)

  # generate a formula from the model
  coffs <- fit$coefficients
  lmfmla <- as.formula(paste0(depVar, " ~ ", coffs[1], paste(sprintf("%+f*%s", coffs[-1], names(coffs[-1])), collapse="")))

  # convert formula to string and use the appropriate parts to contruct final equation
  chrtmp <- as.character(lmfmla)
  lmfmlastr <- paste(chrtmp[2], "=", chrtmp[3])

  # retrieve r-squared
  lmsummary <- summary(fit)
  adj_rsq <- lmsummary$adj.r.squared

  outlist <- list(pdf=basename(pdffile), png=basename(pngfile), equation=lmfmlastr, adj_rsquared=rsq)
  outjson <- toJSON(outlist, collapse="")
}
