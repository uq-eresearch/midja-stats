library(RPostgreSQL)
library(RJSONIO)


midjaSquare <- function(x) {
	x * x
}

midjaJson <- function(inputjsonstr) {
  inputjson <- fromJSON(inputjsonstr)
}

midjaNewScatterplot <- function(inputjsonstr) {
  timestart <- proc.time()

  inputjson <- fromJSON(inputjsonstr)
  dataset <- inputjson$dataset
  xvar <- inputjson$xvar
  xlabel <- inputjson$xlabel
  yvar <- inputjson$yvar
  ylabel <- inputjson$ylabel
  useRemoteness <- inputjson$useRemoteness
  labelLocations <- inputjson$labelLocations
  plotfilesdir <- inputjson$plotfilesdir
  iloc_codes <- inputjson$iloc_codes

  # check nargs()
  if(is.null(dataset) || is.null(xvar) || is.null(yvar)) {
    stop("must specify dataset, xvar,  yvar!")
  }

  # check argument types
  if(!is.character(dataset) || !is.character(xvar) || !is.character(yvar)) {
    stop("incorrect arguments specified!")
  }

  # SQL injection? check for ';' characters?
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user="publicuser", password="public", dbname="cartodb_dev_user_6917ba11-1bf9-43b7-8929-a0585a8667e6_db", host="localhost", port=5432)

  query <- paste("select", paste(xvar, yvar, "ra_name", "iloc_name", sep=","), "from", dataset, "where iloc_code in(", paste0("'", iloc_codes, "'",collapse=",") , ")")
  #query <- paste("select", paste(xvar, yvar, "ra_name", sep=","), "from", dataset)
  #query <- paste("select", paste(depVar, paste(indepVars, collapse=","), sep=","), "from", dataset)
  res <- dbSendQuery(con, statement=query)
  rows <- fetch(res, n=-1)

  # disconnect from db, RPostgreSQL module does not support connection
  # pooling yet :-(
  dbDisconnect(con)

  library(ggplot2)
  #ggplot(rows, aes_string(x=xvar, y=yvar)) + geom_point(aes(col=ra_name)) + theme(legend.position="bottom", legend.direction="vertical")
  #gplot(rows, aes_string(x=xvar, y=yvar)) + geom_point(aes(col=ra_name),pch=19,alpha=0.8) + theme(legend.position="bottom", legend.direction="vertical")

  rareas <- c("Major Cities of Australia", "Inner Regional Australia", "Outer Regional Australia", "Remote Australia", "Very Remote Australia")
  collevels <- factor(rows$ra_name, levels=rareas)

  if(labelLocations) {
    myplot <- ggplot(rows, aes_string(x=xvar, y=yvar, label="iloc_name"), environment=environment()) + theme(legend.position="none")
    if(useRemoteness) {
      # if remoteness is also enabled, colour the text according to it
      myplot <- myplot + geom_text(size=2, hjust=-0.1, vjust=-0.1,aes(color=collevels))
    } else {
      # all text labels are the same colour
      myplot <- myplot + geom_text(size=2, hjust=-0.1, vjust=-0.1)
    }
  } else {
    myplot <- ggplot(rows, aes_string(x=xvar, y=yvar), environment=environment())
  }

  if(useRemoteness) {
    myplot <- myplot + geom_point(aes(col=collevels),pch=19,alpha=0.8) + theme(legend.position="bottom", legend.direction="vertical") + scale_color_discrete("Remoteness Area")
    #myplot <- myplot + geom_point(aes(col=ra_name),pch=19,alpha=0.8) + theme(legend.position="bottom", legend.direction="vertical") + scale_color_discrete("Remoteness Area")
  } else {
    myplot <- myplot + geom_point(pch=19, col="#2211bb", alpha=0.8, cex=3)
  }

  if(!is.null(xlabel)) {
    myplot <- myplot + xlab(xlabel)
  }
  if(!is.null(ylabel)) {
    myplot <- myplot + ylab(ylabel)
  }

  fname <- tempfile(tmpdir=plotfilesdir)
  pdffile <- paste(fname, ".pdf", sep="")
  pngfile <- paste(fname, ".png", sep="")
  ppi <- 100

  ggsave(pdffile)
  ggsave(pngfile, width=8, height=8, dpi=ppi)

  timeend <- proc.time()

  outlist <- list(pdf=basename(pdffile), png=basename(pngfile), elapsed_time=(timeend-timestart)[[3]])
  outjson <- toJSON(outlist, collapse="")
}


midjaNewRegression <- function(inputjsonstr) {
  timestart <- proc.time()

  inputjson <- fromJSON(inputjsonstr)
  dataset <- inputjson$dataset
  depVar <- inputjson$depVar
  indepVars <- inputjson$indepVars
  plotfilesdir <- inputjson$plotfilesdir

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
  con <- dbConnect(drv, user="publicuser", password="public", dbname="cartodb_dev_user_6917ba11-1bf9-43b7-8929-a0585a8667e6_db", host="localhost", port=5432)

  query <- paste("select", paste(depVar, paste(indepVars, collapse=","), sep=","), "from", dataset)
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
	con <- dbConnect(drv, user="publicuser", password="public", dbname="cartodb_dev_user_6917ba11-1bf9-43b7-8929-a0585a8667e6_db", host="localhost", port=5432)

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
