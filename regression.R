library(RJSONIO)

midjaRegression <- function(inputjsonstr) {
  timestart <- proc.time()

  inputjson <- fromJSON(inputjsonstr)
  depVar <- inputjson$depVar
  indepVars <- inputjson$indepVars
	data <- inputjson$data

  # check nargs()
  if(is.null(data) || is.null(depVar) || is.null(indepVars)) {
    stop("must specify dataset, depVar & indepVars!")
  }

  fmla <- as.formula(paste(depVar, "~", paste(indepVars, collapse="+")))
  fit <- lm(fmla, data=data)
  rsq <- summary(fit)$adj.r.squared

  # generate a formula from the model
  coffs <- fit$coefficients
  lmfmla <- as.formula(paste0(depVar, " ~ ", coffs[1], paste(sprintf("%+f*%s", coffs[-1], names(coffs[-1])), collapse="")))

  # convert formula to string and use the appropriate parts to contruct final equation
  chrtmp <- as.character(lmfmla)
  lmfmlastr <- paste(chrtmp[2], "=", chrtmp[3])

  timeend <- proc.time()

  outlist <- list(equation=lmfmlastr, adj_rsquared=rsq, elapsed_time=(timeend-timestart)[[3]])
  outjson <- toJSON(outlist, collapse="")
}
