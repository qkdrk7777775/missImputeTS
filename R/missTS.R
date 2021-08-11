#' @title nonparametric missing value imputation by kalman smoothing, state space models and randomForest
#'
#' This R script contains the actual missImputeTS function.
#'
#' @author: changje Cho, qkdrk7777775@gmail.com
#'
#'@description Uses Kalman Smoothing on structural time series models and missForest for imputation. 'missImputeTS' is used to impute missing values, especially for mixed type time series data. It can be used to impute continuous and/or categorical data, including complex interactions and non-linear relationships. This yields an out-of-bag (OOB) imputation error estimate. You can also save computation time by running them in parallel.
#'
#'@param xmis = data matrix with missing values
#'@param time_var_name = time variable names. this column is equally spaced
#'@param maxiter = stop after how many iterations (default = 10)
#'@param ntree = how many trees are grown in the forest (default =100)
#'@param mtry = number of variables randomly sampled at each split. This argument is directly supplied to the 'randomForest' function. default value is sqrt(p) where p is the number of variables in 'xmis'.
#'@param variablewise = ddd
#'@param decreasing = dd
#'@param verbose = dd
#'@param replace = dd
#'@param classwt = dd
#'@param cutoff = dd
#'@param strata = dd
#'@param sampsize = dd
#'@param nodesize = dd
#'@param maxnodes = dd
#'@param xtrue = dd
#'@param parallelize =dd
#'@param ... = dd
#'@examples
#'data("EuStockMarkets2")
#'EuStockMarkets2$times=lubridate::round_date(EuStockMarkets2$times,'1d')
#'temp=dplyr::full_join(EuStockMarkets2,
#'data.frame(
#'  times=seq(
#'    min(EuStockMarkets2$times),
#'    max(EuStockMarkets2$times),
#'    3600*24)
#'    )
#'  )
#'
#'temp=temp[order(temp$times),]
#'
#'temp=data.frame(times=temp$times,
#'                sapply(temp[,-grep('times',colnames(temp))],
#'                function(x){zoo::na.approx(x,rule=2)}))
#'
#'for(i in 2:5){
#'  set.seed(i)
#'  temp[sample(1:nrow(temp),nrow(temp)*.1),i]=NA
#'}
#'
#'o=missTS(xmis=temp,time_var_name='times')
#'@importFrom  stats predict var
#'@importFrom randomForest randomForest
#'@importFrom iterators idiv
#'@import doMC foreach imputeTS itertools missForest
#'@export

missTS <- function(xmis,time_var_name, maxiter = 10, ntree = 100, variablewise = FALSE,
                       decreasing = FALSE, verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
                       classwt = NULL, cutoff = NULL, strata = NULL,
                       sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                       xtrue = NA, parallelize = c('no', 'variables', 'forests'),...){
  times=xmis[,time_var_name]
  columnIndex=colnames(xmis)
  n <- nrow(xmis)
  p <- ncol(xmis)-1
  if (!is.null(classwt)){
    stopifnot(length(classwt) == p, typeof(classwt) == 'list')
  }
  if (!is.null(cutoff)){
    stopifnot(length(cutoff) == p, typeof(cutoff) == 'list')
  }
  if (!is.null(strata)){
    stopifnot(length(strata) == p, typeof(strata) == 'list')
  }
  if (!is.null(nodesize)){
    stopifnot(length(nodesize) == 2)
  }
  if(length(unique(diff(xmis[,time_var_name])))!=1){
    stop('The time interval is different(time_var_name).')
  }

  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n)){
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[,-indCmis]
    p <- ncol(xmis)
    cat('  removed variable(s)', indCmis,
        'due to the missingness of all entries\n')
  }

  ## return feedback on parallelization setup
  parallelize <- match.arg(parallelize)
  if (parallelize %in% c('variables', 'forests')) {
    doMC::registerDoMC()
    if (foreach::getDoParWorkers() == 1) {
      stop("You must register a 'foreach' parallel backend to run 'missForest' in parallel. Set 'parallelize' to 'no' to compute serially.")
    } else if (verbose) {
      if (parallelize == 'variables') {
        cat("  parallelizing over the variables of the input data matrix 'xmis'\n")
      } else {
        cat("  parallelizing computation of the random forest model objects\n")
      }
    }
    if (getDoParWorkers() > p){
      stop("The number of parallel cores should not exceed the number of variables (p=", p, ")")
    }
  }
  ## perform initial S.W.A.G. on xmis (mean imputation)
  xmis=xmis[,setdiff(colnames(xmis),time_var_name)]
  ximp <- xmis
  varType <- character(p)
  for (t.co in which(sapply(xmis,is.numeric))){
    varType[t.co] <- 'numeric'
    #continuous missing data imputation with imputeTS
    ximp[is.na(xmis[,t.co]),t.co] <- imputeTS::na_kalman(xmis[,t.co])[is.na(xmis[,t.co])]
    next()
  }

  nonNumericVar=which(!sapply(xmis,is.numeric))
  fac_cols=nonNumericVar[setdiff(names(nonNumericVar),time_var_name)]
  if(length(fac_cols)!=0){
    for (t.co in fac_cols){
      varType[t.co] <- 'factor'
      idx_=stats::complete.cases(xmis)
      m=randomForest(x=xmis[idx_,-t.co],y=xmis[idx_,t.co])
      ximp[-idx_,t.co]=predict(m,ximp[-idx_,-t.co])
      next()
    }
  }

  ## extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar)        # indices of increasing amount of NA in vars
  if (decreasing){
    sort.j <- rev(sort.j)
  }
  sort.noNAvar <- noNAvar[sort.j]

  ## compute a list of column indices for variable parallelization
  nzsort.j <- sort.j[sort.noNAvar > 0]
  if (parallelize == 'variables'){
    '%cols%' <- get('%dopar%')
    idxList <- as.list(itertools::isplitVector(nzsort.j, chunkSize = getDoParWorkers()))
  }

  ## output
  Ximp <- vector('list', maxiter)

  ## initialize parameters of interest
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType

  ## setup convergence variables w.r.t. variable types
  if (k == 1){
    if (unique(varType) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('factor')
    }
    convergence <- c()
    OOBerr <- numeric(1)
  } else {
    names(convNew) <- c('numeric', 'factor')
    convergence <- matrix(NA, ncol = 2)
    OOBerr <- numeric(2)
  }

  ## function to yield the stopping criterion in the following 'while' loop
  stopCriterion <- function(varType, convNew, convOld, iter, maxiter){
    k <- length(unique(varType))
    if (k == 1){
      (convNew < convOld) & (iter < maxiter)
    } else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
    }
  }

  ## iterate missForest
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)){
    if (iter != 0){
      convOld <- convNew
      OOBerrOld <- OOBerr
    }
    if (verbose){
      cat("  missForest iteration", iter+1, "in progress...")
    }
    t.start <- proc.time()
    ximp.old <- ximp

    if (parallelize == "variables"){
      for (idx in idxList) {
        results <- foreach(varInd = idx, .packages = 'randomForest') %cols% {
          obsi <- !NAloc[, varInd] # which i's are observed
          misi <- NAloc[, varInd] # which i's are missing
          obsY <- ximp[obsi, varInd] # training response
          obsX <- ximp[obsi, seq(1, p)[-varInd]] # training variables
          misX <- ximp[misi, seq(1, p)[-varInd]] # prediction variables
          typeY <- varType[varInd]
          if (typeY == 'numeric'){
            RF <- randomForest(
              x = obsX,
              y = obsY,
              ntree = ntree,
              mtry = mtry,
              replace = replace,
              sampsize = if (!is.null(sampsize)){
                sampsize[[varInd]]
                }else if(replace){
                  nrow(obsX)
                  }else ceiling(0.632 * nrow(obsX)),
              nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
              maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
            ## record out-of-bag error
            oerr <- RF$mse[ntree]
            #           }
            ## predict missing values in column varInd
            misY <- predict(RF, misX)
          } else { # if Y is categorical
            obsY <- factor(obsY) ## remove empty classes
            summarY <- summary(obsY)
            if (length(summarY) == 1){ ## if there is only one level left
              oerr <- 0
              misY <- factor(rep(names(summarY), length(misi)))
            } else {
              RF <- randomForest(
                x = obsX,
                y = obsY,
                ntree = ntree,
                mtry = mtry,
                replace = replace,
                classwt = if (!is.null(classwt)) classwt[[varInd]] else
                  rep(1, nlevels(obsY)),
                cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
                  rep(1/nlevels(obsY), nlevels(obsY)),
                strata = if (!is.null(strata)) strata[[varInd]] else obsY,
                sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                  if (replace) nrow(obsX) else ceiling(0.632*nrow(obsX)),
                nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
                maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
              ## record out-of-bag error
              oerr <- RF$err.rate[[ntree,1]]
              #             }
              ## predict missing values in column varInd
              misY <- predict(RF, misX)
            }
          }
          list(varInd = varInd, misY = misY, oerr = oerr)
        }
        ## update the master copy of the data
        for (res in results) {
          misi <- NAloc[,res$varInd]
          ximp[misi, res$varInd] <- res$misY
          OOBerror[res$varInd] <- res$oerr
        }
      }
    } else { # if parallelize != "variables"
      for (s in 1 : p) {
        varInd <- sort.j[s]
        if (noNAvar[[varInd]] != 0) {
          obsi <- !NAloc[, varInd]
          misi <- NAloc[, varInd]
          obsY <- ximp[obsi, varInd]
          obsX <- ximp[obsi, seq(1, p)[-varInd]]
          misX <- ximp[misi, seq(1, p)[-varInd]]
          typeY <- varType[varInd]
          if (typeY == "numeric") {
            if (parallelize == 'forests') {
              xntree <- NULL
              RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()),
                            .combine = 'combine', .multicombine = TRUE,
                            .packages = 'randomForest') %dopar% {
                              randomForest( x = obsX,
                                            y = obsY,
                                            ntree = xntree,
                                            mtry = mtry,
                                            replace = replace,
                                            sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                              if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                            nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
                                            maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                            }
              ## record out-of-bag error
              OOBerror[varInd] <- mean((predict(RF) - RF$y) ^ 2, na.rm = TRUE)
              #               OOBerror[varInd] <- RF$mse[ntree]
            } else {
              RF <- randomForest( x = obsX,
                                  y = obsY,
                                  ntree = ntree,
                                  mtry = mtry,
                                  replace = replace,
                                  sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                    if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                  nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
                                  maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
              ## record out-of-bag error
              OOBerror[varInd] <- RF$mse[ntree]
            }
            misY <- predict(RF, misX)
          } else {
            obsY <- factor(obsY)
            summarY <- summary(obsY)
            if (length(summarY) == 1) {
              misY <- factor(rep(names(summarY), sum(misi)))
            } else {
              if (parallelize == 'forests') {
                RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()),
                              .combine = 'combine', .multicombine = TRUE,
                              .packages = 'randomForest') %dopar% {
                                randomForest(
                                  x = obsX,
                                  y = obsY,
                                  ntree = xntree,
                                  mtry = mtry,
                                  replace = replace,
                                  classwt = if (!is.null(classwt)) classwt[[varInd]] else
                                    rep(1, nlevels(obsY)),
                                  cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
                                    rep(1/nlevels(obsY), nlevels(obsY)),
                                  strata = if (!is.null(strata)) strata[[varInd]] else obsY,
                                  sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                    if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                  nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
                                  maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                              }
                ## record out-of-bag error
                ne <- as.integer(predict(RF)) != as.integer(RF$y)
                ne <- ne[! is.na(ne)]
                OOBerror[varInd] <- sum(ne) / length(ne)
              } else {
                RF <- randomForest(x = obsX,
                                   y = obsY,
                                   ntree = ntree,
                                   mtry = mtry,
                                   replace = replace,
                                   classwt = if (!is.null(classwt)) classwt[[varInd]] else
                                     rep(1, nlevels(obsY)),
                                   cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
                                     rep(1 / nlevels(obsY), nlevels(obsY)),
                                   strata = if (!is.null(strata)) strata[[varInd]] else obsY,
                                   sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
                                     if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
                                   nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
                                   maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
                ## record out-of-bag error
                OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
              }
              ## predict missing parts of Y
              misY <- predict(RF, misX)
            }
          }
          ximp[misi, varInd] <- misY
        }
      }
    }
    cat('done!\n')

    iter <- iter + 1
    Ximp[[iter]] <- ximp

    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)){
      t.ind <- which(varType == t.type)
      if (t.type == 'numeric'){
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2) / sum(ximp[, t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist / (n * sum(varType == 'factor'))
      }
      t.co2 <- t.co2 + 1
    }

    ## compute estimated imputation error
    if (!variablewise){
      NRMSE <- sqrt(mean(OOBerror[varType == 'numeric'])/
                      var(as.vector(as.matrix(xmis[, varType == 'numeric'])),
                          na.rm = TRUE))
      PFC <- mean(OOBerror[varType == 'factor'])
      if (k == 1){
        if (unique(varType) == 'numeric'){
          OOBerr <- NRMSE
          names(OOBerr) <- 'NRMSE'
        } else {
          OOBerr <- PFC
          names(OOBerr) <- 'PFC'
        }
      } else {
        OOBerr <- c(NRMSE, PFC)
        names(OOBerr) <- c('NRMSE', 'PFC')
      }
    } else {
      OOBerr <- OOBerror
      names(OOBerr)[varType == 'numeric'] <- 'MSE'
      names(OOBerr)[varType == 'factor'] <- 'PFC'
    }

    if (any(!is.na(xtrue))){
      err <- suppressWarnings(missForest::mixError(ximp, xmis, xtrue))
    }

    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))){
        cat("    error(s):", err, "\n")
      }
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((convNew<convOld)&(iter<maxiter)){

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    } else {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr, error = err)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
    } else {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld,
                  error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue)))
    }
  }
  out$ximp[,time_var_name]=times
  out$ximp=out$ximp[,columnIndex]
  class(out) <- 'missForest'
  return(out)
}

