#' Run a MODFLOW model response surface mapping
#' 
#' \code{run_modflow_rsm} runs a MODFLOW response surface mapping.
#' 
#' @param file path to name file; typically '*.nam'
#' @param executable name of the MODFLOW executable to use
#' @param par central parameter values (for all or only included parameters); current parameter value file values are used if par is not provided
#' @param include logical vector indicating which parameters in the parameter value file to include in the mapping
#' @param trans vector of transformations; currently only 'log' is supported
#' @param lower lower parameter bounds
#' @param upper upper parameter bounds
#' @param n number of intervals sampled for each parameter
#' @return an rsm object with the full list of parameters and the response value
#' @export
rmf_run_rsm <- function(file,executable='mf2005',par=NULL,include=NULL, trans=NULL, lower, upper, n)
{
  dir <- dirname(file)
  file <- basename(file)
  nam <- rmf_read_nam(paste0(dir,'/',file))
  pvl <- rmf_read_pvl(paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
  hob <- rmf_read_hob(paste0(dir,'/',nam$fname[which(nam$ftype=='HOB')]))
  if(is.null(par)) par <- pvl$parval
  if(is.null(include)) include <- rep(TRUE,length(par))
  if(length(par)!=length(pvl$parval)) 
  {
    par2 <- par
    par <- pvl$parval
    par[which(include)] <- par2
  }
  if(!is.null(trans))
  {
    par[which(trans=='log')] <- log(par[which(trans=='log')])
    lower[which(trans=='log')] <- log(lower[which(trans=='log')])
    upper[which(trans=='log')] <- log(upper[which(trans=='log')])
  }

  # create parameter sets to run
    parameter_values <- list()
    for(i in 1:sum(include)) parameter_values[[i]] <- seq(lower[include][i],upper[include][i],length=n)
    rsm <- expand.grid(parameter_values)
    rsm$rmse <- NA
  
  # run parameter sets and get RMSE
    rsm_modflow <- function(par_include)
    {
      pvl$parval <- par
      pvl$parval[which(include)] <- as.numeric(par_include)
      if(!is.null(trans)) pvl$parval[which(trans=='log')] <- exp(pvl$parval[which(trans=='log')])
      rmf_write_pvl(pvl, file=paste0(dir,'/',nam$fname[which(nam$ftype=='PVAL')]))
      rmf_run_modflow(paste0(dir,'/',file),executable)
      rmse <- rmf_performance(rmf_read_hpr(paste0(dir,'/',nam$fname[which(nam$nunit==hob$iuhobsv)])))$rmse
      cat(paste('\n RMSE=',format(rmse,scientific=TRUE,digits=4),'parval=',paste(format(pvl$parval[include],scientific=TRUE,digits=4),collapse=' '),'\n')) # file=report, append=T
      return(rmse)
    }
    for(i in 1:nrow(rsm))
    {
      rsm$rmse[i] <- rsm_modflow(rsm[i,1:sum(include)])
    }
  names(rsm) <- c(paste0(trans,pvl$parnam)[include],'rmse')
  # add attributes later
  class(rsm) <- c('rsm','data.frame')
  return(rsm)
}

#' @describeIn rmf_run_rsm Deprecated function name
#' @export
run_rsm <- function(...) {
  .Deprecated(new = "rmf_run_rsm", old = "run_rsm")
  rmf_run_rsm(...)
}
