#' Plot a MODFLOW head predictions file
#' 
#' @param hpr head predictions file object
#' @param type plot type: 'scatter' or 'residual'
#' @method rmf_plot hpr
#' @export
rmf_plot.hpr <- function(hpr,type='scatter') {
  dat <- data.frame(SIMULATED.EQUIVALENT=hpr$SIMULATED.EQUIVALENT, OBSERVED.VALUE=hpr$OBSERVED.VALUE,OBSERVATION.NAME=hpr$OBSERVATION.NAME)[which(hpr$SIMULATED.EQUIVALENT!=-888),]
  if(type=='scatter') {
    return(  ggplot(dat,aes(x=OBSERVED.VALUE,y=SIMULATED.EQUIVALENT))+
               geom_point(aes(colour=abs(OBSERVED.VALUE-SIMULATED.EQUIVALENT)))+
               geom_abline(aes(intercept=0,slope=1),linetype='dashed')+
               scale_colour_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observed value')+ylab('Simulated equivalent')
    )
  } else if(type=='residual') {
    return(  ggplot(dat,aes(x=OBSERVATION.NAME,y=SIMULATED.EQUIVALENT-OBSERVED.VALUE))+
               geom_bar(aes(fill=abs(OBSERVED.VALUE-SIMULATED.EQUIVALENT)),stat='identity')+
               scale_fill_gradientn('Misfit',colours=rev(rainbow(7)),trans='log10')+
               xlab('Observation name')+ylab('Residuals (simulated - observed)')
    )
  }
}

#' @describeIn rmf_plot.hpr Deprecated function name
#' @export
plot.hpr <- function(...) {
  .Deprecated(new = "rmf_plot.hpr", old = "plot.hpr")
  rmf_plot.hpr(...)
}
