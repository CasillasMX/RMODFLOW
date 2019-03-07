#' Get model performance measures from a hpr object
#' 
#' @param hpr head predictions file object
#' @return performance measures
#'
#' @rdname rmf_performance
#' @method rmf_performance hpr
#' @export
rmf_performance.hpr <- function(hpr) {
  obsAndSims <- data.frame(SIMULATED.EQUIVALENT=hpr$SIMULATED.EQUIVALENT, OBSERVED.VALUE=hpr$OBSERVED.VALUE,OBSERVATION.NAME=hpr$OBSERVATION.NAME)[which(hpr$SIMULATED.EQUIVALENT!=-888),]
  observations <- obsAndSims$OBSERVED.VALUE
  predictions <- obsAndSims$SIMULATED.EQUIVALENT
  dry <- 0; if(-888 %in% predictions) dry <- length(which(predictions == -888))
  if(dry > 0) predictions <- predictions[-which(predictions == -888)]
  names <- obsAndSims$OBSERVATION.NAME
  perform <- rmfi_performance_measures(observations,predictions)
  perform$rmse <- sqrt(perform$mse)
#  notRoundedPerformance <- perform
  perform <- round(perform,2)
  return(perform)
}


