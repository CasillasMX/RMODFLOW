#' Read a MODFLOW head file
#' 
#' \code{read_hed} reads in a MODFLOW head file and returns it as an \code{\link{RMODFLOW}} hed object.
#' 
#' @param file filename; typically '*.hed'
#' @param dis discretization file object
#' @param bas basic file object
#' @param huf huf object; optional; provide only if huf heads are being read
#' @param convert_hnoflo_to_NA logical; should hnoflo values be converted to NA?
#' @return object of class hed
#' @importFrom readr read_lines
#' @export
rmf_read_hed <- function(file = {cat('Please select hed file ...\n'); file.choose()},
                         dis = {cat('Please select dis file ...\n'); rmf_read_dis(file.choose())},
                         bas = {cat('Please select bas file ...\n'); rmf_read_bas(file.choose(), dis = dis)},
                         huf = NULL,
                         convert_hnoflo_to_NA=TRUE,
                         binary = TRUE,
                         precision = 'single') {
  if(binary) {
    if(!is.null(huf)) {
      dis$nlay <- huf$nhuf
    }
    real_number_bytes <- ifelse(precision == 'single', 4, 8)
    con <- file(file,open='rb')
    hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, sum(dis$nstp)))
    attr(hed, 'kstp') <- attr(hed, 'kper') <- attr(hed, 'pertim') <- attr(hed, 'totim') <- attr(hed, 'desc') <- attr(hed, 'ncol') <- attr(hed, 'nrow') <- attr(hed, 'ilay') <- NULL
    
    kstp <- readBin(con,what='integer',n=1)
    kper <- readBin(con,what='integer',n=1)
    pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
    totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
    desc <- readChar(con,nchars=16)
    if(! desc %in% c('            HEAD',
                     '        DRAWDOWN',
                     '      SUBSIDENCE',
                     '      COMPACTION',
                     '   CRITICAL HEAD',
                     '     HEAD IN HGU',
                     'NDSYS COMPACTION',
                     '  Z DISPLACEMENT',
                     ' D CRITICAL HEAD',
                     'LAYER COMPACTION',
                     ' DSYS COMPACTION',
                     'ND CRITICAL HEAD',
                     'LAYER COMPACTION',
                     'SYSTM COMPACTION',
                     'PRECONSOL STRESS',
                     'CHANGE IN PCSTRS',
                     'EFFECTIVE STRESS',
                     'CHANGE IN EFF-ST',
                     '      VOID RATIO',
                     '       THICKNESS',
                     'CENTER ELEVATION',
                     'GEOSTATIC STRESS',
                     'CHANGE IN G-STRS')) {
      close(con)
      stop('Array description not recognized. Is the file really binary? If so, you could try double precision. If not, set the binary argument to FALSE.')
    }
    while(length(desc != 0)) {
      ncol <- readBin(con, what = 'integer', n = 1)
      nrow <- readBin(con, what = 'integer', n = 1)
      ilay <- readBin(con, what = 'integer', n = 1)
      stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      hed[,,ilay,stp_nr] <- aperm(array(readBin(con,what='numeric',n = ncol * nrow, size = real_number_bytes),dim=c(ncol, nrow)), c(2, 1))
      attr(hed, 'kstp')[stp_nr] <- kstp
      attr(hed, 'kper')[stp_nr] <- kper
      attr(hed, 'pertim')[stp_nr] <- pertim
      attr(hed, 'totim')[stp_nr] <- totim
      attr(hed, 'desc')[stp_nr] <- desc
      attr(hed, 'ncol')[stp_nr] <- ncol
      attr(hed, 'nrow')[stp_nr] <- nrow
      attr(hed, 'ilay')[stp_nr] <- ilay
      kstp <- readBin(con,what='integer',n=1)
      kper <- readBin(con,what='integer',n=1)
      pertim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      totim <- readBin(con,what='numeric',n = 1, size = real_number_bytes)
      desc <- readChar(con,nchars=16)
    }
    no_data <- which(is.na(attr(hed, 'kstp')))
    if(length(no_data) != 0) {
      hed <- hed[,,,-no_data]
      attr(hed, 'kstp') <- attr(hed, 'kstp')[-no_data]
      attr(hed, 'kper') <- attr(hed, 'kper')[-no_data]
      attr(hed, 'pertim') <- attr(hed, 'pertim')[-no_data]
      attr(hed, 'totim') <- attr(hed, 'totim')[-no_data]
      attr(hed, 'desc') <- attr(hed, 'desc')[-no_data]
      attr(hed, 'ncol') <- attr(hed, 'ncol')[-no_data]
      attr(hed, 'nrow') <- attr(hed, 'nrow')[-no_data]
      attr(hed, 'ilay') <- attr(hed, 'ilay')[-no_data]
    }
    close(con)
  } else {
    hed.lines <- readr::read_lines(file)
    hed <- array(NA, dim = c(dis$nrow, dis$ncol, dis$nlay, sum(dis$nstp)))
    attr(hed, 'kstp') <- attr(hed, 'kper') <- attr(hed, 'pertim') <- attr(hed, 'totim') <- attr(hed, 'desc') <- attr(hed, 'ncol') <- attr(hed, 'nrow') <- attr(hed, 'ilay') <- NULL
    
    while(length(hed.lines) != 0) {
      variables <- rmfi_remove_empty_strings(strsplit(hed.lines[1],' ')[[1]])
      kstp <- as.numeric(variables[1])
      kper <- as.numeric(variables[2])
      pertim <- as.numeric(variables[3])
      totim <- as.numeric(variables[4])
      desc <- variables[5]
      if(! desc %in% c('HEAD',
                       'DRAWDOWN',
                       'SUBSIDENCE',
                       'COMPACTION',
                       'CRITICAL HEAD', # spaces! fix!
                       'HEAD IN HGU',
                       'NDSYS COMPACTION',
                       'Z DISPLACEMENT',
                       'D CRITICAL HEAD',
                       'LAYER COMPACTION',
                       'DSYS COMPACTION',
                       'ND CRITICAL HEAD',
                       'LAYER COMPACTION',
                       'SYSTM COMPACTION',
                       'PRECONSOL STRESS',
                       'CHANGE IN PCSTRS',
                       'EFFECTIVE STRESS',
                       'CHANGE IN EFF-ST',
                       'VOID RATIO',
                       'THICKNESS',
                       'CENTER ELEVATION',
                       'GEOSTATIC STRESS',
                       'CHANGE IN G-STRS')) {
        stop('Array description not recognized. Are you sure the file is not binary? If so, make sure you added the keyword LABEL in the output control file.')
      }
      ncol <- as.numeric(variables[6])
      nrow <- as.numeric(variables[7])
      ilay <- as.numeric(variables[8])
      stp_nr <- ifelse(kper==1,kstp,cumsum(dis$nstp)[kper-1]+kstp)
      hed.lines <- hed.lines[-1]
      data_set <- rmfi_parse_array(hed.lines,nrow,ncol,1, skip_header = TRUE)
      hed[,,ilay,stp_nr] <- data_set$array
      hed.lines <- data_set$remaining_lines
      attr(hed, 'kstp')[stp_nr] <- kstp
      attr(hed, 'kper')[stp_nr] <- kper
      attr(hed, 'pertim')[stp_nr] <- pertim
      attr(hed, 'totim')[stp_nr] <- totim
      attr(hed, 'desc')[stp_nr] <- desc
      attr(hed, 'ncol')[stp_nr] <- ncol
      attr(hed, 'nrow')[stp_nr] <- nrow
      attr(hed, 'ilay')[stp_nr] <- ilay
    }
    no_data <- which(is.na(attr(hed, 'kstp')))
    if(length(no_data != 0)) {
      hed <- hed[,,,-no_data]
      attr(hed, 'kstp') <- attr(hed, 'kstp')[-no_data]
      attr(hed, 'kper') <- attr(hed, 'kper')[-no_data]
      attr(hed, 'pertim') <- attr(hed, 'pertim')[-no_data]
      attr(hed, 'totim') <- attr(hed, 'totim')[-no_data]
      attr(hed, 'desc') <- attr(hed, 'desc')[-no_data]
      attr(hed, 'ncol') <- attr(hed, 'ncol')[-no_data]
      attr(hed, 'nrow') <- attr(hed, 'nrow')[-no_data]
      attr(hed, 'ilay') <- attr(hed, 'ilay')[-no_data]
    }
  }
  class(hed) <- c('hed','rmf_4d_array')
  if(convert_hnoflo_to_NA) hed[which(hed==bas$hnoflo)] <- NA
  return(hed)
}

#' @describeIn rmf_read_hed Deprecated function name
#' @export
read_hed <- function(...) {
  .Deprecated(new = "rmf_read_hed", old = "read_hed")
  rmf_read_hed(...)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_fhd <- function(...) {
  rmf_read_hed(..., binary = FALSE)
}

#' @describeIn rmf_read_hed Compatible with default ModelMuse file extension
#' @export
rmf_read_bhd <- function(...) {
  rmf_read_hed(..., binary = TRUE)
}
