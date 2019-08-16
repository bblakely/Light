#Import function for CS

importCSdata <- function(filename,RetOpt="data"){
  if(RetOpt=="info"){
    # bring in entire header of CSI TOA5 data file for metadata
    stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
    return(stn.info)
  } else {
    # second line of header contains variable names
    header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
    # bring in data
    stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
    names(stn.data) <- header
    # add column of R-formatted date/timestamps
    stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
    return(stn.data)}
}

