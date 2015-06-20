#' Internal Function To Clean ResponseTable
#'
#' This function takes a data.frame that has NA's in its first row from when it
#' got populated, removes that first row and returns a cleaned response, OR
#' tells you that the response was empty to begin with.
#'
#' @param responsetable A data.frame that needs to be cleaned.
#' @param rm.rows The rows to remove, if needed. Default is first row.
#'
#' @examples
#' ## negative response
#' sampleresponse <- data.frame(col1=NA, col2=NA)
#' cleanResponse(sampleresponse)
#'
#' ## positive response
#' sampleresponse <- rbind(sampleresponse, c(1, 2), c(3, 4))
#' cleanResponse(sampleresponse)
#' @export
#'

cleanResponse <- function(responsetable, rm.rows=0){

  if(nrow(responsetable) == 0){
    responsetable <- "Sorry, nothing was found."
  } else if(nrow(responsetable) == 1 & rm.rows != 0){
    responsetable <- "Sorry, nothing was found."
  } else{
    if(rm.rows != 0){
      responsetable <- responsetable[-rm.rows,]
    }
      rownames(responsetable) <- 1:nrow(responsetable)
  }
  return(responsetable)
}
