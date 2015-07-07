#' Rotate Plot To NSEW Orientation
#'
#' This function takes a plot that is not oriented at true North/South and
#' rotates it to be correct.
#'
#' @param df A data.frame containing an "x" and a "y" column, representing
#'   coordinates. Other information about the data.frame (extra columns) should
#'   be retained.
#' @param origin A vector of two numbers representing the desired (x,y)
#'   coordinates of the desired origin of the resulting dataframe. This defaults
#'   to 0,0, meaning that the left/bottom-most point will be at (0,0)
#' @param rnd A rounding value, defaults to 0, for the new (x,y) coordinates.
#' @param truesw The most southwestern point of the plot, even if it is farther
#'   out than any of the trees.
#'
#' @return This function returns the data.frame from "x", with modified x and y
#'   columns.
#' @examples
#' rotatePlot(data.frame(x=c(0,4), y=c(0,-3), stringsAsFactors=FALSE))
#'
#' @export


rotatePlot <- function(df, origin=c(0,0), rnd=0, truesw=c(0,0)){

  if(!is.numeric(df$x) | !is.numeric(df$y)){
    stop("Sorry, x and y columns are not numeric or do not exist.")
  }

  leftmost <- df[df$x==min(df$x), c("x", "y") ]
  bottommost <- df[df$y==min(df$y), c("x", "y")]

  if(leftmost[1, "y"] == bottommost[1, "y"]){
    stop("Sorry, your plot is already aligned correctly. leftmost y == bottommost y. If you believe this is an incorrect error, you may have repeat bottommost y values.")
  }

  ## if truesw is set, that represents a different origin point.
  if(truesw==c(0,0)){
    conversion <- findRotation(leftmost[1,"x"],
                               leftmost[1,"y"],
                               bottommost[1,"x"],
                               bottommost[1,"y"])
  } else{
    conversion <- findRotation(truesw[1],
                               truesw[2],
                               bottommost[1,"x"],
                               bottommost[1, "y"])
  }

 ##save leftmost by setting origin slightly to left of leftmost point

 polardf <- cart2polar(leftmost[1,1]-1, leftmost[1,2]-1, df$x, df$y)
 polardf$theta <- polardf$theta+conversion
 newdf <- polar2cart(origin[1], origin[2], polardf$r, polardf$theta)

 df$x <- round(newdf$x, rnd)
 df$y <- round(newdf$y, rnd)
return(df)

}
