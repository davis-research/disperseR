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
#' @param method The method to calculate the plot rotation. Accepts
#'   "bottomleft", "topright", and "plotcorner"; see Note for details.
#' @param returntruesw If set to true, this function will return the "truesw"
#'   points, converted to the new plot space, instead of the entire data.frame.
#' @param manualconversion Defaults to 0; you can set this to a manual value to
#'   add to the plot conversion, if there are issues with the default options.
#'
#' @return This function returns the data.frame from "x", with modified x and y
#'   columns.
#'
#' @note This function has a few different ways to rotate the plot. The default
#'   method, "plot corner", takes the "truesw" value and uses that as the first
#'   point to get rotation angle, with the bottommost point as the second point.
#'   The "bottomleft" method takes the rotation angle from the angle between the
#'   leftmost point and the bottommost point. The "topright" method takes the
#'   rotation angle from the angle between the topmost and rightmost point.
#' @examples
#' rotatePlot(data.frame(x=c(0,4), y=c(0,-3), stringsAsFactors=FALSE))
#'
#' @export


rotatePlot <- function(df, origin=c(0,0), rnd=2, truesw=c(0,0), method="plotcorner", returntruesw=FALSE, manualconversion=0){

  if(!is.numeric(df$x) | !is.numeric(df$y)){
    stop("Sorry, x and y columns are not numeric or do not exist.")
  }

  ## define variables to be used in all possible methods
  leftmost <- df[df$x==min(df$x), c("x", "y")]
  bottommost <- df[df$y==min(df$y), c("x", "y")]
  rightmost <- df[df$x==max(df$x), c("x", "y")]
  topmost <- df[df$y==max(df$y), c("x", "y")]

if(method=="plotcorner"){
    conversion <- findRotation(truesw[1],
                               truesw[2],
                               bottommost[1,"x"],
                               bottommost[1,"y"])


 polardf <- cart2polar(truesw[1], truesw[2], df$x, df$y)
 polardf$theta <- polardf$theta+conversion+90+manualconversion
 newdf <- polar2cart(origin[1], origin[2], polardf$r, polardf$theta)
 returnedsw <- c(origin[1], origin[2])
}

  ## if we need to do it by leftmost bottommost
if(method=="bottomleft"){
  conversion <- findRotation(leftmost[1, "x"],
                             leftmost[1, "y"],
                             bottommost[1, "x"],
                             bottommost[1, "y"])
  polardf <- cart2polar(leftmost[1, "x"], leftmost[1, "y"], df$x, df$y)
  polardf$theta <- polardf$theta+conversion+90+manualconversion
  newdf <- polar2cart(origin[1], origin[2], polardf$r, polardf$theta)

  tswdf <- cart2polar(leftmost[1, "x"], leftmost[1, "y"], truesw[1], truesw[2])
  tswdf$theta <- tswdf$theta + conversion + 90 + manualconversion
  returnedsw <- polar2cart(origin[1], origin[2], tswdf$r, tswdf$theta)
}

if(method=="topright"){
    conversion <- findRotation(topmost[1, "x"],
                               topmost[1, "y"],
                               rightmost[1, "x"],
                               rightmost[1, "y"])
    polardf <- cart2polar(topmost[1, "x"], topmost[1, "y"], df$x, df$y)
    polardf$theta <- polardf$theta+conversion+90+manualconversion
    newdf <- polar2cart(origin[1], origin[2], polardf$r, polardf$theta)

    tswdf <- cart2polar(topmost[1, "x"], topmost[1, "y"], truesw[1], truesw[2])
    tswdf$theta <- tswdf$theta + conversion + 90 + manualconversion
    returnedsw <- polar2cart(origin[1], origin[2], tswdf$r, tswdf$theta)
}


 df$x <- round(newdf$x, rnd)
 df$y <- round(newdf$y, rnd)

 if(returntruesw==FALSE){
   return(df)
 } else{
   return(returnedsw)
 }

}
