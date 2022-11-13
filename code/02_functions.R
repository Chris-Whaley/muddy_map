# 
# Muddy algorithm uses hue, saturation, and lightness to determine county color.
# In order to not skew map color with highly populated counties, an upper limit
# of 59,828 is used. 
# 
#   1. hue: party color
#     a. democrat = blue. hue = 240
#     b. republican = red. hue = 0
#   2. saturation: vote margin
#     a. absolute vote count margin. (difference / total votes)
#   3. lightness: number of total votes
#     a. if county vote total >= upper fence, then upper fence. lightness = 50%
#     b. else lightness is scaled between 50%-100%
#
# hsl and rgb functions are used to convert HSL (hue-saturation-lightness) to
#   RGB (red-green-blue) for mapping coloring. reverse functionality applied
#   for additional support.


# calculates hue based on party
hue <- function(party_winner){
  ifelse(party_winner == "democrat", 240, 0)
}


# calculates saturation based on vote margin
saturation <- function(dem_votes, rep_votes){
  total_votes = sum(dem_votes, rep_votes)
  
  abs(dem_votes - rep_votes) / total_votes
}


# calculates lightness based on total vote amount
lightness <- function(total_votes){
  # if county vote total >= upper fence then 50% lightness. 
  # lower vote total = higher lightness
  UPPERFENCE = 59828  # statistical upperfence
  tot_votes = if_else(total_votes >= UPPERFENCE, UPPERFENCE, total_votes)
  (
    ( (( 1 - ((tot_votes)/UPPERFENCE) ) * 100 ) / 2 ) + 50
  ) * .01
  
}


###### Color Conversions
# specify h as whole input degrees (e.g 0-360)
# s = 0.0 - 1 (0 - 100%)
# l = 0.0 - 1, (0 - 100%)
# returns output from R's rgb() function
# source: https://stackoverflow.com/questions/28562288/how-to-use-the-hsl-hue-saturation-lightness-cylindric-color-model

hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }
  return(rgb(r,g,b))
}

# r, g, b = 0.0 - 1 (0 - 100%)
# returns h/s/l in a vector, h = 0-360 deg, s = 0.0 - 1 (0-100%), l = 0.0 - 1 (0-100%)
rgb_to_hsl <- function(r, g, b) {
  val_max <- max(c(r, g, b))
  val_min <- min(c(r, g, b))
  h <- s <- l <- (val_max + val_min) / 2
  if (val_max == val_min){
    h <- s <- 0
  } else {
    d <- val_max - val_min
    s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))
    if (val_max == r) { h <- (g - b) / d + (ifelse(g < b, 6, 0)) }
    if (val_max == g) { h <- (b - r) / d + 2 }
    if (val_max == b) { h <- (r - g) / d + 4 }
    h <- (h / 6) * 360
  }
  return(c(h=h, s=s, l=l))
}
