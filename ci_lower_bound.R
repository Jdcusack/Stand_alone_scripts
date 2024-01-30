
#' This function calculates the lower confidence interval that a sample is representative.
#' Imagine that you are looking at two different samples and you want to see how often 
#' a binary event takes place:
#' in sample a you test to see how often event foo happens
#' you take 100 samples and find that characteristic foo is present in 80 of your 100 tests,
#' in sample b you test to see how often characteristic bar is present.
#' you take 100,000 samples and find that event bar occurs 80,000 times.
#' 
#' If you look at just the rate/100 events, sample a and sample b are the same.
#' But that does not mean that you can be equally confident that foo and 
#' bar both appear 80% of the time. 
#' 
#' Variation within populations mean that no sample is perfectly representative 
#' but larger samples are like to be more accurate so the "true value" will be 
#' within a smaller range
#' 
#' So if you took a second sample of 100 people you would be able to say:
#' "I am 95% confident that foo will be present in between x and y of the 
#' of the people sampled"
#' 
#' The ci_lower_bound function calculates the lower end of the x and y range. 
#' The purpose is to allow for better comparisons between samples of different 
#' sizes. For example if you are looking at the number of people discharged 
#' after their first appointment in a speciality. When you do that you need to 
#' find a way to account for different sizes of hospital, who will have different
#' list sizes for the same speciality.
#' 
#' This function takes in the following arguments:
#'@param pos an integer value giving the number of "successes", that is how often 
#'            you found the thing you were looking for when you took your sample          
#'@param n an integer value stating the number of samples you took
#'@param confidence a floating point number between 0 and 1 that sets the 
#'                  confidence interval you are testing against, that is how 
#'                  accurate you want your lower bound to be. 0.95 would give a 
#'                  confidence interval of 95%
#'                  
#' @returns The lower confidence limit for the sample based on the 
#'          confidence interval set above. This is the "x" if you were to say 
#'          something like "I'm 95% sure the number is as low as x or as high
#'          as y"
#'                  
#'This function is intended to be an R equivalent to the Ruby code provided here:
#'https://www.evanmiller.org/how-not-to-sort-by-average-rating.html
#'with credit to Evan Miller


ci_lower_bound <- function(pos, n, confidence) {
  if (n == 0) {
    return(0)
  }
  
  z <- qnorm(1 - (1 - confidence) / 2)
  phat <- 1.0 * pos / n
  
  (phat + z^2 / (2 * n) - z * sqrt((phat * (1 - phat) + z^2 / (4 * n)) / n)) / (1 + z^2 / n)
}
