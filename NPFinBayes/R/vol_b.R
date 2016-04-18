#' Volatility of a Bond
#'
#' The function calculates the Volatility of a Bond. The Bond in question is assumed to be fixed rate and option-free in nature.
#' @param face_value Face Value of the Bond
#' @param ytm Yield to Maturity,
#' @param coupon_rate Coupon Payment Rate of the Bond,
#' @param period Period of Investment.
#' @return The volatility of the bond, subject to the current YTM.
#' @export
vol_b <- function(face_value=1000,
                  ytm,
                  coupon_rate,
                  period){
  duration <- macaulay_duration(face_value=face_value,
                                coupon_rate = coupon_rate,
                                int_rate = ytm,
                                period=period)$duration
  vol <- duration/(1+ytm)
  vol
}
