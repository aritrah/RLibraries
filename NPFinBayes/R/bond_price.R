#' Bond Price Calculation
#'
#' This function calcuates the value of a bond as a function of the  face value, interest rate (YTM), coupon rate, and the period of investment.
#' @param face_value Face Value of the Bond, the default value is 1000,
#' @param int_rate Interest Rate,
#' @param coupon_rate Coupon Payment Rate of the Bond,
#' @param period Period of Investment.
#' @details The formula used for the calculation of the bond price is
#'
#' \deqn{B_p=c\frac{1-(\frac{1}{(1+i)^n}}{i}+\frac{M}{(1+i)}}
#' where c is the coupon payment, i is the interest rate, M is the face value, and n is the period of investment.
#' @examples bond_price(face_value=1000, int_rate=0.17, coupon_rate=0.16, period=6)
#' @return A list with two components
#'
#' \code{coupon_payment} the coupon payment subject to the interset rate.
#'
#' \code{bond_price} the bond price.
#' @references Fabozzi Frank J. and Mann Steven V., Handbook of Fixed Income Securities.
#' @export

# Bond Price Calculation Function
bond_price <- function(face_value=1000,
                       int_rate,
                       coupon_rate,
                       period){
  c <- coupon_rate*face_value # Coupon Payment
  # Bond Price
  b_p <- c*((1-1/(1+int_rate)^period))/(int_rate)+face_value/(1+int_rate)^period
  list(coupon_payment=c,bond_price=b_p)
}
