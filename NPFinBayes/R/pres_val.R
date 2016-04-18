#' Present Value Calculation
#'
#' Given the face value, interest rate and the period of investment this function calculates the present value of a financial security
#' @param face_value Face Value of the security, the default value is 1000,
#' @param int_rate Interest Rate,
#' @param period Period of Investment.
#' @details The formula used for the calculation of the present value is
#'
#' \deqn{PV=FV\frac{1}{(1+i)^n}}
#' where FV is the face value and n is the period of investment.
#' @examples pres_val(face_value=1000, int_rate=0.17, period=6)
#' @return The present value of the security.
#' @references Fabozzi Frank J. and Mann Steven V., Handbook of Fixed Income Securities.
#' @export


pres_val <- function(face_value=1000,
                     int_rate,
                     period){
  face_value*(1/(1+int_rate)^period)
}
