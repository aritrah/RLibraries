#' Macaulay's Duration
#'
#' Given the face value, coupon rate, interest rate (YTM) and the period of investment this function calculates the Macaulay's Duration
#' @param face_value Face Value of the Bond, the default value is 1000,
#' @param int_rate Interest Rate,
#' @param coupon_rate Coupon Payment Rate of the Bond,
#' @param period Period of Investment.
#' @examples macaulay_duration(coupon_rate=0.16,int_rate=0.17,period=6)
#' @return A list with two components
#'
#' \code{duration} the Macaulay's duration.
#'
#' \code{calc_data} The required table for calculation.
#' @references Fabozzi Frank J. and Mann Steven V., Handbook of Fixed Income Securities.
#' @export


macaulay_duration <- function(face_value=1000,
                              coupon_rate,
                              int_rate,
                              period){
  # Cash Flows
  c <- face_value*coupon_rate
  cash_flows <- c(rep(c,(period-1)),
                  face_value+c)

  # Present Value facing interest
  pv_vec <- rep(NA,period)
  for(i in 1:period) pv_vec[i] <- pres_val(face_val=1, int_rate=int_rate, period=i)

  # Discounted Cash Flows
  disc_cf <- rep(NA,period)
  for(i in 1:period) disc_cf[i] <- cash_flows[i]*pv_vec[i]
  current_price <- sum(disc_cf)

  # Present Value at time t
  period_vec <- 1:period
  pv_t <- rep(NA)
  for(i in 1:period) pv_t[i] <- period_vec[i]*disc_cf[i]/current_price
  md <- sum(pv_t) # Macualay's Duration

  mac_tab <- data.frame(cbind(cash_flows,
                              pv_vec,
                              disc_cf,
                              pv_t))

  colnames(mac_tab) <- c("Cash_Flows",
                         "PVIF_i_t",
                         "PV_t",
                         "Duration")
  tab <- rbind(mac_tab,c(NA,NA,current_price,md))

  rnm <- rep(NA,period)
  for(i in 1:period) rnm[i] <- paste("Period",i,sep=" ")
  rownames(tab) <- c(rnm,"Sum")


  list(duration=md,
       calc_data=data.frame(tab))
}
