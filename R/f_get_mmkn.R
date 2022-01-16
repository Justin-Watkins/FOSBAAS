#' @title f_get_MMKN
#' @description produce queuing output
#' @source \url{GIT}
#' @export



f_get_MMKN <- function(k,N,ta,ts){

  # k = number of servers
  # N = maximum allowed in the system
  # ta = average time between arrivals
  # ts = average service time

  lambda = 1/ta #: per minute
  mu     = 1/ts #: per minute
  rho = lambda/mu #: utilization ratio

  #------------------------------------------------------------------
  # Probability of n units in the system
  # for
  n = seq(0, N-1, by = 1 )
  P0 <- 1/ sum(((rho^n)/factorial(n)) + ((rho^k)/(factorial(k)*((k^(N-k+1)) - (rho^(N-k+1))/((k-rho)*(k^(N-k)))))))

  # Probability of n units in the system
  # for
  n = seq(0, k, by = 1 )
  Pn0 <- rho^n/factorial(n)*P0

  # for
  n = seq(k + 1, N, by = 1 )
  Pn1 <- rho^n/(factorial(k)*k^(n-k))*P0

  Pn      <- c(Pn0,Pn1)

  #------------------------------------------------------------------
  # calculations
  len     <- max(length(Pn))

  lambda_e  <- lambda*(1 - Pn[len])
  rho_e    <- lambda_e/mu

  # Expected in queue
  Ls = rho_e #   Ls = 1*Pn[2] + 2*sum(Pn[-c(1,2)])

  # for
  n = seq(k+2, N + 1, by = 1 )
  Lq = sum((n-(k+1))*Pn[n]) # Lq = 1*Pn[4] + 2*Pn[5] + 3*Pn[6]

  # expected units in the system
  L = Ls + Lq

  # Expected service time
  Ws = Ls/lambda_e # minutes in service
  Wq = Lq/lambda_e # minutes in queue
  W  = Wq + Ws   # minutes in system

  #------------------------------------------------------------------
  # Build output
  frame <- data.frame(matrix(nrow = 7,ncol =2))
  names(frame) <- c('Metric','Value')

  metric <- c('Servers:','System Capacity:','Time between arrivals:',
              'Average service time:','Minutes in service:',
              'Minutes in queue:','Minutes in system:')
  values <- c(k,N,ta,ts,Ws,Wq,W)

  frame[,1] <- metric
  frame[,2] <- values

  return(frame)

}
