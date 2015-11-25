# Potapov Anatoly 2015 

# Fit baseline
# Фитируем задающую синусоиду
# frequency ( "constant", "variable")
FitBaseline <- function(data, frequency = "constant") {
  pi2 <- 3.1415926 * 2
  # колличество циклов за период
  pstart <- pi2 / 5000
  # температура базовой линии
  tstart <- 25.0
  # амплитуда задающей волюны
  astart <-5.0
  fit <- c()
  if ( frequency == "variable") { 
    # fitting model
    df = data.frame(x = data[,1], y = data[,2])
    fit = nls(y ~ p1 * sin( p2 * x + p3 ) + p4, data = df, start = list(p1 = astart, p2 = pstart, p3 = 0, p4 = tstart))
  }
  if ( frequency == "constant") { 
    # fitting model
    df = data.frame(x = data[,1], y = data[,2])
    fit = nls(y ~ p1 * sin( pstart * x + p3 ) + p4, data = df, start = list(p1 = astart, p3 = 0, p4 = tstart))
  }
  return(fit)
}

# Фитируем результируюшую функцию
# frequency ( "constant", "variable")
FitResultLine <- function(data, frequency = "constant") {
  pi2 <- 3.1415926 * 2
  # колличество циклов за период
  pstart <- pi2 / 5000
  # температура базовой линии
  tstart <- 25.0
  # амплитуда задающей волюны
  astart <-3.0
  fit<-c()
  if ( frequency == "variable" ) {
    # fitting model
    df = data.frame(x = data[,1], y = data[,3])
    fit = nls(y ~ p1 * sin( p2 * x + p3 ) + p4 + p5*x + p6*x**2 + p7*x**3 , data = df, start = list(p1 = astart, p2 = pstart, p3 = 0, p4 = tstart, p5 = 0, p6 = 0, p7 = 0))
  }
  if ( frequency == "constant" ) {
    df = data.frame(x = data[,1], y = data[,3])
    fit = nls(y ~ p1 * sin( pstart * x + p3 ) + p4 + p5*x + p6*x**2 + p7*x**3 , data = df, start = list(p1 = astart, p3 = 0, p4 = tstart, p5 = 0, p6 = 0, p7 = 0))
  }
  return(fit)
}
