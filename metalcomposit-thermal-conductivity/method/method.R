# method.R 2015 Potapov Anatoly
# for Metalcompozit

# Constants
pi = 3.1415926

# Стандартизируем фазы и амплитуды
Standartise <- function(nls) {
  amplitude <- summary(nls)$parameters[1,1]
  phase <- c()
  if (amplitude < 0) {
    amplitude <- abs(amplitude)
    phase <- summary(nls)$parameters[2,1] + pi
  }
  else {
    phase <- summary(nls)$parameters[2,1]
  }
  return(list(amp = amplitude,ph = phase, mn = 2*pi / 5000))
}

# Находим х , соответствующий первому максимуму или первому минимуму
SolveLinearEq <- function(data, result) {
  output <- (result - data$ph) / data$mn
  return(output)
} 

# Находим циклы, между которыми ищем разницу
FindSolution <- function(data1, data2) {
  cycle_base <- SolveLinearEq(data1, pi/2)
  cycle_sygnal <- c()
  for (k in (-100:100)) {
    if (( SolveLinearEq(data2, 3*pi/2 + 2*pi * (k-1)) <= cycle_base - 0.0001 ) &&
        ( SolveLinearEq(data2, 3*pi/2 + 2*pi * (k)) >= cycle_base + 0.0001 )) {
    cycle_sygnal <- SolveLinearEq(data2, 3*pi/2 + 2*pi * (k))
    }
  }
  return(list(cycle_sygnal - cycle_base, cycle_base, data2))
}

CalculateParameters <- function(nls1, nls2, period) {
  curve1_base <- Standartise(nls1[[1]])
  curve1_sygnal <- Standartise(nls1[[2]])
  curve2_base <- Standartise(nls2[[1]])
  curve2_sygnal <- Standartise(nls2[[2]])
  # Определяем соотношение амплитуд
  amplitudeRatio <- curve1_sygnal$amp / curve2_sygnal$amp
  # Определяем дельту Т
  deltaT <- (period / 5000) * 
    ( FindSolution(curve2_base, curve2_sygnal)[[1]] - FindSolution(curve1_base, curve1_sygnal)[[1]] )
  return(list(amplitudeRatio, deltaT, FindSolution(curve2_base, curve2_sygnal), FindSolution(curve1_base, curve1_sygnal)))
}