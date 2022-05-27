require(tidyverse)

# N.B. I have no idea what the units of any of these should be.

# positive quadratic root
quad_root <- function(a, b, c) (-b + sqrt(b**2 - 4*a*c)) / (2*a)

# compute Michaelis-Menten constant at temp T. Ei in units J
MM_Arrhenius <- function(Ei, t, K25) K25*exp(Ei*(t - 298)/(298.15*8.3145*t))

# returns the expected degree of limitation
get_Aexp <-  function(Ac, Aj, na.rm = FALSE) pmin(Ac, Aj, na.rm = na.rm)

get_Ac <-  function(Ca, Gs, Press, Rd, tC, Vcmax){
  # Gs = stomatal conductance (?units)
  # Vcmax = maximum rate of carboxylization (?units)
  # Rd = dark respiration, (?units)
  # Ca = Ambient CO2 conc, µmol mol-1?
  # Kc = MM Constant for carboxylase, Pa?
  # Ko = MM Constant for oxygenase, Pa?
  # Press = atmospheric press, kPa
  # tC = temperature, deg C
  
  t <- tC + 273.15
  Kc <- MM_Arrhenius(1, t, 38.67764)  # Kc25 from von Caemmerer 2013 PC&E
  Ko <- MM_Arrhenius(1, t, 26123.26)  # Ko25, von Caemmerer 2013 PC&E
  Ox <- 0.21*Press*1000  # Pa
  
  CaKcKo <- Ca + Kc*(1 + Ox)/Ko  # for brevity
  a <- -Gs**-1
  b <- (Vcmax - Rd)/Gs + CaKcKo
  c <- Rd*CaKcKo
  
  # Ac = Rubisco limitation
  Ac <- quad_root(a, b, c)
  
  return(Ac)
}

get_Aj <- function(Ca, Gs, Rd, Ji, Ci_obs){
  # GamStar25 = CO2 photocompensation point at 25c (? units)
  # Ji = Electron transport rate, (? units)
  # Ci_obs = internal CO2, observed, µmol mol-1 (?)
  
  GamStar25 <- 38.6  # µmol, von Caemmerer 2013 PC&E
  
  a <- -Gs**-1
  b <- (Ji/4 - Rd)/Gs + Ca + 2*GamStar25
  c <- Rd*(Ci_obs + 2*GamStar25) - Ji/4*(Ca - GamStar25) 
  
  # Aj25 = ECT limitation at 25C
  Aj <- quad_root(a, b, c)
  
  # x <- seq(-100, 100, 0.1)
  # plot(x, a*x**2 + b*x + c, xlim=c(0, 50), ylim=c(-100000, 100000), type='l')
  return(Aj)
}

get_Jf <- function(Fmp, Fs, Q, LeafAbs, f){
  # aleaf = fraction of incident PAR absorbed 
  # Q = PAR, µmol m-2 s-1 (?)
  # f = Partition of E between PSII and PSI
  # I think we can get this value from 
  Jf <- f*Q*LeafAbs*(Fmp - Fs)/Fmp
  
  return(Jf)
}