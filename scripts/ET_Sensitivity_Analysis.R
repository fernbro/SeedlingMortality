library()

# Stanghellini model:

# Validated (?) further in Villarreal-Guerrero et al. 2011:
# https://doi.org/10.1016/j.scienta.2011.10.016

et <- function(Tair, netrad, lai, rho_air, Cp, r_e, vpd, r_i){
  
  delta = 41.45 * exp(0.061*Tair)
  
  # gamma (psychrometric constant): 66 (from paper above)
  
  x = ((delta * netrad) +  (((2*lai*rho_air*Cp)/r_e)*vpd))/(66 * (1 + (delta/66) + (r_i/r_e)))
  
  return(x)
}

# resistance terms rely on so many assumptions
# i don't feel comfortable with this necessarily EEK

# mortality under experimental drought reveals no impacts of a humid heatwave and
# highlights trailing edge planting risks for southwestern forest species

# Tair: air temperature, ºC
# delta: slope of the saturation curve of the psychrometric chart, Pa/ºC
# netrad: net shortwave radiation, W/m2
# lai: leaf area index
# rho_air: density of air, kg/m3
# Cp: specific heat capacity of air, J/kg/ºC
# r_e: external crop resistance, s/m
# vpd: vapor pressure deficit of air, Pa
# gamm: physchrometric constant, Pa/ºC (set to 66 as per VG et al. '11)
# r_i: internal crop resistance, s/m

# any constants I can define?

# according to paper above:
# delta <-  41.45 * exp(0.061·T)
# net shortwave? 





# or PM model:


