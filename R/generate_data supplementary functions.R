# functions to supplement generate_data 
# these can be useful for constructing questions or performing unit conversions and the like



# Raindrop Sizes  ----


# Number of raindrops per cubic metre in the air per hour is approximately 
# Very crude approximation, only suitable for generic examples/problems
estimate_number_drops <- function(rainfall_mm_per_hour = 10, area_m2 = 1, time_h = 1, print_pretty = FALSE){
  area_mm2 <- area_m2 * 1e6 # convert to mm^2
  water_volume <- rainfall_mm_per_hour * area_mm2 * time_h
  lambda <- 4.1 * (rainfall_mm_per_hour)^(-0.21) # Marshall-Palmer exponential coefficient 
  raindrop_volume <- pi/(lambda^3)
  raindrop_count <- water_volume/raindrop_volume
  if (print_pretty){
    print(paste(prettyNum(raindrop_count, big.mark = ",", scientific = FALSE), " drops", sep = ""))
  }
  return(raindrop_count)
}


# Given number of drops and rainfall rate, report some reasonable values of context parameters
# Very crude approximation, only suitable for generic examples/problems 
raindrop_context_helper <- function(raindrop_count = 1e4, rainfall_mm_per_hour = 10){
  lambda <- 4.1 * (rainfall_mm_per_hour)^(-0.21) # Marshall-Palmer exponential coefficient 
  raindrop_volume <- pi/(lambda^3) / 1e9 # convert volume in mm^3 to m^3
  water_volume <- raindrop_count * raindrop_volume
  areatime <- water_volume/(rainfall_mm_per_hour/1000)
  area_candidates <- c(.001, .002, .005, # 10, 20, 50 cm^2
                       .01,  .02,  .05,  # 100, 200, 500 cm^2
                       .1,   .2,   .5,  # 1000, 2000, 5000 cm^2
                       1)  
  time_estimates <- areatime / area_candidates # should be hours
  time_candidates <- c(1/60, 30/60, 1, 2, 5, 10, 15, 20, 30, 45, 60)/60 # minutes/60 = hours
  area_estimates <- areatime / time_candidates # should be m^2
  
  print("time is MINUTES")
  print(data.frame(area_m2 = round(area_candidates, 3), area_cm2 = round(area_candidates*1e4, 0), time = round(time_estimates * 60, 2)))
  print("time is MINUTES")
  print(data.frame(area_m2 = round(area_estimates, 3), area_cm2 = round(area_estimates*1e4, 0), time = round(time_candidates * 60, 2)))
  
}
