# Complementarity and Selection effects (CE and SE, resp.) computation 
# analyses related to chapter 3 of Oliveira thesis

# load libraries
library(tidyverse)

# load .Rdata files required
load("Mix20.Rdata") # mixture data (n = 20 spp)
load("mModels.Rdata") # monoculture models
load("Models.Rdata") # mixture models
load("Mono_ini.Rdata") # monoculture initial values

# create df to store results
result_df <- data.frame(
  Species = character(),  # Column to store species names
  NutrientRegime = character(),  # Column to store nutrient regime names
  TimeStep = integer(), # Column to store time step values
  #MonoBio = numeric(), # Column to store monoculture bio
  DeltaRY = numeric()  # Column to store the calculated deviations
)

# loop to compute CE and SE for phytoplankton
for(nutrient_reg in Mix20$N_reg){
  for(species in Mix20$Species){
    for(time_step in Mix20$time){
      
      #subset biomass values of mixture at day x
      mix_subset <- subset(Mix20)
      
      #subset biomass values of mixture at initial day
      mix_subset_ini <- subset(Mix20,
                               time == 1461)
      
      #subset biomass values of monocultures at final day
      mono_subset <- subset(Mono_ini)
      
      #observed yield of species i in the mixture at final day
      Yi <- mix_subset$TPHY
      
      #observed yield of species i in the monoculture at final day
      Mi <- mono_subset$TPHY
      
      #initial yield of species i in the mixture
      Yini <- mix_subset_ini$TPHY
      
      #initial total biomass of mixture
      totYini <- sum(mix_subset_ini$TPHY)
      
      #expected biovolume yield
      d_RYi <- (Yi/Mi) - (Yini/totYini)
      
      return(d_RYi)
      
      result_df <- rbind(result_df, data.frame(
        Species = Mix20$Species,
        NutrientRegime = Mix20$N_reg,
        TimeStep = Mix20$time,
        #MonoBio = Mono_ini$TPHY,
        DeltaRY = d_RYi))
      
    }
    
  }
  
}

#mean overall, final day
result_df <- result_df |>
  group_by(NutrientRegime) |>
  mutate(meanDRY = mean(DeltaRY))

mean_Mono <- Mono_ini|>
  group_by(N_reg, time)|>
  mutate(meanMono = mean(TPHY)) |>
  select(meanMono, N_reg, Species, TPHY) |>
  rename(NutrientRegime = N_reg, TimeStep = time)

result_df <- left_join(result_df, mean_Mono, by = c("Species", "NutrientRegime", "TimeStep"))

N_spp <- 20

result_df <- result_df |>
  mutate(CE = (N_spp * meanDRY * meanMono), 
         SE = N_spp * cov(DeltaRY, TPHY, use = "everything"))

result_df$NutrientRegime <- factor(result_df$NutrientRegime, levels = c("N1", "N2","N3","N4","N5","N6",
                                                                        "N7","N8","N9","N10","N11","N12",
                                                                        "N13", "N14", "N15", "N16", "N17",
                                                                        "N18", "N19", "N20"))

result_df |>
  #filter(Species == "Spp_15") |>
  group_by(NutrientRegime) |>
  ggplot(aes(TimeStep, CE, color = Species))+
  geom_point() 

