#San Francisco Bay 1992-2021 data set for univariate analyses and models in chapter 2

library(tidyverse)
library(broom)
library(tidyr)
library(purrr)
library(glue)

rm(list=ls())
load("SFBay1_Env.Rdata")
load("SFBay2_Env.Rdata")
theme_set(theme_bw())

#remove NAs 
SFBay1_Env <- SFBay1_Env |>
  filter(!is.na(Chl)) |>
  filter(!is.na(DOY)) |>
  mutate(DOY=as.numeric(DOY)) |>
  mutate(Year=as.numeric(Year),
         Richness=as.integer(Richness),
         Richness_G=as.integer(Richness_G))

SFBay2_Env <- SFBay2_Env |>
  filter(!is.na(Chl)) |>
  filter(!is.na(DOY)) |>
  mutate(DOY=as.numeric(DOY)) |>
  mutate(Year=as.numeric(Year),
         Richness=as.integer(Richness),
         Richness_G=as.integer(Richness_G))

SFBay_Env <- full_join(SFBay1_Env, SFBay2_Env)

#remove Inf values to force models to run with map()
SFBay2_Env <- SFBay2_Env |> subset(invsimpsonRB != "Inf")

#Regressions of variables 
#BIOMASS ---------
#M1 = y ~ x 
SFB1_M1 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)|>
  map(~lm(log(SFBay1_Env$Biomass) ~ .x, data = SFBay1_Env))


SFB2_M1 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)|>
  map(~lm(log(Biomass) ~ .x, data=SFBay2_Env))

modelsummary(SFB1_M1, stars=T, statistic = "p.value")



#remove parameters with no correlation with Biomass
SFB2_M1 <- within(SFB2_M1, rm(Temperature, simpsonRA, simpsonRB, evennessRA, evennessRB, RUE_NChl, RUE_NC,
                              RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, simpsonD, invsimpsonD, expShannonD,
                              evennessD))

#M2 = y ~ log(x)
SFB1_M2 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth, -Season, -Location)|>
  map(~lm(log(SFBay1_Env$Biomass) ~ log(.x),  data=SFBay1_Env))

SFB2_M2 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Biomass) ~ log(.x), data= SFBay2_Env))

#remove parameters with no correlation with Biomass
SFB2_M2 <- within(SFB2_M2, rm(Temperature, simpsonRA, simpsonRB, evennessRA, evennessRB, RUE_NChl, RUE_NC,
                              RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, simpsonD, invsimpsonD, expShannonD,
                              evennessD))

#M3 = y ~ x + x^2
SFB1_M3 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Biomass) ~ .x + I(.x^2), data=SFBay1_Env)) 

SFB2_M3 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Biomass) ~ .x + I(.x^2), data=SFBay2_Env))

#remove parameters with no correlation with Biomass
SFB2_M3 <- within(SFB2_M3, rm(Temperature, simpsonRA, simpsonRB, evennessRA, evennessRB, RUE_NChl, RUE_NC,
                              RUE_PChl, RUE_PC, RUE_SChl, RUE_SC,simpsonD, invsimpsonD, expShannonD,
                              evennessD))
#M4 = y ~ log(x + x^2)
SFB1_M4 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Biomass) ~ log(.x) + I(log(.x)^2), data=SFBay1_Env)) 

SFB2_M4 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Biomass) ~ log(.x) + I(log(.x)^2), data = SFBay2_Env)) 

#remove parameters with no correlation with Biomass
SFB2_M4 <- within(SFB2_M4, rm(Temperature, simpsonRA, simpsonRB, evennessRA, evennessRB, RUE_NChl, RUE_NC,
                              RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, simpsonD, invsimpsonD, expShannonD,
                              evennessD)) 


#summary of models
Biomass_Models <- list(
  "Richness" = SFB2_M3$Richness, "Richness_G" = SFB2_M3$Richness_G, "Salinity" = SFB2_M3$Salinity, 
  "expShannonRA" = SFB2_M4$expShannonRA, "expShannonRB" = SFB2_M4$expShannonRB, "invSimpsonRA" = SFB2_M4$invsimpsonRA,
  "invSimpsonRB" = SFB2_M4$expShannonRB, "NO3" = SFB2_M3$NO3, "PO4" = SFB2_M4$PO4, "SiO4" = SFB2_M3$SiO4, 
  "Mean Size RA" = SFB2_M4$CWM_Size, "Mean Size RB" = SFB2_M3$CWM_Sizeb, "Size Var RA" = SFB2_M4$SizeVar_abu,
  "Size Var RB" = SFB2_M3$SizeVar_bio, "Ext_Coeff" = SFB2_M4$Ext_Coeff, "Chl" = SFB2_M4$Chl, 
  "Abundancee" = SFB2_M4$Abundance
)

modelsummary(Biomass_Models, stars=T, statistic = "p.value", output = "Biomass.html")


#CHLOROPHYLL A -------
#M5 = y ~ x 
SFB1_M5 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Chl) ~ .x, data= SFBay1_Env)) 

SFB2_M5 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Chl) ~ .x, data=SFBay2_Env)) 

#remove parameters with no correlation with Chl-a
SFB2_M5 <- within(SFB2_M5, rm(evennessD, Abundance, simpsonD, simpsonRA, simpsonRB,
                              RUE_NChl, RUE_NC, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, NO3, Season))

#M6 = y ~ log(x) 
SFB1_M6 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Chl) ~ log(.x), data= SFBay1_Env)) 

SFB2_M6 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Chl) ~ log(.x), data=SFBay2_Env)) 

#remove parameters with no correlation with Chl-a
SFB2_M6 <- within(SFB2_M6, rm(evennessD, Abundance, simpsonD, simpsonRA, simpsonRB,
                              RUE_NChl, RUE_NC, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, NO3))

#M7 = y ~ x + x^2
SFB1_M7 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Chl) ~ .x + I(.x^2), data= SFBay1_Env)) 

SFB2_M7 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Chl) ~ .x + I(.x^2), data=SFBay2_Env))

#remove parameters with no correlation with Chl-a
SFB2_M7 <- within(SFB2_M7, rm(evennessD, Abundance, simpsonD, simpsonRA, simpsonRB,
                              RUE_NChl, RUE_NC, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, NO3))

#M8 = y ~ log(x) + log(x^2)
SFB1_M8 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$Chl) ~ log(.x) + I(log(.x)^2), data= SFBay1_Env)) 

SFB2_M8 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$Chl) ~ log(.x) + I(log(.x)^2), log = "x", data=SFBay2_Env))

#remove parameters with no correlation with Chl-a
SFB2_M8 <- within(SFB2_M8, rm(evennessD, Abundance, simpsonD, simpsonRA, simpsonRB,
                              RUE_NChl, RUE_NC, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, NO3))

#model summaries
Chl_Models <- list(
  "Richness" = SFB2_M8$Richness, "Richness_G" = SFB2_M7$Richness_G, "Salinity" = SFB2_M7$Salinity, 
  "expShannonRA" = SFB2_M8$expShannonRA, "expShannonRB" = SFB2_M8$expShannonRB, "invSimpsonRA" = SFB2_M8$invsimpsonRA,
  "invSimpsonRB" = SFB2_M8$expShannonRB, "PO4" = SFB2_M8$PO4, "SiO4" = SFB2_M7$SiO4, 
  "Mean Size RA" = SFB2_M8$CWM_Size, "Mean Size RB" = SFB2_M7$CWM_Sizeb, "Size Var RA" = SFB2_M8$SizeVar_abu,
  "Size Var RB" = SFB2_M8$SizeVar_bio, "Evenness RA" = SFB2_M8$evennessRA, "Evenness RB" = SFB2_M8$evennessRB, 
  "Biomass" = SFB2_M6$Biomass, "Temperature" = SFB2_M7$Temperature
)

modelsummary(Chl_Models, stars=T, statistic = "p.value", output = "Chl.html")

#RUE N Chl -----
#M9 = y ~ x 
SFB1_M9 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NChl) ~ .x, data= SFBay1_Env)) 

SFB2_M9 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NChl) ~ .x, data=SFBay2_Env)) 

#remove parameters with no correlation with RUE_N Chl
SFB2_M9 <- within(SFB2_M9, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD, Richness_G,
                              Ext_Coeff, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NC, evennessD))

#M10 = y ~ log(x) 
SFB1_M10 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NChl) ~ log(.x), data= SFBay1_Env)) 

SFB2_M10 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NChl) ~ log(.x), data=SFBay2_Env)) 

#remove parameters with no correlation with RUE_N Chl
SFB2_M10 <- within(SFB2_M10, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD, Richness_G,
                              Ext_Coeff, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NC, evennessD))
#M11 = y ~ x + x^2
SFB1_M11 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NChl) ~ .x + I(.x^2), data= SFBay1_Env)) 

SFB2_M11 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NChl) ~ .x + I(.x^2), data=SFBay2_Env))

#remove parameters with no correlation with RUE_N Chl
SFB2_M11 <- within(SFB2_M11, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD, Richness_G,
                                Ext_Coeff, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NC, evennessD))
#M12 = y ~ log(x) + log(x^2)
SFB1_M12 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NChl) ~ log(.x) + I(log(.x)^2), data= SFBay1_Env)) 

SFB2_M12 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NChl) ~ log(.x) + I(log(.x)^2),  data=SFBay2_Env))

#remove parameters with no correlation with RUE_N Chl
SFB2_M12 <- within(SFB2_M12, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD, Richness_G,
                                Ext_Coeff, RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NC, evennessD))

#model summaries
RUE_NChl_Models <- list(
  "expShannonRA" = SFB2_M12$expShannonRA, "expShannonRB" = SFB2_M12$expShannonRB, "Richness" = SFB2_M11$Richness,
  "invSimpsonRA" = SFB2_M12$invsimpsonRA, "invSimpsonRB" = SFB2_M12$invsimpsonRB,
  "Size Var RA" = SFB2_M12$SizeVar_abu,  "Size Var RB" = SFB2_M11$SizeVar_bio, "Salinity" = SFB2_M12$Salinity,
  "Mean Size RA" = SFB2_M10$CWM_Size, "Mean Size RB" = SFB2_M9$CWM_Sizeb, "Temperature" = SFB2_M11$Temperature,
  "NO3" = SFB2_M12$NO3, "PO4" = SFB2_M12$PO4, "SiO4" = SFB2_M12$SiO4, "Chl" = SFB2_M12$Chl, 
  "Biomass" = SFB2_M12$Biomass, "Abundance" = SFB2_M12$Abundance, "Evenness RA" = SFB2_M12$evennessRA,
  "Evenness RB" = SFB2_M12$evennessRB
)

modelsummary(RUE_NChl_Models, stars=T, statistic = "p.value", output = "RUE_NChl.html")


#RUE N C -----
#M13 = y ~ x 
SFB1_M13 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NC) ~ .x, data= SFBay1_Env)) 

SFB2_M13 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NC) ~ .x, data=SFBay2_Env)) 

#remove parameters with no correlation with RUE_N C
SFB2_M13 <- within(SFB2_M13, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD,
                              RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NChl, evennessD))

#M10 = y ~ log(x) 
SFB1_M14 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NC) ~ log(.x), data= SFBay1_Env)) 

SFB2_M14 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NC) ~ log(.x), data=SFBay2_Env)) 

#remove parameters with no correlation with RUE_N C
SFB2_M14 <- within(SFB2_M14, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD,
                                RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NChl, evennessD))
#M11 = y ~ x + x^2
SFB1_M15 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NC) ~ .x + I(.x^2), data= SFBay1_Env)) 

SFB2_M15 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NC) ~ .x + I(.x^2), data=SFBay2_Env))

#remove parameters with no correlation with RUE_N C
SFB2_M15 <- within(SFB2_M15, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD,
                                RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NChl, evennessD))
#M12 = y ~ log(x) + log(x^2)
SFB1_M16 <- SFBay1_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay1_Env$RUE_NC) ~ log(.x) + I(log(.x)^2), data= SFBay1_Env)) 

SFB2_M16 <- SFBay2_Env|>
  ungroup()|>
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth, -Location, -Season)|>
  map(~lm(log(SFBay2_Env$RUE_NC) ~ log(.x) + I(log(.x)^2),  data=SFBay2_Env))

#remove parameters with no correlation with RUE_N Chl
SFB2_M16 <- within(SFB2_M16, rm(simpsonD, simpsonRA, simpsonRB, expShannonD, invsimpsonD,
                                RUE_PChl, RUE_PC, RUE_SChl, RUE_SC, RUE_NChl, evennessD))

#model summaries
RUE_NC_Models <- list(
  "expShannonRA" = SFB2_M16$expShannonRA, "expShannonRB" = SFB2_M16$expShannonRB, "Richness" = SFB2_M15$Richness,
  "Richness_G" = SFB2_M15$Richness_G, "invSimpsonRA" = SFB2_M16$invsimpsonRA, "invSimpsonRB" = SFB2_M16$invsimpsonRB,
  "Size Var RA" = SFB2_M14$SizeVar_abu,  "Size Var RB" = SFB2_M15$SizeVar_bio, "Salinity" = SFB2_M15$Salinity,
  "Mean Size RA" = SFB2_M16$CWM_Size, "Mean Size RB" = SFB2_M15$CWM_Sizeb, "Temperature" = SFB2_M15$Temperature,
  "NO3" = SFB2_M16$NO3, "PO4" = SFB2_M15$PO4, "SiO4" = SFB2_M15$SiO4, "Chl" = SFB2_M14$Chl, 
  "Biomass" = SFB2_M16$Biomass, "Abundance" = SFB2_M14$Abundance, "Evenness RA" = SFB2_M16$evennessRA,
  "Evenness RB" = SFB2_M16$evennessRB, "Ext_Coeff" = SFB2_M16$Ext_Coeff
)


modelsummary(RUE_NC_Models, stars=T, statistic = "p.value", output = "RUE_NC.html")

#Location Models-----
#remove NAs 
SFBay1_Location <- SFBay1_Location |>
  filter(!is.na(Chl)) |>
  filter(!is.na(DOY)) |>
  mutate(DOY=as.numeric(DOY)) |>
  mutate(Year=as.numeric(Year),
         Area=as.factor(Area))

SFBay2_Location <- SFBay2_Location |>
  filter(!is.na(Chl)) |>
  filter(!is.na(DOY)) |>
  mutate(DOY=as.numeric(DOY)) |>
  mutate(Year=as.numeric(Year),
         Area=as.factor(Area))

#Biomass y ~ x
SFB1_M1Loc <- SFBay1_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay1_Location$Biomass) ~ (.x) + Area, data= SFBay1_Location)) 

SFB2_M1Loc <- SFBay2_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay2_Location$Biomass) ~ .x + Area,  data=SFBay2_Location))

#Biomass y ~ log(x)
SFB1_M2Loc <- SFBay1_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay1_Location$Biomass) ~ log(.x) + Area, data= SFBay1_Location)) 

SFB2_M2Loc <- SFBay2_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay2_Location$Biomass) ~ log(.x) + Area,  data=SFBay2_Location))


#Biomass y ~ x + x^2
SFB1_M3Loc <- SFBay1_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay1_Location$Biomass) ~ (.x) + I(.x^2) + Area, data= SFBay1_Location)) 

SFB2_M3Loc <- SFBay2_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay2_Location$Biomass) ~ (.x) + I(.x^2) + Area,  data=SFBay2_Location))


#Biomass y ~ log(x) + log(x)^2
SFB1_M4Loc <- SFBay1_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay1_Location$Biomass) ~ log(.x) + I(log(.x)^2) + Area, data= SFBay1_Location)) 

SFB2_M4Loc <- SFBay2_Location|>
  ungroup()|>
  select(-Biomass, -Month,-Year,-DOY,-Station_Number,-Depth, -Area)|>
  map(~lm(log(SFBay2_Location$Biomass) ~ log(.x) + I(log(.x)^2) + Area,  data=SFBay2_Location))


summary(SFB2_M4Loc$Temperature)

modelsummary::modelsummary(SFB2_M4Loc)


car::Anova(SFB2_M4Loc$Salinity)

