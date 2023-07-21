#San Francisco Bay Seasonal Plots

#plot
SFBay1_Env|>
  filter(Station_Number == "6")|>
  ggplot(aes(DOY, log(Biomass), colour=Year))+
  geom_point()

#number of samples per year
Year_1993 <- SFBay_Env |>
  filter(Year =="1993") |>
  count(Sample_ID)

#joining all as a list
Sample_Years <- mget(str_glue("Year_{1992:2021}"))%>%
  reduce(full_join)

#numbers about sample amounts
Sample_Years|>
  group_by(Station_Number)|>
  summarise(no_stations_sampled = length(Sample_ID))
