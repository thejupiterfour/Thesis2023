#San Francisco Bay 1992-2014, seasonal plots
library(tidyverse)
library(MetBrewer)
library(lubridate)
library(showtext)
library(patchwork)

load("SFBay1_Env.Rdata")
load("SFBay2_Env.Rdata")
theme_set(theme_bw())


#counting n per group
tally_SFB1 <- SFBay1|>
  count(Tag)

tally_SFB2 <- SFBay2|>
  count(Tag)

#joining after counting n per group
SFBay1 <- left_join(SFBay1, tally_SFB1)

SFBay2 <-  left_join(SFBay2, tally_SFB2)

SFBay <- full_join(SFBay1, SFBay2)
SFBay_Env <- full_join(SFBay1_Env, SFBay2_Env)

#fonts ---
font_add("Palatino", "Palatino.ttc")
myfont <- "Palatino"

#Rel Bio SFB1 and SFB2------
par(mfrow = c(2,2))
dev
bio_year <- SFBay2%>%
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  ggplot(aes(Year, y=Rel_Bio, fill=Type))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=12))+
  ylab("Relative Biomass Contribution per Sample")+
  theme(text = element_text(size=20, family=myfont))+
  labs(fill="Taxa")+
  #facet_wrap(~Dataset, scales="free")+
  theme(axis.text = element_text(size = 20, family = myfont), 
        plot.title = element_text(size = 24, family = myfont),
        axis.title.x = element_text(size=20, family = myfont),
        axis.title.y = element_text(size=20, family = myfont),
        ) 

abu_year <- SFBay2%>%
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  ggplot(aes(Year, y=Rel_Abu, fill=Type))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=12))+
  ylab("Relative Abundance Contribution per Sample")+
  theme(text = element_text(size=20, family=myfont))+
  labs(fill="Taxa")+
  #facet_wrap(~Dataset, scales="free")+
  theme(axis.text = element_text(size = 20, family = myfont), 
        plot.title = element_text(size = 24, family = myfont),
        axis.title.x = element_text(size=20, family = myfont),
        axis.title.y = element_text(size=20, family = myfont),
        legend.position = "none") 

#save as pdf, jpeg
#cairo_pdf("chfig1.pdf", antialias = "subpixel", width = 14, height = 14)
jpeg("chfig1.jpeg", width = 4008, height = 3008, res = 300)

chfig1 <- abu_year+bio_year
chfig1 

dev.off()
#Rel Abu SFB1 and SFB2----
#Rel Abu based on Location at SFB2
dev.cur()

abu_area <- SFBay2%>%
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  mutate(Area = factor(Area, levels=c("North Bay", "Central Bay", "South Bay"))) %>%
  ggplot(aes(y = reorder(Type, desc(Type)), x=Rel_Abu, fill=Area))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=3))+
  ylab("Phytoplankton Group") +
  xlab("Relative Abundance") +
  scale_x_reverse() +
  theme(text = element_text(size=20, family=myfont))+
  #facet_wrap(~Dataset, scales="free")+
  theme(axis.text = element_text(size = 18, family=myfont), 
        plot.title = element_text(size = 24, family=myfont),
        axis.title.x = element_text(size=20, family=myfont),
        axis.title.y = element_text(size=20, family=myfont),
        legend.position = "none") 

bio_area <- SFBay2%>%
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  mutate(Area = factor(Area, levels=c("North Bay", "Central Bay", "South Bay"))) %>%
  ggplot(aes(y = reorder(Type, desc(Type)), x=Rel_Bio, fill=Area))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=3))+
  ylab("Phytoplankton Group") +
  scale_x_reverse() +
  xlab("Relative Biomass") +
  theme(text = element_text(size=20, family=myfont))+
  #facet_wrap(~Dataset, scales="free")+
  theme(axis.text = element_text(size = 18, family=myfont), 
        plot.title = element_text(size = 24, family=myfont),
        axis.title.x = element_text(size=20, family=myfont),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) 

jpeg("chfig2.jpeg", width = 4008, height = 3008, res = 300)

chfig2 <- abu_area + bio_area
chfig2

dev.off()
#Rel Bio based on Location at SFB2


#% Biomass per each area
bio_yr_area <- SFBay2%>%
  #filter(Type == "Diatom")|>
  #filter(Area == "North Bay")|>
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  mutate(Area = factor(Area, levels=c("North Bay", "Central Bay", "South Bay"))) %>%
  ggplot(aes(y = Rel_Bio, x = Year, fill = Type))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=12))+
  xlab("Year") +
  ylab("Relative Biomass") +
  theme(text = element_text(size=20, family=myfont))+
  facet_grid(~Area)+
  theme(axis.text = element_text(size = 20, family=myfont), 
        plot.title = element_text(size = 24, family=myfont),
        axis.title.x = element_text(size=20, family=myfont),
        axis.title.y = element_text(size=20, family=myfont),
        legend.position = "bottom")

#% Abundance per each area
abu_yr_area <- SFBay2%>%
  #filter(Type == "Diatom")|>
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  mutate(Area =factor(Area, levels=c("North Bay", "Central Bay", "South Bay"))) %>%
  ggplot(aes(y = Rel_Abu, x = Year, fill = Type))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir", n=12))+
  xlab("Year") +
  ylab("Relative Abundance") +
  theme(text = element_text(size=20, family=myfont))+
  facet_grid(~Area)+
  theme(axis.text = element_text(size = 20, family=myfont), 
        plot.title = element_text(size = 24, family=myfont),
        axis.title.x = element_text(size=20, family=myfont),
        axis.title.y = element_text(size=20, family=myfont),
        legend.position = "none")

jpeg("chfig3.jpeg", width = 4008, height = 3008, res = 300)

chfig3 <- abu_yr_area / bio_yr_area
chfig3

dev.off()


#adding season
SFBay2 <- SFBay2 |>
  mutate(Season =  case_when(Month == "1" ~ "Winter", Month == "2" ~ "Winter", Month == "12" ~ "Winter",
                             Month == "3" ~ "Spring", Month == "4" ~ "Spring", Month == "5" ~ "Spring",
                             Month == "6" ~ "Summer", Month == "7" ~ "Summer", Month == "8" ~ "Summer",
                             Month == "9" ~ "Fall", Month == "10" ~ "Fall", Month == "11" ~ "Fall"))

#reorder factor levels
SFBay2 <- SFBay2|>
  mutate(Season = factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")),
         Area = factor(Area, levels=c("North Bay", "Central Bay", "South Bay")),
         Station_Number = factor(Station_Number, levels = c("657","653", "649", "16", "13", "12", "6", "4",
                                                            "18",
                                                            "36", "34", "33", "32", "30", "27", "22", "21")))

jpeg("chfig8.jpeg", width = 4008, height = 3008, res = 300)


SFBay2|>
  group_by(Station_Number, Depth, Year, Month, DOY)|>
  ##mutate(Mean = mean(Salinity))|>
  #filter(Area == "North Bay")|>
  #mutate(ESD = exp(mean(ESD)))|>
  ggplot(aes((Carbon_mL), exp(ESD)))+
  geom_point(aes(color = Area))+
  geom_smooth()+scale_x_log10()


dev.off()

#Plots------
SFBay_Env|>
ggplot(mapping = aes(x = expShannonRA, y = expShannonRB)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Exp Shannon based on abundance') +
  ylab('Exp Shannon based on biomass') +
  facet_grid(~Dataset) +
  scale_x_log10() +
  scale_y_log10() +
  xlim(1, 20) +
  ylim(1, 20)


SFBay_Env|>
ggplot(mapping = aes(x = Richness, y = expShannonRB)) +
  geom_point() +
  geom_smooth() +
  xlab('Richness') +
  ylab('Exp Shannon based on biomass') +
  facet_grid(~Dataset) +
  scale_x_log10() +
  scale_y_log10() +
  xlim(1, 20) +
  ylim(1, 20) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')

SFBay_Env|>
ggplot(mapping = aes(x = Richness, y = expShannonRA)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Richness') +
  ylab('Exp Shannon based on abundance') +
  facet_grid(~Dataset) +
  scale_x_log10() +
  scale_y_log10() +
  xlim(1, 20) +
  ylim(1, 20) +
  geom_abline(intercept=0, slope=1)+
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')

#Biomass increases with exp Shannon RA both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth(color = "#6667AB") +
  xlab('exp Shannon based on Abundance') +
  facet_grid(~Dataset) +
  ylab(expression(paste("log Biomass (", mu,"g C mL"^"-1",")"))) +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
  scale_y_log10()+
  theme(text = element_text(size=25))

#Biomass decreases with exp Shannon RB both datasets
SFBay_Env|>
    ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
    geom_point(alpha=0.6) +
    geom_smooth(color = "#6667AB") +
    xlab('exp Shannon based on Biomass') +
    facet_grid(~Dataset) +
    ylab(expression(paste("log Biomass (", mu,"g C mL"^"-1",")"))) +
    scale_x_log10() +
    stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
    scale_y_log10()+
    theme(text = element_text(size=25))

#Biomass increases with invSimpson RA both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth(color = "#6667AB") +
  xlab('inv Simpson based on Abundance') +
  facet_grid(~Dataset) +
  ylab(expression(paste("log Biomass (", mu,"g C mL"^"-1",")"))) +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
  scale_y_log10()+
  theme(text = element_text(size=25))

#Biomass decreases with invsimpson RB both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRB, y = Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth(color = "#6667AB") +
  xlab('inv Simpson based on Biomass') +
  facet_grid(~Dataset) +
  ylab(expression(paste("log Biomass (", mu,"g C mL"^"-1",")"))) +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
  scale_y_log10()+
  theme(text = element_text(size=25))


#RUE N Chl decreases with exp Shannon RB only at SFB2
SFBay_Env|>
  ggplot(mapping = aes(x= expShannonRB, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Exp Shannon based on biomass') +
  facet_grid(~Dataset) +
  #ylab('RUE_N Chl') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N Chl increases with expShannon RA in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = expShannonRA, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Exp Shannon based on abundance') +
  facet_grid(~Dataset) +
  ylab('RUE_N Chl') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()


#RUE N Chl increases with inv Simpson RA in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRA, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Inverse Simpson based on abundance') +
  facet_grid(~Dataset) +
  ylab('RUE_N Chl') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N Chl decreases with inv Simpson RB in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRB, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Inverse Simpson based on biomass') +
  facet_grid(~Dataset) +
  ylab('RUE_N Chl') +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N Chl increases with Size Variance RA in both datasets
SFBay_Env|> 
  ggplot(mapping = aes(x = SizeVar_abu, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Size Variance based on abundance') +
  facet_grid(~Dataset) +
  ylab('RUE_N Chl') +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N Chl decreases with Size Variance RB in both datasets
SFBay_Env|> 
  ggplot(mapping = aes(x = SizeVar_bio, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Size Variance based on biomass') +
  facet_grid(~Dataset) +
  ylab('RUE_N Chl') +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()


#RUE N C increases with inv Simpson RA in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRA, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Inverse Simpson based on abundance') +
  facet_grid(~Dataset) +
  ylab('RUE_N C') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N C decreases with inv Simpson RB in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = invsimpsonRB, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Inverse Simpson based on biomass') +
  facet_grid(~Dataset) +
  ylab('RUE_N C') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()


#RUE N C increases with Size Variance RA in both datasets
SFBay_Env|> 
  ggplot(mapping = aes(x = SizeVar_abu, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Size Variance based on abundance') +
  facet_grid(~Dataset) +
  ylab('RUE_N Biomass') +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N C decreases with Size Variance RB in both datasets
SFBay_Env|> 
  ggplot(mapping = aes(x = SizeVar_bio, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Size Variance based on biomass') +
  facet_grid(~Dataset) +
  ylab('RUE_N Biomass') +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#RUE N C increases with Richness in both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = Richness_G, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Richness') +
  facet_grid(~Dataset) +
  ylab('RUE_N Biomass') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#Chl increases with richness at SFB1 and decreases at SFB2
SFBay_Env|> 
  ggplot(mapping = aes(x = Richness, y = Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Richness') +
  facet_grid(~Dataset) +
  ylab('Chl') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#Biomass increases with Richness at both datasets
SFBay_Env|> 
  ggplot(mapping = aes(x = Richness, y = Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  xlab('Richness') +
  facet_grid(~Dataset) +
  ylab('Biomass') +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')+
  scale_y_log10()

#Check the relationships between RUE and other factors
#RUE N Chl increases with temperature at SFB2 and no correlation at SFB1
SFBay_Env|>
ggplot(mapping = aes(x = Temperature, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('Temperature') +
  #ylab('RUE_NChl') +
  scale_y_log10()

#Richness decreases with temperature
SFBay_Env|>
  ggplot(mapping = aes(x = Temperature, y = Richness)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('Temperature') +
  #ylab('RUE_NChl') +
  scale_y_log10()

#RUE N C decreases with temperature at SFB1 and increases at SFB2
SFBay_Env|>
  ggplot(mapping = aes(x = Temperature, y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('Temperature') +
  #ylab('RUE_NChl') +
  scale_y_log10()

#RUE N Chl increases with temperature at SFB2
SFBay_Env|>
  ggplot(mapping = aes(x = Temperature, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('Temperature') +
  #ylab('RUE_NChl') +
  scale_y_log10()

#RUE N Chl increases with salinity at SFB2
SFBay_Env|>
  ggplot(mapping = aes(x = Salinity, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('Temperature') +
  #ylab('RUE_NChl') +
  scale_y_log10()

#RUE N Chl increases with Mean Size RA at SFB2
SFBay_Env|>
  ggplot(mapping = aes(x = CWM_Size, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('CWM Size based on abundance') +
  ylab('RUE_N Chl') +
  scale_y_log10()

#RUE N Chl increases with Mean Size RB at both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = CWM_Sizeb, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('CWM Size based on abundance') +
  ylab('RUE_N Chl') +
  scale_y_log10()

SFBay_Env|>
  ggplot(mapping = aes(x = Abundance, y = Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
 # xlab('Richness G') +
#  ylab('RUE_N Chl') +
  scale_y_log10() +
  scale_x_log10()



#seasonal change of RUE 
#RUE N Chl decreases with DOY at SFB1
SFBay_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = RUE_NChl)) +
  geom_point(alpha=0.6, aes(color=Location)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('RUE_N Chl') +
  scale_y_log10()

#RUE N C decreases with DOY at SFB1
SFBay_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = RUE_NC)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('RUE_N Chl') +
  scale_y_log10()

#seasonal change of exp Shannon RA 
#exp Shannon RA decreases with DOY at SFB1
SFBay2_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = expShannonRA)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('exp Shannon based on abundance') +
  scale_y_log10()


#seasonal change of exp Shannon RB 
#exp Shannon RB decreases with DOY at SFB2
SFBay2_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = Richness)) +
  geom_point(alpha=0.6, aes(color=Season)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('exp Shannon based on biomass') +
  scale_y_log10()

#seasonal change of inv Simpson RA
#inv Simpson RA has no seasonal changes
SFBay2_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = invsimpsonRA)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('Inverse Simpson based on abundance') +
  scale_y_log10()

#seasonal change of inv Simpson RB
#inv Simpson RB has no seasonal changes
SFBay_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = invsimpsonRB)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('Inverse Simpson based on biomass') +
  scale_y_log10()

#seasonal change of Richness
#Richness decreases with DOY at both datasets
SFBay_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = Richness)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('Richness') +
  scale_y_log10()

#seasonal change of Size Variance RA
#Size Variance RA has no seasonal changes
SFBay2_Env|>
  ggplot(mapping = aes(x = as.numeric(DOY), y = SizeVar_abu)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir", 30)) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('Size Variance based on abundance') +
  scale_y_log10()

#seasonal change of Size Variance RB
#Size Variance RB has no seasonal changes
SFBay2_Env|>
  ggplot(mapping = aes(x = (DOY), y = expShannonRA)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('DOY') +
  ylab('Size Variance based on biomass') +
  scale_y_log10()

#gam model RUE NChl
gam_RUENChl <- gam(log(RUE_NChl) ~ s((DOY), bs = 'cc'),
                 data = SFBay2_Env)


plot.gam(gam_RUENChl, pages = 1) 




#check all patterns using RUEc and RUEchl, MeanBio
#RUE N C decreases with size variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('RUE_N C ') +
  scale_y_log10()

#RUE N C increases with size variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('RUE_N C') +
  scale_y_log10()

#no correlation between RUE N C ~ Richness G
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('RUE_N C') +
  scale_y_log10()

#no correlation between RUE N Chl ~ Size Variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('RUE_N Chl ') +
  scale_y_log10()

#RUE N Chl increases with Size Variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('RUE_N Chl') +
  scale_y_log10()


#No correlation between RUE N Chl ~ Richness G  and Richness
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_Richness, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#Biomass decreases with Size Variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('Mean Total Biomass ') +
  scale_y_log10()

#No correlation between Biomass ~ Richness G
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Biomass increases with Size Variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#no correlation between Chl ~ Size Variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('Mean Total Chl ') +
  scale_y_log10()

#no correlation between Chl ~ Size Variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#Chl decreases with Richness G
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#RUE N Chl increases with Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#no correlation between RUE N Chl ~ Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#RUE N C increases with Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('RUE_N C') +
  scale_y_log10()

#RUE N C increases with Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('RUE_N C') +
  scale_y_log10()

#Biomass increases with Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Biomass increases with Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#no correlation between Chl ~ Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#no correlation between Chl ~ Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#no correlation between RUE N Chl ~ exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#RUE N Chl increases with exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#no correlation between RUE N C ~ exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('RUE_N C') +
  scale_y_log10()

#RUE N C increases with exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('RUE_N C') +
  scale_y_log10()

#no correlation between biomass ~ exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Biomass increases with exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Chl decreases with exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#no correlation between Chl ~ exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#RUE N Chl decreases with inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#no correlation
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('RUE_N Chl') +
  scale_y_log10()

#no correlation between RUE N C ~ inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('RUE_N C') +
  scale_y_log10()

#RUE N C increases with inv Simpson RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_RUEC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('RUE_N C') +
  scale_y_log10()

#no correlation between biomass ~ inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Biomass increases with inv Simpson RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('Mean Total Biomass') +
  scale_y_log10()

#Chl decreases wiith inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#no correlation between chl ~ inv Simpson RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_Chl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('Mean Total Chl') +
  scale_y_log10()

#RUE P Chl increases with Size Variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#negative correlation between RUE P Chl ~ Size Variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#RUE P Chl increases with Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ inv Simpson RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#no correlation between RUE P Chl ~ Richness
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_Richness, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#positive correlation between RUE P Chl ~ Richness G
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_RUEPChl)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('RUE P Chl') +
  scale_y_log10()

#RUE P C increases with Size Variance RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRA, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on abundance')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C decreases with Size Variance RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeVarRB, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Size Variance based on biomass')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with Mean Size RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRA, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on abundance')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with Mean Size RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_SizeRB, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Size based on biomass')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with exp Shannon RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on abundance')  +
  ylab('RUE P C') +
  scale_y_log10()

#no correlation between RUE P C ~ exp Shannon RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Shannon based on biomass')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with inv Simpson RA
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRA, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on abundance')  +
  ylab('RUE P C') +
  scale_y_log10()

#no correlation between RUE P C ~ inv Simpson RB
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_InvSimpsonRB, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Inverse Simpson based on biomass')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with Richness
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_Richness, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  geom_smooth() +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness')  +
  ylab('RUE P C') +
  scale_y_log10()

#RUE P C increases with Richness G
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_RUEPC)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('RUE P C') +
  scale_y_log10()

#Gam seasonal models -----
gamm_Biomass <- gamm(log(Biomass) ~ s(DOY, bs = 'cc') + Year,
                     data = SFBay2_Env,
                     random = list(Station_Number = ~1))
gamm_Chl <- gamm(log(Chl) ~ s(DOY, bs = 'cc') + Year,
                 data = SFBay2_Env,
                 random = list(Station_Number = ~1))
gamm_NO3 <- gamm(log(NO3) ~ s(DOY, bs = 'cc'),
                 data = SFBay2_Env,
                 random = list(Station_Number = ~1))
gamm_CWM <- gamm(CWM_Sizeb ~ s(DOY, bs = 'cc') + Station_Number,
                 data = SFBay2_Env,
                 gamma = 1.4)

gam_CWM <- gam(CWM_Size ~ s(DOY, bs = 'cc') + s(Year),
               data = SFBay2_Env,
               gamma = 1.4)
gamm_CWM2 <- gamm(CWM_Sizeb ~ s(DOY, bs = 'cc'),
                  data = SFBay2_Env,
                  gamma = 1.4,
                  random = list(Station_Number = ~1))


par(mfrow = c(2, 2))

gam.check(gamm_Biomass$gam)

#plot
SFBay2_AnnualMeans|>
  #filter(Station_Number == "13")|>
  ggplot(aes(factor(x=Station_Number, levels = c("657", "649", "2","3","4","6","12","13",
                                               "16","18", "21", "22","27", "30", "32",
                                               "33", "34", "36")), y=(Mean_SizeRB)))+
  geom_point(alpha = 0.6)+
  geom_smooth()+
  xlab("Station Number") +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')


  
#plot
SFBay2|>
  filter(Genus == "Cryptomonas")|>
  ggplot(aes(x= as.numeric(DOY), y = CWM_Sizeb))+
  geom_point(alpha = 0.6)+
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cc"))+
  scale_y_log10()+
  theme(legend.position = "none")+
  #xlab("DOY") +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')

SFBay2_Env|> 
  #filter(genus == "Cryptomonas")|>
  ggplot(aes(y = SizeVar_abu, x = CWM_Size))+
  geom_point(alpha = 0.6)+
  geom_smooth()+
  #xlab("DOY") +
  #scale_y_log10()+
  #scale_x_log10()+
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho')

#first year talk plots ----- 

SFBay2_Env|>
  ggplot(mapping = aes(x = expShannonRA, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  geom_smooth(color = "#6667AB") +
  xlab('Size Diversity based on biomass') +
  facet_grid(~Dataset) +
  ylab("RUE N Chl") +
  scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
  scale_y_log10()+
  theme(text = element_text(size=25))


(ExpSRA_Bio|ExpSRB_Bio|RichnessBio)/(SizeVarBio|SizeVar1Bio|Mean_SizeBio)

SFBay2_Env|>
  ggplot(mapping = aes(y=Abundance, x = DOY)) +
  geom_point(alpha=0.6) +
  geom_smooth(color = "#6667AB", method = "lm", formula = y ~ (x) + I((x)^2)) +
  xlab("expShannon") +
  #facet_grid(~Dataset)+
  ylab("ln RUE Chl:N") +
  #scale_x_log10() +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho', size=10)+
  scale_y_log10()+
  theme(text = element_text(size=25))


(expSRUE|MeanSizeRUE|RichRUE)/(SizeVarRUE|SizeVar1RUE|TempRUE)
  
ChlBio|SiO4Bio

#RUE ~ Diversity -----
#RUE ~ RUE N Chl
#positive correlation
SFBay1_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#no correlation
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_RichnessG, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness G')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#positive correlation
SFBay1_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_Richness, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#no correlation
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_Richness, y = Mean_RUEChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean Richness')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#positive correlation
SFBay1_Env |>
  ggplot(mapping = aes(x = Richness, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Richness ')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#positive correlation
SFBay2_Env |>
  ggplot(mapping = aes(x = Richness, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Richness ')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#positive correlation
SFBay1_Env |>
  ggplot(mapping = aes(x = Richness_G, y = RUE_NChl)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Richness ')  +
  ylab('RUE N Chl') +
  scale_y_log10()

#no correlation
SFBay2_Env|>
  ggplot(mapping = aes(x = Richness_G, y = RUE_NChl)) +
  geom_point(alpha=0.6, aes(color=Biomass)) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
 # xlab('Richness ')  +
  #ylab('RUE N Chl') +
  scale_y_log10() 

#Biomass ~ Diversity -----
#positive correlation
SFBay1_Env |>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10()

#negative correlation
SFBay1_Env |>
  ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10()

#positive correlation
SFBay2_Env |>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#negative correlation
SFBay2_Env |>
  ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#no correlation
SFBay1_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean exp Shannon based on relative abundance')  +
  ylab('Mean Biomass') +
  scale_y_log10()

#positive correlation
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean exp Shannon based on relative abundance')  +
  ylab('Mean Biomass') +
  scale_y_log10()

#negative correlation
SFBay1_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean exp Shannon based on relative biomass')  +
  ylab('Mean Biomass') +
  scale_y_log10()

#no correlation
SFBay2_AnnualMeans |>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Mean exp Shannon based on relative biomass')  +
  ylab('Mean Biomass') +
  scale_y_log10()

#Biomass ~ Diversity per Type----
#positive correlation
SFBay1_Env_Type|>
  filter(Type == "Diatom")|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10()

#positive correlation
SFBay2_Env_Type|>
  filter(Type == "Diatom")|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#positive correlation
SFBay1_Env_Type|>
  filter(Type == "Dinoflagellates")|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10()

#positive correlation
SFBay2_Env_Type|>
  filter(Type == "Dinoflagellates")|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10()

#negative correlation
SFBay1_Env_Type|>
  filter(Type == "Diatom")|>
  ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10()

#negative correlation
SFBay2_Env_Type|>
  filter(Type == "Diatom")|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#positive correlation
SFBay2_Env_Type|>
  filter(Type == "Diatom")|>
  ggplot(mapping = aes(y = expShannonRA, x = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10()

#positive correlation
SFBay2_Env_Type|>
  filter(Type == "Dinoflagellates")|>
  ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  #scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10()

#all types, positive correlation
SFBay2_Env_Type|>
  ggplot(mapping = aes(x = Biomass, y = expShannonRB)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir")) +
  #facet_wrap(~Type) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#positive correlation
SFBay2_Env_Type|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir")) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#based on location, positive correlation
SFBay2_Location|>
  ggplot(mapping = aes(x = expShannonRA, y = Biomass)) +
  geom_point(alpha=0.6, aes(color=Area)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#based on location, negative correlation
SFBay2_Location|>
  ggplot(mapping = aes(x = expShannonRB, y = Biomass)) +
  geom_point(alpha=0.6, aes(color=Area)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#based on location, positive correlation
SFBay2_Location|>
  ggplot(mapping = aes(x = Richness_G, y = Biomass)) +
  geom_point(alpha=0.6, aes(color=Area)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
 #facet_grid(~Area) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('Genus Richness')  +
  ylab('Biomass') +
  scale_y_log10() +
  scale_x_log10()

#based on location, negative correlation
SFBay2_Location|>
  ggplot(mapping = aes(x = expShannonRB, y = RUE_NChl)) +
  geom_point(alpha=0.6, aes(color=Area)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('RUE N Chl') +
  scale_y_log10() +
  scale_x_log10()

#based on location, positive correlation
SFBay2_Location|>
  ggplot(mapping = aes(x = expShannonRA, y = RUE_NChl)) +
  geom_point(alpha=0.6, aes(color=Area)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('RUE N Chl') +
  scale_y_log10() +
  scale_x_log10()

#based on location, positive correlation
SFBay2_Location_AnnualMeans|>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_Biomass)) +
  geom_point(alpha=0.6, aes(color=Location)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Mean Biomass') +
  scale_y_log10() +
  scale_x_log10()

#based on location, no correlation
SFBay2_Location_AnnualMeans|>
  ggplot(mapping = aes(x = Mean_ShannonRB, y = Mean_Biomass)) +
  geom_point(alpha=0.6, aes(color=Location)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative biomass')  +
  ylab('Mean Biomass') +
  scale_y_log10() +
  scale_x_log10()


#based on location, no correlation
SFBay2_Location_AnnualMeans|>
  ggplot(mapping = aes(x = Mean_ShannonRA, y = Mean_RUEChl)) +
  geom_point(alpha=0.6, aes(color=Location)) +
  #geom_smooth() +
  geom_smooth() +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  xlab('exp Shannon based on relative abundance')  +
  ylab('Mean RUE Chl') +
  scale_y_log10() +
  scale_x_log10()

#based on location, negative correlation
SFBay2_Env|>
  ggplot(mapping = aes(x = Temperature, y = expShannonRB)) +
  geom_point(alpha=0.6, aes(color=Location)) +
  #geom_smooth() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = 'cs')) +
  scale_color_manual(values = met.brewer("Renoir", n=3)) +
  #facet_grid(~Dataset) +
  stat_cor(method='spearman', na.rm=T, cor.coef.name = 'rho') +
  #xlab('exp Shannon based on relative biomass')  +
  #ylab('Mean RUE Chl') +
  scale_y_log10() 


