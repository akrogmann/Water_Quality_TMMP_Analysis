#Adam Krogmann
#Feb 10, 2026

#Libraries
library(tidyverse)
library(lubridate)
library(patchwork)


#input your data
wqraw <- read.csv("C:/Users/adamk/Desktop/WQ Labwork/Adam WQ Dont Analyze This - Sheet1.csv")

##############Goals############################################################
#Need to separate by year site, and by plot
#Then we need to graph means against each other.
###############################################################################

#View Your data:
glimpse(wqraw)


#str(wqraw)
#wqraw$Date_Survey <- mdy(wqraw$Date_Survey)
#str(wqraw)
#str(wqbrewers)
wqbrewers$Date_Survey <- as.factor(wqbrewers$Date_Survey)
wqbrewers$SY <- as.factor(wqbrewers$SY)

#Separation focused on Plots of Brewers
wqbrewers <- wqraw %>% 
  select(Site:Date_Survey, Water_depth:pH) %>% 
  filter((Site == "Brewers Bay") & (Plot == "A" | Plot == "B" | Plot == "C")) %>% 
  group_by(SY) %>% 
  mutate(meansal = mean(Salinity_ppt), meanwd = mean(Water_depth), meantemp = mean(Temp), meandop = mean(DO_percent), meandomgl = mean(DO_mg_L), meanspc = mean(SPC), meantds = mean(TDS), meanph = mean(pH)) %>%
  distinct() %>% 
  arrange(Date_Survey)

wqbrewers$SY <- as.factor(wqbrewers$SY)

#All WQ Classification for Island#
wqclassification <- wqraw %>% 
  select(Island:Date_Survey, Water_depth:pH) %>% 
  filter(Date_Survey == ("8/27/2021") | Date_Survey == ("9/20/2021") 
         | Date_Survey == ("9/20/2021") 
         | Date_Survey == ("9/21/2021") 
         | Date_Survey == ("10/14/2021") 
         | Date_Survey == ("10/22/2021") 
         | Date_Survey == ("11/30/2021") 
         | Date_Survey == ("10/1/2021") 
         | Date_Survey == ("10/12/2021") 
         | Date_Survey == ("9/23/2021") 
         | Date_Survey == ("10/19/2021") 
         | Date_Survey == ("2/22/2022") 
         | Date_Survey == ("10/15/2021") 
         | Date_Survey == ("9/28/2021") 
         | Date_Survey == ("9/30/2021") 
         | Date_Survey == ("10/28/2021") 
         | Date_Survey == ("10/29/2021") 
         | Date_Survey == ("10/26/2021") 
         | Date_Survey == ("4/5/2022") 
         | Date_Survey == ("10/25/2021") 
         | Date_Survey == ("10/29/2021") 
         | Date_Survey == ("12/3/2021") 
         | Date_Survey == ("12/8/2021") 
         | Date_Survey == ("12/2/2021") 
         | Date_Survey == ("12/10/2021") 
         | Date_Survey == ("2/25/2022") 
         | Date_Survey == ("11/15/2021") 
         | Date_Survey == ("11/17/2021") 
         | Date_Survey == ("11/18/2021") 
         | Date_Survey == ("11/16/2021") 
         | Date_Survey == ("11/22/2022") 
         | Date_Survey == ("11/23/2022") 
         | Date_Survey == ("11/11/2022") 
         | Date_Survey == ("11/21/2022") 
         | Date_Survey == ("1/12/2023") 
         | Date_Survey == ("11/21/2022") 
         | Date_Survey == ("2/22/2023") 
         | Date_Survey == ("2/21/2023") 
         | Date_Survey == ("2/23/2023") 
         | Date_Survey == ("1/23/2023") 
         | Date_Survey == ("2/16/2023") 
         | Date_Survey == ("2/15/2023") 
         | Date_Survey == ("2/17/2023") 
         | Date_Survey == ("6/5/2024") 
         | Date_Survey == ("6/10/2024") 
         | Date_Survey == ("6/17/2024") 
         | Date_Survey == ("6/11/2024") 
         | Date_Survey == ("7/9/2024") 
         | Date_Survey == ("7/19/2024") 
         | Date_Survey == ("6/7/2024") 
         | Date_Survey == ("6/12/2024") 
         | Date_Survey == ("6/18/2024") 
         | Date_Survey == ("7/8/2024") 
         | Date_Survey == ("7/5/2024") 
         | Date_Survey == ("6/13/2024") 
         | Date_Survey == ("7/17/2024") 
         | Date_Survey == ("8/6/2024") 
         | Date_Survey == ("8/9/2024") 
         | Date_Survey == ("7/18/2024") 
         | Date_Survey == ("9/18/2024") 
         | Date_Survey == ("8/7/2024") 
         | Date_Survey == ("8/8/2024") 
         | Date_Survey == ("7/22/2024") 
         | Date_Survey == ("7/24/2024") 
         | Date_Survey == ("7/23/2024") 
         | Date_Survey == ("7/24/2024") 
         | Date_Survey == ("7/22/2024")) %>% 
  mutate(Forest_Type = case_when(
    Site %in% c("Great Pond", "Southgate", "Francis Bay", "Lameshur Bay", "Reef Bay", "Compass Point", "Perseverance Bay") ~ "Salt Pond",
    Site %in% c("Krause Lagoon", "Salt River", "Mary Creek", "Princess Bay", "Turner Bay", "Water Creek", "Brewers Bay", "Mandahl Bay", "STEER Fringe", "Vessup Bay") ~ "Fringe",
    Site %in% c("STEER Basin", "Magens Bay") ~ "Basin"
  )) %>% 
  group_by(SY)

wqclassification$SY <- as.factor(wqclassification$SY)



#test




#Factors of all Sites#
ggplot(data = wqclassification) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 35),
            alpha = 0.1, fill = "lightblue") + #RHMA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 25), alpha = 0.5, fill = "red") + #AVGE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 20), alpha = 0.5, fill = "blue") + #LARA
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=10, ymax=35, alpha=0.9, fill = "lightblue") +
  geom_hline (yintercept = 50, linetype = "dashed", color = "blue4") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 20, linetype = "dashed", color = "blue4") +
  geom_hline (yintercept = 65, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 35, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 15, linetype = "dotdash", color = "blue") +
  geom_hline (yintercept = 55, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 10, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 25, linetype = "solid", color = "purple4") +
  geom_boxplot(aes(x = SY, y = Salinity_ppt, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = Salinity_ppt, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)

##Factors of all sites grouped by island##
ggplot(data = wqclassification) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 35),
            alpha = 0.1, fill = "lightblue") + #RHMA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 25), alpha = 0.5, fill = "red") + #AVGE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 20), alpha = 0.5, fill = "blue") + #LARA
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=10, ymax=35, alpha=0.9, fill = "lightblue") +
  geom_hline (yintercept = 50, linetype = "dashed", color = "blue4") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 20, linetype = "dashed", color = "blue4") +
  geom_hline (yintercept = 65, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 35, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 15, linetype = "dotdash", color = "blue") +
  geom_hline (yintercept = 55, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 10, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 25, linetype = "solid", color = "purple4") +
  geom_boxplot(aes(x = SY, y = Salinity_ppt, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = Salinity_ppt, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)



##Factors of all sites grouped by Forest Type##
ggplot(data = wqclassification) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 35),
            alpha = 0.1, fill = "lightblue") + #RHMA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 25), alpha = 0.5, fill = "red") + #AVGE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 20), alpha = 0.5, fill = "blue") + #LARA
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=10, ymax=35, alpha=0.9, fill = "lightblue") +
  geom_hline (yintercept = 50, linetype = "dashed", color = "blue4") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 20, linetype = "dashed", color = "blue4") +
  geom_hline (yintercept = 65, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 35, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 15, linetype = "dotdash", color = "blue") +
  geom_hline (yintercept = 55, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 10, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 25, linetype = "solid", color = "purple4") +
  geom_boxplot(aes(x = SY, y = Salinity_ppt, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = Salinity_ppt, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


#wqbrewers1 <- wqbrewers %>% 
#  select(Site:Date_Survey, Salinity_ppt) %>% 
#  filter((Site == "Brewers Bay") & (Plot == "A" | Plot == "B" | Plot == "C")) %>%
#  group_by(Plot, Site, SY) %>% 
#  summarize(meansal = mean(Salinity_ppt))

#Function for brewers time###########
BrewersWQ1 <- function(yaxis, ymean, ylab){
  ggplot(data = wqbrewers) +
    geom_boxplot(aes(x = SY, y = {{yaxis}}, fill = Plot)) +
    geom_point(aes(x = SY, y = {{ymean}}), group = 1, shape = "triangle", position = position_dodge(width = 0.75), color = "blue4") +
    geom_line(aes(x = SY, y = {{ymean}}), group = 1, linewidth = 1, color = "blue4") +
    geom_point(aes(x = SY, y = {{yaxis}}, fill = Plot, group = Plot), position = position_dodge(width = 0.75)) +
    labs(title = "Brewers Bay", x = "Sample Year", y = ylab) +
    theme_bw()
}

BrewersWQ1(Salinity_ppt, meansal, "Salinity (ppt)") +
  BrewersWQ1(Water_depth, meanwd, "Water Depth (cm)") +
  BrewersWQ1(Temp, meantemp, "Temperature (C)") +
  BrewersWQ1(DO_percent, meandop, "Dissolved Oxygen (%)") +
  BrewersWQ1(DO_mg_L, meandomgl, "Dissolved Oxygen (mg/L)") +
  BrewersWQ1(SPC, meanspc, "SPC") +
  BrewersWQ1(TDS, meantds, "Total Dissolved Solids") +
  BrewersWQ1(pH, meanph, "pH")



#Brewers SY x Salinity with hlines of ideal salinities for LARA + RHMA and line of means.
ggplot(data = wqbrewers) +
  geom_boxplot(aes(x = SY, y = Salinity_ppt, fill = Plot)) +
  geom_point(aes(x = SY, y = meansal), group = 1, shape = "triangle", position = position_dodge(width = 0.75), color = "blue4") +
  geom_line(aes(x = SY, y = meansal), group = 1, linewidth = 1, color = "blue4") +
  geom_point(aes(x = SY, y = Salinity_ppt, fill = Plot, group = Plot), position = position_dodge(width = 0.75)) +
  geom_hline (yintercept = c(14.8, 20), linetype = "dashed", color = "hotpink") + #Optimal Range of LARA is 15-20ppt.
  geom_hline (yintercept = c(15.2, 35), linetype = "dashed", color = "red") + #Optimal range of RHMA is 15-35ppt
  geom_hline (yintercept = 50, linetype = "solid", color = "hotpink") + #50ppt limits growth of LARA, 65ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 65, linetype = "solid", color = "red") +
  labs(title = "Brewers Bay", x = "Sample Year", y = "Salinity (ppt)") +
  theme_bw()


#Clean graph of fill lines for mangrove physiological metrics (AVGE, LARA, RHMA)#
ggplot(data = wqbrewers) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 35),
            alpha = 0.1, fill = "lightblue") + #RHMA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 25), alpha = 0.5, fill = "red") + #AVGE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 15, ymax = 20), alpha = 0.5, fill = "blue") + #LARA
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=10, ymax=35, alpha=0.9, fill = "lightblue") +
  geom_hline (yintercept = 50, linetype = "dashed", color = "blue4") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 20, linetype = "dashed", color = "blue4") +
  geom_hline (yintercept = 65, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 35, linetype = "dotted", color = "blue") +
  geom_hline (yintercept = 15, linetype = "dotdash", color = "blue") +
  geom_hline (yintercept = 55, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 10, linetype = "solid", color = "purple4") +
  geom_hline (yintercept = 25, linetype = "solid", color = "purple4") +
  geom_boxplot(aes(x = SY, y = Salinity_ppt, fill = Plot)) +
  geom_point(aes(x = SY, y = Salinity_ppt, fill = Plot, group = Plot), position = position_dodge(width = 0.75)) +
  labs(title = "Brewers Bay", x = "Sample Year", y = "Salinity (ppt)") +
  theme_bw()




#View Your data:
unique(wqbrewers$Date_Survey)
unique(wqbrewers$SY)

#Brewers Water Depth of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = Salinity_ppt, fill = Plot)) +
  geom_boxplot() +
  geom_point(aes(group = Plot), position = position_dodge(width = 0.75)) +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "Water Depth (cm)") +
  theme_bw()

###Smooth Brewers plot, DO NOT USE###
ggplot(wqbrewers, aes(x = SY, y = Salinity_ppt, fill = Plot)) +
  geom_boxplot() +
  geom_smooth(aes(group = Plot, color = Plot)) +
  geom_hline (yintercept = c(14.8, 20), linetype = "dashed", color = "hotpink") + #Optimal Range of LARA is 15-20ppt.
  geom_hline (yintercept = c(15.2, 35), linetype = "dashed", color = "red") + #Optimal range of RHMA is 15-35ppt
  geom_hline (yintercept = 50, linetype = "solid", color = "hotpink") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
  geom_hline (yintercept = 65, linetype = "solid", color = "red") +
  geom_point(aes(group = Plot), position = position_dodge(width = 0.75)) +
  labs(title = "Brewers Bay", x = "Sample Year", y = "Salinity (ppt)") +
  theme_bw()



###Function for all of the data points in Brewers###########################

#################WRONG LINES#################

BrewersWQ <- function(yaxis, ylab){
#Brewers Water Depth of years and all plots
ggplot(wqbrewers, aes(x = SY, y = {{yaxis}}, fill = Plot)) +
  geom_violin() +
    geom_smooth(aes(group = Plot, color = Plot)) +
  geom_point(aes(group = Plot), position = position_dodge(width = 0.75)) +
    labs(title = "Brewers Bay", x = "SY", y = ylab) +
  theme_bw()
}

BrewersWQ(Water_depth, "Water Depth (cm)") + 
  BrewersWQ(Temp, "Temperature (C)") +
  BrewersWQ(DO_percent, "Dissolved Oxygen (%)") +
  BrewersWQ(DO_mg_L, "Dissolved Oxygen (mg/L)") +
  BrewersWQ(Salinity_ppt, "Salinity (ppt)") +
  BrewersWQ(SPC, "SPC") +
  BrewersWQ(TDS, "TDS") +
  BrewersWQ(pH, "pH")

#######################################################
##Testing data, IRRELEVANT###
#Brewers Temp of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = Temp, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "Temp (C)") +
  theme_bw()

#Brewers DO Percent of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = DO_percent, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "DO (%)") +
  theme_bw()

#Brewers DO (mg/L) of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = DO_mg_L, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "DO (mg/L)") +
  theme_bw()

#Brewers Salinity (ppt) of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = Salinity_ppt, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "Salinity (ppt)") +
  theme_bw()

#Brewers SPC of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = SPC, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "SPC") +
  theme_bw()

#Brewers TDS of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = TDS, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "TDS") +
  theme_bw()

#Brewers pH of all dates and all plots
ggplot(wqbrewers, aes(x = Date_Survey, y = pH, fill = Plot)) +
  geom_boxplot() +
  labs(title = "Brewers Bay", x = "Date Surveyed", y = "pH") +
  theme_bw()

############################



#For cleaning the rows that contain na values
#wqbrewers23_cleaned_drop_na <- wqbrewers23 %>% 
#  drop_na()



#%>% 
#  summarize(mean_brewers = mean(Water_depth, na.rm = TRUE))




