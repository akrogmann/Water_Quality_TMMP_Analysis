#Adam Krogmann
#Feb 10, 2026

#Libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(readr)


#input your data
wqraw <- read.csv("C:/Users/adamk/Desktop/WQ Labwork/Adam WQ Dont Analyze This - Sheet1.csv")

TreeMeasurements <- read_csv("QA_QC TMMP Data 6-25-25.csv")

##############Goals############################################################
#Need to separate by year site, and by plot
#Then we need to graph means against each other.
###############################################################################

#View Your data:
glimpse(wqraw)

#################Old Data#########################################################
#str(wqraw)
#wqraw$Date_Survey <- mdy(wqraw$Date_Survey)
#str(wqraw)
#str(wqbrewers)
#wqbrewers$Date_Survey <- as.factor(wqbrewers$Date_Survey)
#wqbrewers$SY <- as.factor(wqbrewers$SY)

#Separation focused on Plots of Brewers
#wqbrewers <- wqraw %>% 
#  select(Site:Date_Survey, Water_depth:pH) %>% 
#  filter((Site == "Brewers Bay") & (Plot == "A" | Plot == "B" | Plot == "C")) %>% 
#  group_by(SY) %>% 
#  mutate(meansal = mean(Salinity_ppt), meanwd = mean(Water_depth), meantemp = mean(Temp), meandop = mean(DO_percent), meandomgl = mean(DO_mg_L), meanspc = mean(SPC), meantds = mean(TDS), meanph = mean(pH)) %>%
#  distinct() %>% 
#  arrange(Date_Survey)

#wqbrewers$SY <- as.factor(wqbrewers$SY)
###############################################################################

#All WQ Classification for Island and syringe use#
wqclassification <- wqraw %>% 
  select(Island:Date_Survey, Water_depth:Syringe_used) %>% 
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



#Now lets separite sites by tree type
RHMA<-TreeMeasurements%>%
  filter(Species=="RHMA")


unique(RHMA$Site)
#[1] "Compass Point"    "Salt River"       "Krause Lagoon"    "STEER Fringe"    
#[5] "Princess Bay"     "Magens Bay"       "STEER Basin"      "Turner Bay"      
#[9] "Lameshur Bay"     "Water Creek"      "Brewers Bay"      "Mary Creek"      
#[13] "Great Pond"       "Mandahl Bay"      "Perseverance Bay" "Vessup Bay" 


RHMAwq<-wqclassification%>%
  filter(Site %in% RHMA$Site)

RHMAwq<-RHMAwq%>%
  mutate(
    Syringe_used=recode(Syringe_used,
                        "N"="No",
                        "Y"="Yes",
                        "no"="No",
                        "yes"="Yes",
                        "n"="No")
  )%>% 
  filter(nzchar(as.character(Syringe_used)))

RHMAwq<-RHMAwq%>%
  mutate(Syringe_used=replace_na(Syringe_used, "Unknown")) 

RHMAwq$Syringe_used<-as.factor(RHMAwq$Syringe_used)
RHMAwq$SY<-as.factor(RHMAwq$SY)

unique(RHMAwq$Syringe_used)

unique(RHMAwq$Site)

#now do AVGE
AVGE<-TreeMeasurements%>%
  filter(Species=="AVGE")%>%
  filter(!(is.na(Site)))

unique(AVGE$Site)
# "Salt River"    "Reef Bay"      "Lameshur Bay"  "Mary Creek"    "Southgate"    
#[6] "Compass Point" "Great Pond"    "Water Creek"   "STEER Basin"

AVGEwq<-wqclassification%>%
  filter(Site %in% AVGE$Site)


AVGEwq<-AVGEwq%>%
  mutate(
    Syringe_used=recode(Syringe_used,
                        "N"="No",
                        "Y"="Yes",
                        "no"="No",
                        "yes"="Yes",
                        "n"="No")
  )%>% 
  filter(nzchar(as.character(Syringe_used)))

AVGEwq<-AVGEwq%>%
  mutate(Syringe_used=replace_na(Syringe_used, "Unknown")) 

AVGEwq$Syringe_used<-as.factor(AVGEwq$Syringe_used)
AVGEwq$SY<-as.factor(AVGEwq$SY)

unique(AVGEwq$Syringe_used)

unique(AVGEwq$Site)
#excellent!

#Now do LARA
LARA<-TreeMeasurements%>%
  filter(Species=="LARA")

unique(LARA$Site)
#[1] "Salt River"       "Brewers Bay"      "Mary Creek"       "Turner Bay"      
#[5] "STEER Fringe"     "STEER Basin"      "Southgate"        "Reef Bay"        
#[9] "Water Creek"      "Compass Point"    "Francis Bay"      "Magens Bay"      
#[13] "Princess Bay"     "Mandahl Bay"      "Perseverance Bay" "Vessup Bay"      
#[17] "Lameshur Bay"

LARAwq<-wqclassification%>%
  filter(Site %in% LARA$Site)

LARAwq<-LARAwq%>%
  mutate(
    Syringe_used=recode(Syringe_used,
                        "N"="No",
                        "Y"="Yes",
                        "no"="No",
                        "yes"="Yes",
                        "n"="No")
  )%>% 
  filter(nzchar(as.character(Syringe_used)))

LARAwq<-LARAwq%>%
  mutate(Syringe_used=replace_na(Syringe_used, "Unknown")) 

LARAwq$Syringe_used<-as.factor(LARAwq$Syringe_used)
LARAwq$SY<-as.factor(LARAwq$SY)

unique(LARAwq$Syringe_used)

unique(LARAwq$Site)
#yay!!!




#salinity rectangles
salrect_df <- data.frame(
  xmin = c(-Inf, -Inf, -Inf),
  xmax = c(Inf, Inf, Inf),
  ymin = c(15, 10, 15),
  ymax = c(35, 25, 20),
  Ideal_Range = c("RHMA", "AVGE", "LARA") # Optional: add grouping
)

#Salinity rectange RHMA
salrectRHMA_df <- data.frame(
  xmin = (-Inf),
  xmax = (Inf),
  ymin = (15),
  ymax = (35),
  Ideal_Range = ("RHMA") # Optional: add grouping
)

#Salinity rectangle LARA
salrectLARA_df <- data.frame(
  xmin = (-Inf),
  xmax = (Inf),
  ymin = (15),
  ymax = (20),
  Ideal_Range = ("LARA") # Optional: add grouping
)

#Salinity rectangle AVGE
salrectAVGE_df <- data.frame(
  xmin = (-Inf),
  xmax = (Inf),
  ymin = (10),
  ymax = (25),
  Ideal_Range = ("AVGE") # Optional: add grouping
)

#salinity hlines
salhline_data <- data.frame(y = c(50, 20, 65, 35, 15, 55, 10, 25), type = factor(c(2, 2, 3, 3, 4, 1, 1, 1)), 
                           stringsAsFactors = FALSE)

#Salinity hlines RHMA
salhlineRHMA_data <- data.frame(y = (65), type = factor(1), 
                            stringsAsFactors = FALSE)

#Salinity hlines LARA
salhlineLARA_data <- data.frame(y = (50), type = factor(1), 
                                stringsAsFactors = FALSE)
  
#Salinity hlines AVGE
salhlineAVGE_data <- data.frame(y = (55), type = factor(1), 
                                stringsAsFactors = FALSE)


###Testing#####################################################################

#Old hline data points, bad now because no legend.
#  geom_hline (yintercept = 50, linetype = "dashed", color = "blue4") + #50ppt limits growth of LARA, 60ppt ceases growth of RHMA saplings
#    geom_hline (yintercept = 20, linetype = "dashed", color = "blue4") +
#    geom_hline (yintercept = 65, linetype = "dotted", color = "blue") +
#    geom_hline (yintercept = 35, linetype = "dotted", color = "blue") +
#    geom_hline (yintercept = 15, linetype = "dotdash", color = "blue") +
#    geom_hline (yintercept = 55, linetype = "solid", color = "purple4") +
#    geom_hline (yintercept = 10, linetype = "solid", color = "purple4") +
#    geom_hline (yintercept = 25, linetype = "solid", color = "purple4") +






#this line data works, but it doesnt work if i plug it in to my graph.
#    ggplot() +
#    geom_hline(data = salhline_data, 
#               aes(yintercept = y, linetype = type, colour = type)) +
#    scale_colour_manual(values = c("blue4", "blue", "blue", "purple4"), 
#                        labels = c("LARA", "RHMA", "LARA + RHMA", "AVGE"),
#                        name = "Key") +
#    scale_linetype_manual(values = 1:4, 
#                          labels = c("LARA", "RHMA", "LARA + RHMA", "AVGE"),
#                          name = "Key")    
    
    
###TEST Concluded ############################################################

  
##############################################################################
  #Salinity of all AVGE sites
ggplot() +
    geom_rect(data = salrectAVGE_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
    geom_hline(data = salhlineAVGE_data, 
               aes(yintercept = y, linetype = type)) +
    scale_linetype_manual(values = 1, 
                          labels = ("AVGE"),
                          name = "Physiological Limit") +
    scale_fill_manual(values = c("AVGE" = "grey3", name = "Fill of Ideal Salinity")) +
    geom_boxplot(data = AVGEwq, aes(x = SY, y = Salinity_ppt, color = Site)) +
    geom_point(data = AVGEwq, aes(x = SY, y = Salinity_ppt, Fill = Site, group = Site, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
    labs(x = "Year", y = "Salinity (ppt)")
  
  #Salinity of all RHMA sites
  
  ggplot() +
    geom_rect(data = salrectRHMA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
    geom_hline(data = salhlineRHMA_data, 
               aes(yintercept = y, linetype = type)) +
    scale_linetype_manual(values = 1, 
                          labels = ("RHMA"),
                          name = "Physiological Limit") +
    scale_fill_manual(values = c("RHMA" = "pink3", name = "Fill of Ideal Salinity")) +
    geom_boxplot(data = RHMAwq, aes(x = SY, y = Salinity_ppt, color = Site)) +
    geom_point(data = RHMAwq, aes(x = SY, y = Salinity_ppt, Fill = Site, group = Site, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
    labs(x = "Year", y = "Salinity (ppt)")
  
  #Salinity of all LARA sites
  
  ggplot() +
    geom_rect(data = salrectLARA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
    geom_hline(data = salhlineLARA_data, 
               aes(yintercept = y, linetype = type)) +
    scale_linetype_manual(values = 1, 
                          labels = ("LARA"),
                          name = "Physiological Limit") +
    scale_fill_manual(values = c("LARA" = "brown", name = "Fill of Ideal Salinity")) +
    geom_boxplot(data = LARAwq, aes(x = SY, y = Salinity_ppt, color = Site)) +
    geom_point(data = LARAwq, aes(x = SY, y = Salinity_ppt, Fill = Site, group = Site, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
    labs(x = "Year", y = "Salinity (ppt)")
  
  
##############################################################################
 #Factors of all Sites#
  
  #sal of all sites
  ggplot() +
    geom_rect(data = salrect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
    geom_hline(data = salhline_data, 
               aes(yintercept = y, linetype = type)) +
    scale_linetype_manual(values = 1:4, 
                          labels = c("AVGE", "LARA", "RHMA", "LARA + RHMA"),
                          name = "Linetype") +
    scale_fill_manual(values = c("RHMA" = "lightblue3", "AVGE" = "red", "LARA" = "purple3", name = "Fill of Ideal Salinity")) +
    geom_boxplot(data = wqclassification, aes(x = SY, y = Salinity_ppt, color = Site)) +
    geom_point(data = wqclassification, aes(x = SY, y = Salinity_ppt, Fill = Site, group = Site,), position = position_dodge(width = 0.75), size = 0.4) +
    labs(x = "Year", y = "Salinity (ppt)")




##Water Depth all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Water_depth, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = Water_depth, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)


##Temp all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Temp, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = Temp, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)


##DO% all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_percent, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = DO_percent, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)

##DO/mg/l all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_mg_L, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = DO_mg_L, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)

##SPC all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = SPC, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = SPC, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)


##TDS all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = TDS, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = TDS, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)


##pH all sites##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = pH, Fill = Site, color = Site)) +
  geom_point(aes(x = SY, y = pH, group = Site, fill = Site), position = position_dodge(width = 0.75), size = 0.4)

################################################################################
##Factors of all sites grouped by island##


#Salinity of all sites grouped by island##
ggplot() +
  geom_rect(data = salrect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhline_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1:4, 
                        labels = c("AVGE", "LARA", "RHMA", "LARA + RHMA"),
                        name = "Linetype") +
  scale_fill_manual(values = c("RHMA" = "lightblue3", "AVGE" = "red", "LARA" = "purple3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = wqclassification, aes(x = SY, y = Salinity_ppt, color = Island)) +
  geom_point(data = wqclassification, aes(x = SY, y = Salinity_ppt, Fill = Island, group = Island,), position = position_dodge(width = 0.75), size = 0.4) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of AVGE grouped by island#

ggplot() +
  geom_rect(data = salrectAVGE_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineAVGE_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("AVGE"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("AVGE" = "grey3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = AVGEwq, aes(x = SY, y = Salinity_ppt, color = Island)) +
  geom_point(data = AVGEwq, aes(x = SY, y = Salinity_ppt, Fill = Island, group = Island, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of RHMA grouped by island#

ggplot() +
  geom_rect(data = salrectRHMA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineRHMA_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("RHMA"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("RHMA" = "pink3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = RHMAwq, aes(x = SY, y = Salinity_ppt, color = Island)) +
  geom_point(data = RHMAwq, aes(x = SY, y = Salinity_ppt, Fill = Island, group = Island, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of LARA grouped by island#

ggplot() +
  geom_rect(data = salrectLARA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineLARA_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("LARA"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("LARA" = "brown", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = LARAwq, aes(x = SY, y = Salinity_ppt, color = Island)) +
  geom_point(data = LARAwq, aes(x = SY, y = Salinity_ppt, Fill = Island, group = Island, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")


##Water Depth by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Water_depth, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = Water_depth, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)



##Temp by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Temp, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = Temp, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)


##DO% by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_percent, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = DO_percent, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)


##DO/mg/L by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_mg_L, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = DO_mg_L, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)

##SPC by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = SPC, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = SPC, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)


##TDS by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = TDS, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = TDS, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)


##pH by Islands##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = pH, Fill = Island, color = Island)) +
  geom_point(aes(x = SY, y = pH, group = Island, fill = Island), position = position_dodge(width = 0.75), size = 0.4)


##Factors of all sites grouped by Forest Type##
#Salinity of all sites grouped by Forest Type##
ggplot() +
  geom_rect(data = salrect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhline_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1:4, 
                        labels = c("AVGE", "LARA", "RHMA", "LARA + RHMA"),
                        name = "Linetype") +
  scale_fill_manual(values = c("RHMA" = "lightblue3", "AVGE" = "red", "LARA" = "purple3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = wqclassification, aes(x = SY, y = Salinity_ppt, color = Forest_Type)) +
  geom_point(data = wqclassification, aes(x = SY, y = Salinity_ppt, Fill = Forest_Type, group = Forest_Type,), position = position_dodge(width = 0.75), size = 0.4) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of AVGE grouped by Forest Type#

ggplot() +
  geom_rect(data = salrectAVGE_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineAVGE_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("AVGE"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("AVGE" = "grey3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = AVGEwq, aes(x = SY, y = Salinity_ppt, color = Forest_Type)) +
  geom_point(data = AVGEwq, aes(x = SY, y = Salinity_ppt, Fill = Forest_Type, group = Forest_Type, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of RHMA grouped by Forest Type#

ggplot() +
  geom_rect(data = salrectRHMA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineRHMA_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("RHMA"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("RHMA" = "pink3", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = RHMAwq, aes(x = SY, y = Salinity_ppt, color = Forest_Type)) +
  geom_point(data = RHMAwq, aes(x = SY, y = Salinity_ppt, Fill = Forest_Type, group = Forest_Type, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")

#Salinity of LARA grouped by Forest Type#

ggplot() +
  geom_rect(data = salrectLARA_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Ideal_Range), alpha = 0.3) +
  geom_hline(data = salhlineLARA_data, 
             aes(yintercept = y, linetype = type)) +
  scale_linetype_manual(values = 1, 
                        labels = ("LARA"),
                        name = "Physiological Limit") +
  scale_fill_manual(values = c("LARA" = "brown", name = "Fill of Ideal Salinity")) +
  geom_boxplot(data = LARAwq, aes(x = SY, y = Salinity_ppt, color = Forest_Type)) +
  geom_point(data = LARAwq, aes(x = SY, y = Salinity_ppt, Fill = Forest_Type, group = Forest_Type, shape = Syringe_used), position = position_dodge(width = 0.75), size = 0.9) +
  labs(x = "Year", y = "Salinity (ppt)")


##Water Depth by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Water_depth, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = Water_depth, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##Temp by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = Temp, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = Temp, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##DO% by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_percent, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = DO_percent, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##DO/mg/L by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = DO_mg_L, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = DO_mg_L, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##SPC by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = SPC, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = SPC, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##TDS by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = TDS, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = TDS, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


##pH by Forest Type##
ggplot(data = wqclassification) +
  geom_boxplot(aes(x = SY, y = pH, Fill = Forest_Type, color = Forest_Type)) +
  geom_point(aes(x = SY, y = pH, group = Forest_Type, fill = Forest_Type), position = position_dodge(width = 0.75), size = 0.4)


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

##################################################################################
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




