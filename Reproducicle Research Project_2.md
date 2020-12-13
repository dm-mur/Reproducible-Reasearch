---
title: "Exploring impact of various events w.r.t economy and human health in United States using U.S.(NOAA) storm database"
author: "Doris Muriungi"
date: "12/13/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


__Synopsis__

The analysis focusses on the determiantion of the most harmful events to human health and those with the highest economic consequences in the United States.Data for the analysis was obtained from U.S.National Oceanic and Atmospheric Administration’s (NOAA) storm database.Data in National Climatic Data Center (NCD) is received from the National Weather Service. The National Weather service receives their information from a variety of sources, which include but are not limited to: county, state and federal emergency management officials, local law enforcement officials, skywarn spotters, NWS damage surveys, newspaper clipping services, the insurance industry and the general public.

__Data Processing__

```{r}
#Loading libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
```

Loading storm data
```{r cars}
#Reading the data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,destfile = "C:/Users/DMuriungi/Desktop/Doris/Doris DOCs/dm/Data Science/Reproducible Research/RepR_Project2/StormData.csv.bz2")
storm_data<-read.csv(bzfile("C:/Users/DMuriungi/Desktop/Doris/Doris DOCs/dm/Data Science/Reproducible Research/RepR_Project2/StormData.csv.bz2"))
str(storm_data)
```

Most harmful event types with respect to population health in the United States

The variables considered in the analysis are as follows:
-Population health related variables are __Fatalities__ and __Injuries__
-Adverse weather related variable __Event type__ 

Injuries and event type
```{r}
INJ<-storm_data %>%
        select(INJURIES,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_INJ=sum(INJURIES,na.rm = TRUE)) %>%
        arrange(desc(Total_INJ))
INJ_data<-INJ[1:10,]
```

Fatalites and event type
```{r}
FATL<-storm_data %>%
        select(FATALITIES,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_FATL=sum(FATALITIES,na.rm = TRUE)) %>%
        arrange(desc(Total_FATL))
FATL_data<-FATL[1:10,]
```

Events with the greatest economic consequences across the United States

The variables considered in the analysis are as follows:
-Economic aspect related variables are approximate __Property damage__ and __Crop damage__
-Adverse weather related variable __Event type__ 

Property damage and event type
```{r}
PROP<-storm_data %>%
        select(PROPDMG,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_Property_Damage=sum(PROPDMG,na.rm = TRUE))
       

PROP <- storm_data %>% 
        mutate(PROP = ifelse(toupper(PROPDMGEXP) =='K', PROPDMG*1000, ifelse(toupper(PROPDMGEXP) =='M', PROPDMG*1000000, ifelse(toupper(PROPDMGEXP) == 'B', PROPDMG*1000000000, ifelse(toupper(PROPDMGEXP) == 'H', PROPDMG*100, PROPDMG)))))

PROP<-storm_data %>%
        select(PROPDMG,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_Property_Damage=sum(PROPDMG,na.rm = TRUE)) %>%
        arrange(desc(Total_Property_Damage))
PROP_data<-PROP[1:10,]
```

Crop Damage and event type
```{r}
CROP<-storm_data %>%
        select(CROPDMG,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_Crop_Damage=sum(CROPDMG,na.rm = TRUE))


CROP <- storm_data%>%
        mutate(CROP = ifelse(toupper(CROPDMGEXP) =='K', CROPDMG*1000, ifelse(toupper(CROPDMGEXP) =='M', CROPDMG*1000000, ifelse(toupper(CROPDMGEXP) == 'B', CROPDMG*1000000000, ifelse(toupper(CROPDMGEXP) == 'H', CROPDMG*100, CROPDMG)))))

CROP<-storm_data %>%
        select(CROPDMG,EVTYPE) %>%
        group_by(EVTYPE) %>%
        summarise(Total_Crop_Damage=sum(CROPDMG,na.rm = TRUE)) %>%
        arrange(desc(Total_Crop_Damage))
CROP_data<-CROP[1:10,]
```

__Results__
Visualizing Injuries and Fatalities with event types
```{r}
#Plotting Fatalities with event type
plot1<-ggplot(FATL_data,aes(reorder(EVTYPE,Total_FATL),Total_FATL,fill=EVTYPE))+
        geom_bar(stat = "identity")+
        geom_label(aes(EVTYPE,Total_FATL,label=Total_FATL),
                   color="white",fontface="bold")+
        xlab("Event Type")+ ylab("Total Fatalites")+
        ggtitle("Fatalities reported in United States in relation to Event Type")+
        theme(legend.position = "none")+
        coord_flip()

#Plotting Injuries with event type
plot2<-ggplot(INJ_data,aes(reorder(EVTYPE,Total_INJ),Total_INJ,fill=EVTYPE))+
        geom_bar(stat = "identity")+
        geom_label(aes(EVTYPE,Total_INJ,label=Total_INJ),
                   color="white",fontface="bold")+
        xlab("Event Type")+ ylab("Total Fatalites")+
        ggtitle("Injuries reported in United States in relation to Event Type")+
        theme(legend.position = "none")+
        coord_flip()
grid.arrange(plot1,plot2,ncol=1)
```

From the above plots, __Tornado__ is the weather event with the most adverse effects to human population health recording the highest overall number of fatalities and injuries accross United States.

Visualizing Property damage and Crop damage with events
```{r}
#Plotting Property damage with event type
plot3<-ggplot(PROP_data,aes(reorder(EVTYPE,Total_Property_Damage),Total_Property_Damage,fill=EVTYPE))+
        geom_bar(stat = "identity")+
        geom_label(aes(EVTYPE,Total_Property_Damage,label=Total_Property_Damage),
                   color="white",fontface="bold")+
        xlab("Event Type")+ ylab("Total Property Damage")+
        ggtitle("Property damage reported in United States in relation to Event Type")+
        theme(legend.position = "none")+
        coord_flip()

#Plotting Crop damaage with event type
plot4<-ggplot(INJ_data,aes(reorder(EVTYPE,CROP_data$Total_Crop_Damage),CROP_data$Total_Crop_Damage,fill=EVTYPE))+
        geom_bar(stat = "identity")+
        geom_label(aes(EVTYPE,CROP_data$Total_Crop_Damage,label=CROP_data$Total_Crop_Damage),
                   color="white",fontface="bold")+
        xlab("Event Type")+ ylab("Total Crop Damage")+
        ggtitle("Crop damage reported in United States in relation to Event Type")+
        theme(legend.position = "none")+
        coord_flip()
grid.arrange(plot3,plot4,ncol=1)
```

From the above plots, __Tornado__ is the weather event with the most adverse effects to on the economy recording the highest overall property and crop damages accross United States.