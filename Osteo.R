library (ggplot2)
library(dplyr)

setwd("C:/Users/CCUMMING/OneDrive - HC-SC PHAC-ASPC/RStudio/")

#stacks dose response data
filename<-"Osteoimage exp1.txt"
#filename<-"Osteoimage exp2.txt"

Dose_Response_Data <- read.delim(filename)

dim(Dose_Response_Data)
str(Dose_Response_Data)

# Puts groups in desired order and spaces equally (Converts x axis to text from number)
Dose_Response_Data_new <- Dose_Response_Data
Dose_Response_Data_new$Dose <- factor(Dose_Response_Data_new$Dose, c("0","0.0048", "0.024", "0.12", "0.6", "3", "15"))

# Dapi results
Graph <- Dose_Response_Data_new %>% ggplot( aes(x=Dose, y=DAPI))  + theme_bw() + geom_point(position="jitter", size=2, colour='black', fill="deepskyblue2", shape=21)  +  geom_boxplot(alpha = 0.3,outlier.shape= NA) + theme(axis.text.x = element_text(angle = 60, hjust=1)) +ylim(0,300000) + labs(title = "Dapi (RFU)", x = "Sodium Arsenite Dose (uM)", y = "Relative Fluorescence (RFU)")
Graph + theme(text = element_text(size = 15)) + facet_grid(rows=vars(Days), cols= vars(Induced), labeller = label_both)

# Osteoimage results
Graph <- Dose_Response_Data_new %>% ggplot( aes(x=Dose, y=Ratio))  + theme_bw() + geom_point(position="jitter", size=2, colour='black', fill="chartreuse3", shape=21)  +  geom_boxplot(alpha = 0.3,outlier.shape= NA) + theme(axis.text.x = element_text(angle = 60, hjust=1)) + labs(title = "Osteoimage/Dapi (RFU)", x = "Sodium Arsenite Dose (uM)", y = "Ratio of Osteoimage/Dapi RFUs")
Graph + theme(text = element_text(size = 15)) + facet_grid(rows=vars(Days), cols= vars(Induced), labeller = label_both)

