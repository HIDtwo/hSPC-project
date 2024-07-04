library (ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/CCUMMING/OneDrive - HC-SC PHAC-ASPC/RStudio/RNAseq2024")

#inputs allcounts data file

filename<-"AllCounts_Genename_CDrename.txt"
AllCounts_Genename <- read.delim(filename)

dim(AllCounts_Genename)
str(AllCounts_Genename)

#stacks Counts 

AllCounts_stacked <- AllCounts_Genename %>% pivot_longer(c(Gr1_0As_dayminus4_S1:Gr12_0.6As_day14induced_S48), 
               names_to = "sample", 
               values_to = "Counts")

#Separates sample identifier by "_"

AllCounts_stacked[c('Group', 'Treatment', 'Days', 'Sample')] <- str_split_fixed(AllCounts_stacked$sample, '_', 4)

# some genes of interest
# Jennings et al 2022 - DDIT3, ATF3,HMOX1,TGFB,NQO1,SLC7A11, ASNS, FTL, NQO1,GCLM, MT2A
# Strober et al 2023-  IL8, CDKN2B, EGFR, BRCA1, DDB2, DNMT1, DNMT3B, DNMT3A
#MSC MARKERS -  Gene <- c("(p)CD73","(p)CD90","(p)CD105", "CD45", "CD34", "CD14", "CD19", "CD11B")
#osteomarkers Gene <- c("ALPL","SOST","DMP1", "FGF23", "SP7", "OPG", "RANKL" "BMP2","SPARC")

# TYPE IN THE GENE NAME YOU WANT TO GRAPH HERE-----------------------------------------------------------------------

#Gene <- c("(p)CD73","(p)CD90","(p)CD105", "(p)CD44", "CD45", "CD34", "CD14", "CD19", "CD11B")
#Gene <- c("ALPL","TNFRSF11B", "PHEX","ACAN", "SPP1", "PPARG","MMP13","SPARC") #, "COL1A1"
#Gene <- c("ALPL","TNFRSF11B", "BMP2", "BGLAP")
#Gene <- c("FGF","VEGF", "TGFB", "PDGF","CD31")
#Gene <- c("TP53")
#Gene <- c("RIPK1","RIPK3")
#Gene <- c("TNF","VEGF", "TGFB", "PDGF","CD31")
Gene <- c("COX1", "COX2", "IL1B","IL6","TGFBR1","TGFB1", "TGFB2", "CCL2","ICAM1")

Genedata <- filter(AllCounts_stacked, Gene_name %in% c(Gene))  

# to restrict groups graphed select from below by removing #---------------------------------------------------------

#all groups
Genedata <- filter(Genedata, Days %in% c("dayminus4","day2", "day29", "day14uninduced", "day14induced"))  

# just the osteocyte induction groups
#Genedata <- filter(Genedata, Days %in% c("day14uninduced", "day14induced"))

#just the passaged cell groups
#Genedata <- filter(Genedata, Days %in% c("dayminus4","day2", "day29")) 

#-------------------------------------------------------------------------------------------------------------------

# Puts groups in desired order

Genedata$Group <- factor(Genedata$Group, c("Gr1","Gr2", "Gr3", "Gr4", "Gr5", "Gr6", "Gr7", "Gr8", "Gr9", "Gr10", "Gr11", "Gr12"))
Genedata$Days <- factor(Genedata$Days, c("dayminus4","day2", "day29", "day14uninduced", "day14induced"))

# generates dotplots and boxplots

# generates dotplots and boxplots
Graph <- Genedata %>% ggplot( aes(x=Group, y=Counts, fill=Days)) 
Graph + scale_fill_manual(values=c("deepskyblue", "deeppink","chartreuse1","coral","cyan")) + theme_bw() + geom_point(position="jitter", size=3, shape=21) + geom_boxplot(alpha = 0.3,outlier.shape= NA) + theme(axis.text.x = element_text(angle = 60, hjust=1)) + labs(title = Gene, x = "Sodium Arsenite Dose (uM)", y = "Per Sample Normalized Counts") + scale_x_discrete(labels = c('0uM','0uM','0.12uM','0.6uM','3uM','0uM','0.12uM','0.6uM','3uM', '0uM', '0uM', '0.6uM')) + theme(text = element_text(size = 12)) + facet_wrap(vars(Gene_name))

#incorporate this code for facet wrap, but the y axes will all be same scale
#
#+ facet_grid(rows=vars(rowvariable), cols= vars(column variable), labeller = label_both)