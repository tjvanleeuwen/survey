library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(rlang)
library(janitor)
library(forcats)
library(scales)
library(patchwork)


## ===== IMPORTING THE DATA =====

## read two data frames: labs and vals
labs <- read.csv( "../labels.csv", header=TRUE )
vals <- read.csv( "../values.csv", header=TRUE )

## save the questions to a csv and data frame for easy access
write.csv( t( labs[-c(1:12)][1,] ), "../data/questions.csv", row.names = TRUE )
questions <- read.csv( "../questions.csv", header=TRUE )
colnames(questions) <- c( "name", "question" )

## delete the metadata: the first two rows and first twelve columns
write.csv( labs[-c(1, 2), ][, -c(1:12)], "selected_labels.csv", row.names = FALSE )
labs <- read.csv( "selected_labels.csv", header=TRUE, 
                  na.strings = c("", "I don't know", "NA") )
write.csv( vals[-c(1, 2), ][, -c(1:12)], "selected_values.csv", row.names = FALSE )
vals <- read.csv( "selected_values.csv", header=TRUE )
vals[is.na(labs)] <- NA