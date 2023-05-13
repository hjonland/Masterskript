## Author: Helga Sofie Gisholt J??nland
## Date: 13.05.2023

##### Lager mdf #####
library(dplyr)
library(sp)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(sf)
library(randomForest)
library(esquisse)
library(lattice)
library(caret)
library(rmarkdown)
library(stats)
library(zoo)


base_path <- "filbane"

#NFI plot table
fla <- readRDS(paste0(base_path, "Filnavn"))  %>%  # Landsskogstakseringsinformasjon on pr??veflater
  rename(FLATEID = flateid)

#NFI tree table
dat <- readRDS(paste0(base_path, "Filnavn")) # Informasjon om alle tr??r i Landsskogstakseringen

#species names
species_lookup <- readRDS(paste0(base_path, "Filnavn")) # Informasjon om treslagene

#weekly S2 bands without ndvi for 2019 in wide format BX_1=week 18
s2w <- readRDS(paste0(base_path, 'filnavn'))  %>% # S2-tidsseriene for hver av pr??veflatene
  rename(FLATEID = flateid)

#ndvi per week
s2w_ndvi <- readRDS(paste0(base_path, 'filnavn')) %>% # NDVI fra S2-tidsserier
  rename(FLATEID=plotid)

#additional variables for SI modeling (partly redundant with fla)
fla.si <- readRDS(paste0(base_path, "filnavn")) # Bonitetsinformasjon p?? pr??veflater



##### Variabler #####

# Alle variabler benyttet i analyse og bearbeiding av dataene
variabler <- c("FLATEID"                     ,"TRESLAG"                    ,"VMTPRHA"                    ,"VMTPRHA_all"                ,"prop"                      
               , "trslno"                    ,"beskrivelse"                ,"luc.center"       
               , "dted10_hoh"                ,"approx_latitude"            ,"approx_longitude" 
               , "B02_1"                     ,"B02_2"                      ,"B02_3"                      ,"B02_4"                      ,"B02_5"                      ,"B02_6"                     
               , "B02_7"                     ,"B02_8"                      ,"B02_9"                      ,"B02_10"                     ,"B02_11"                    
               , "B02_12"                    ,"B02_13"                     ,"B02_14"                     ,"B02_15"                     ,"B02_16"                    
               , "B02_17"                    ,"B02_18"                     ,"B02_19"                     ,"B02_20"                     ,"B02_21"                    
               , "B02_22"                    ,"B03_1"                      ,"B03_2"                      ,"B03_3"                      ,"B03_4"                     
               , "B03_5"                     ,"B03_6"                      ,"B03_7"                      ,"B03_8"                      ,"B03_9"                     
               , "B03_10"                    ,"B03_11"                     ,"B03_12"                     ,"B03_13"                     ,"B03_14"                    
               , "B03_15"                    ,"B03_16"                     ,"B03_17"                     ,"B03_18"                     ,"B03_19"                    
               , "B03_20"                    ,"B03_21"                     ,"B03_22"                     ,"B04_1"                      ,"B04_2"                     
               , "B04_3"                     ,"B04_4"                      ,"B04_5"                      ,"B04_6"                      ,"B04_7"                     
               , "B04_8"                     ,"B04_9"                      ,"B04_10"                     ,"B04_11"                     ,"B04_12"                    
               , "B04_13"                    ,"B04_14"                     ,"B04_15"                     ,"B04_16"                     ,"B04_17"                    
               , "B04_18"                    ,"B04_19"                     ,"B04_20"                     ,"B04_21"                     ,"B04_22"                    
               , "B8A_1"                     ,"B8A_2"                      ,"B8A_3"                      ,"B8A_4"                      ,"B8A_5"                     
               , "B8A_6"                     ,"B8A_7"                      ,"B8A_8"                      ,"B8A_9"                      ,"B8A_10"                    
               , "B8A_11"                    ,"B8A_12"                     ,"B8A_13"                     ,"B8A_14"                     ,"B8A_15"                    
               , "B8A_16"                    ,"B8A_17"                     ,"B8A_18"                     ,"B8A_19"                     ,"B8A_20"                    
               , "B8A_21"                    ,"B8A_22"                     ,"B11_1"                      ,"B11_2"                      ,"B11_3"                     
               , "B11_4"                     ,"B11_5"                      ,"B11_6"                      ,"B11_7"                      ,"B11_8"                     
               , "B11_9"                     ,"B11_10"                     ,"B11_11"                     ,"B11_12"                     ,"B11_13"                    
               , "B11_14"                    ,"B11_15"                     ,"B11_16"                     ,"B11_17"                     ,"B11_18"                    
               , "B11_19"                    ,"B11_20"                     ,"B11_21"                     ,"B11_22"                     ,"B12_1"                     
               , "B12_2"                     ,"B12_3"                      ,"B12_4"                      ,"B12_5"                      ,"B12_6"                     
               , "B12_7"                     ,"B12_8"                      ,"B12_9"                      ,"B12_10"                     ,"B12_11"                    
               , "B12_12"                    ,"B12_13"                     ,"B12_14"                     ,"B12_15"                     ,"B12_16"                    
               , "B12_17"                    ,"B12_18"                     ,"B12_19"                     ,"B12_20"                     ,"B12_21"                    
               , "B12_22"                    ,"ndvi_18"                    ,"ndvi_19"                    ,"ndvi_20"                    ,"ndvi_21"                   
               , "ndvi_22"                   ,"ndvi_23"                    ,"ndvi_24"                    ,"ndvi_25"                    ,"ndvi_26"                   
               , "ndvi_27"                   ,"ndvi_28"                    ,"ndvi_29"                    ,"ndvi_30"                    ,"ndvi_31"                   
               , "ndvi_32"                   ,"ndvi_33"                    ,"ndvi_34"                    ,"ndvi_35"                    ,"ndvi_36"                   
               , "ndvi_37"                   ,"ndvi_38"                    ,"ndvi_39"                    
               , "hmean_first"               ,"h25_first"                  ,"h50_first"                  ,"h75_first"                  ,"h90_first"
               , "h95_first"                 ,"cc5"                        ,"cc10"                      
               , "slope"                     ,"avstkyst"                   ,"dtw"
               , "temp_jan"                  ,"temp_feb"                   ,"temp_mar"                   ,"temp_apr"                   ,"temp_may"                  
               , "temp_jun"                  ,"temp_jul"                   ,"temp_aug"                   ,"temp_sep"                   ,"temp_oct"                  
               , "temp_nov"                  ,"temp_dec"                   ,"prec_jan"                  
               , "prec_feb"                  ,"prec_mar"                   ,"prec_apr"                   ,"prec_may"                   ,"prec_jun"                  
               , "prec_jul"                  ,"prec_aug"                   ,"prec_sep"                   ,"prec_oct"                   ,"prec_nov"              
               , "prec_dec")

# Variabler i modell I)
variabler1 <- c("B02_1"                      ,"B02_2"                      ,"B02_3"                      ,"B02_4"                      ,"B02_5"                      ,"B02_6"                     
                , "B02_7"                     ,"B02_8"                      ,"B02_9"                      ,"B02_10"                     ,"B02_11"                    
                , "B02_12"                    ,"B02_13"                     ,"B02_14"                     ,"B02_15"                     ,"B02_16"                    
                , "B02_17"                    ,"B02_18"                     ,"B02_19"                     ,"B02_20"                     ,"B02_21"                    
                , "B02_22"                    ,"B03_1"                      ,"B03_2"                      ,"B03_3"                      ,"B03_4"                     
                , "B03_5"                     ,"B03_6"                      ,"B03_7"                      ,"B03_8"                      ,"B03_9"                     
                , "B03_10"                    ,"B03_11"                     ,"B03_12"                     ,"B03_13"                     ,"B03_14"                    
                , "B03_15"                    ,"B03_16"                     ,"B03_17"                     ,"B03_18"                     ,"B03_19"                    
                , "B03_20"                    ,"B03_21"                     ,"B03_22"                     ,"B04_1"                      ,"B04_2"                     
                , "B04_3"                     ,"B04_4"                      ,"B04_5"                      ,"B04_6"                      ,"B04_7"                     
                , "B04_8"                     ,"B04_9"                      ,"B04_10"                     ,"B04_11"                     ,"B04_12"                    
                , "B04_13"                    ,"B04_14"                     ,"B04_15"                     ,"B04_16"                     ,"B04_17"                    
                , "B04_18"                    ,"B04_19"                     ,"B04_20"                     ,"B04_21"                     ,"B04_22"                    
                , "B8A_1"                     ,"B8A_2"                      ,"B8A_3"                      ,"B8A_4"                      ,"B8A_5"                     
                , "B8A_6"                     ,"B8A_7"                      ,"B8A_8"                      ,"B8A_9"                      ,"B8A_10"                    
                , "B8A_11"                    ,"B8A_12"                     ,"B8A_13"                     ,"B8A_14"                     ,"B8A_15"                    
                , "B8A_16"                    ,"B8A_17"                     ,"B8A_18"                     ,"B8A_19"                     ,"B8A_20"                    
                , "B8A_21"                    ,"B8A_22"                     ,"B11_1"                      ,"B11_2"                      ,"B11_3"                     
                , "B11_4"                     ,"B11_5"                      ,"B11_6"                      ,"B11_7"                      ,"B11_8"                     
                , "B11_9"                     ,"B11_10"                     ,"B11_11"                     ,"B11_12"                     ,"B11_13"                    
                , "B11_14"                    ,"B11_15"                     ,"B11_16"                     ,"B11_17"                     ,"B11_18"                    
                , "B11_19"                    ,"B11_20"                     ,"B11_21"                     ,"B11_22"                     ,"B12_1"                     
                , "B12_2"                     ,"B12_3"                      ,"B12_4"                      ,"B12_5"                      ,"B12_6"                     
                , "B12_7"                     ,"B12_8"                      ,"B12_9"                      ,"B12_10"                     ,"B12_11"                    
                , "B12_12"                    ,"B12_13"                     ,"B12_14"                     ,"B12_15"                     ,"B12_16"                    
                , "B12_17"                    ,"B12_18"                     ,"B12_19"                     ,"B12_20"                     ,"B12_21"                    
                , "B12_22"                    ,"ndvi_18"                    ,"ndvi_19"                    ,"ndvi_20"                    ,"ndvi_21"                   
                , "ndvi_22"                   ,"ndvi_23"                    ,"ndvi_24"                    ,"ndvi_25"                    ,"ndvi_26"                   
                , "ndvi_27"                   ,"ndvi_28"                    ,"ndvi_29"                    ,"ndvi_30"                    ,"ndvi_31"                   
                , "ndvi_32"                   ,"ndvi_33"                    ,"ndvi_34"                    ,"ndvi_35"                    ,"ndvi_36"                   
                , "ndvi_37"                   ,"ndvi_38"                    ,"ndvi_39")

# Variabler i modell II)
variabler2 <- c("dted10_hoh"                  ,"B02_1"                      ,"B02_2"                      ,"B02_3"                      ,"B02_4"                      ,"B02_5"                      ,"B02_6"                     
                , "B02_7"                     ,"B02_8"                      ,"B02_9"                      ,"B02_10"                     ,"B02_11"                    
                , "B02_12"                    ,"B02_13"                     ,"B02_14"                     ,"B02_15"                     ,"B02_16"                    
                , "B02_17"                    ,"B02_18"                     ,"B02_19"                     ,"B02_20"                     ,"B02_21"                    
                , "B02_22"                    ,"B03_1"                      ,"B03_2"                      ,"B03_3"                      ,"B03_4"                     
                , "B03_5"                     ,"B03_6"                      ,"B03_7"                      ,"B03_8"                      ,"B03_9"                     
                , "B03_10"                    ,"B03_11"                     ,"B03_12"                     ,"B03_13"                     ,"B03_14"                    
                , "B03_15"                    ,"B03_16"                     ,"B03_17"                     ,"B03_18"                     ,"B03_19"                    
                , "B03_20"                    ,"B03_21"                     ,"B03_22"                     ,"B04_1"                      ,"B04_2"                     
                , "B04_3"                     ,"B04_4"                      ,"B04_5"                      ,"B04_6"                      ,"B04_7"                     
                , "B04_8"                     ,"B04_9"                      ,"B04_10"                     ,"B04_11"                     ,"B04_12"                    
                , "B04_13"                    ,"B04_14"                     ,"B04_15"                     ,"B04_16"                     ,"B04_17"                    
                , "B04_18"                    ,"B04_19"                     ,"B04_20"                     ,"B04_21"                     ,"B04_22"                    
                , "B8A_1"                     ,"B8A_2"                      ,"B8A_3"                      ,"B8A_4"                      ,"B8A_5"                     
                , "B8A_6"                     ,"B8A_7"                      ,"B8A_8"                      ,"B8A_9"                      ,"B8A_10"                    
                , "B8A_11"                    ,"B8A_12"                     ,"B8A_13"                     ,"B8A_14"                     ,"B8A_15"                    
                , "B8A_16"                    ,"B8A_17"                     ,"B8A_18"                     ,"B8A_19"                     ,"B8A_20"                    
                , "B8A_21"                    ,"B8A_22"                     ,"B11_1"                      ,"B11_2"                      ,"B11_3"                     
                , "B11_4"                     ,"B11_5"                      ,"B11_6"                      ,"B11_7"                      ,"B11_8"                     
                , "B11_9"                     ,"B11_10"                     ,"B11_11"                     ,"B11_12"                     ,"B11_13"                    
                , "B11_14"                    ,"B11_15"                     ,"B11_16"                     ,"B11_17"                     ,"B11_18"                    
                , "B11_19"                    ,"B11_20"                     ,"B11_21"                     ,"B11_22"                     ,"B12_1"                     
                , "B12_2"                     ,"B12_3"                      ,"B12_4"                      ,"B12_5"                      ,"B12_6"                     
                , "B12_7"                     ,"B12_8"                      ,"B12_9"                      ,"B12_10"                     ,"B12_11"                    
                , "B12_12"                    ,"B12_13"                     ,"B12_14"                     ,"B12_15"                     ,"B12_16"                    
                , "B12_17"                    ,"B12_18"                     ,"B12_19"                     ,"B12_20"                     ,"B12_21"                    
                , "B12_22"                    ,"ndvi_18"                    ,"ndvi_19"                    ,"ndvi_20"                    ,"ndvi_21"                   
                , "ndvi_22"                   ,"ndvi_23"                    ,"ndvi_24"                    ,"ndvi_25"                    ,"ndvi_26"                   
                , "ndvi_27"                   ,"ndvi_28"                    ,"ndvi_29"                    ,"ndvi_30"                    ,"ndvi_31"                   
                , "ndvi_32"                   ,"ndvi_33"                    ,"ndvi_34"                    ,"ndvi_35"                    ,"ndvi_36"                   
                , "ndvi_37"                   ,"ndvi_38"                    ,"ndvi_39"                   
                , "slope"                     ,"avstkyst"
                , "temp_jan"                  ,"temp_feb"                   ,"temp_mar"                   ,"temp_apr"                   ,"temp_may"                  
                , "temp_jun"                  ,"temp_jul"                   ,"temp_aug"                   ,"temp_sep"                   ,"temp_oct"                  
                , "temp_nov"                  ,"temp_dec"                   ,"prec_jan"                  
                , "prec_feb"                  ,"prec_mar"                   ,"prec_apr"                   ,"prec_may"                   ,"prec_jun"                  
                , "prec_jul"                  ,"prec_aug"                   ,"prec_sep"                   ,"prec_oct"                   ,"prec_nov"              
                , "prec_dec")

# Variabler i modell III)
variabler3 <- c("B02_1"                       ,"B02_2"                      ,"B02_3"                      ,"B02_4"                      ,"B02_5"                      ,"B02_6"                     
                , "B02_7"                     ,"B02_8"                      ,"B02_9"                      ,"B02_10"                     ,"B02_11"                    
                , "B02_12"                    ,"B02_13"                     ,"B02_14"                     ,"B02_15"                     ,"B02_16"                    
                , "B02_17"                    ,"B02_18"                     ,"B02_19"                     ,"B02_20"                     ,"B02_21"                    
                , "B02_22"                    ,"B03_1"                      ,"B03_2"                      ,"B03_3"                      ,"B03_4"                     
                , "B03_5"                     ,"B03_6"                      ,"B03_7"                      ,"B03_8"                      ,"B03_9"                     
                , "B03_10"                    ,"B03_11"                     ,"B03_12"                     ,"B03_13"                     ,"B03_14"                    
                , "B03_15"                    ,"B03_16"                     ,"B03_17"                     ,"B03_18"                     ,"B03_19"                    
                , "B03_20"                    ,"B03_21"                     ,"B03_22"                     ,"B04_1"                      ,"B04_2"                     
                , "B04_3"                     ,"B04_4"                      ,"B04_5"                      ,"B04_6"                      ,"B04_7"                     
                , "B04_8"                     ,"B04_9"                      ,"B04_10"                     ,"B04_11"                     ,"B04_12"                    
                , "B04_13"                    ,"B04_14"                     ,"B04_15"                     ,"B04_16"                     ,"B04_17"                    
                , "B04_18"                    ,"B04_19"                     ,"B04_20"                     ,"B04_21"                     ,"B04_22"                    
                , "B8A_1"                     ,"B8A_2"                      ,"B8A_3"                      ,"B8A_4"                      ,"B8A_5"                     
                , "B8A_6"                     ,"B8A_7"                      ,"B8A_8"                      ,"B8A_9"                      ,"B8A_10"                    
                , "B8A_11"                    ,"B8A_12"                     ,"B8A_13"                     ,"B8A_14"                     ,"B8A_15"                    
                , "B8A_16"                    ,"B8A_17"                     ,"B8A_18"                     ,"B8A_19"                     ,"B8A_20"                    
                , "B8A_21"                    ,"B8A_22"                     ,"B11_1"                      ,"B11_2"                      ,"B11_3"                     
                , "B11_4"                     ,"B11_5"                      ,"B11_6"                      ,"B11_7"                      ,"B11_8"                     
                , "B11_9"                     ,"B11_10"                     ,"B11_11"                     ,"B11_12"                     ,"B11_13"                    
                , "B11_14"                    ,"B11_15"                     ,"B11_16"                     ,"B11_17"                     ,"B11_18"                    
                , "B11_19"                    ,"B11_20"                     ,"B11_21"                     ,"B11_22"                     ,"B12_1"                     
                , "B12_2"                     ,"B12_3"                      ,"B12_4"                      ,"B12_5"                      ,"B12_6"                     
                , "B12_7"                     ,"B12_8"                      ,"B12_9"                      ,"B12_10"                     ,"B12_11"                    
                , "B12_12"                    ,"B12_13"                     ,"B12_14"                     ,"B12_15"                     ,"B12_16"                    
                , "B12_17"                    ,"B12_18"                     ,"B12_19"                     ,"B12_20"                     ,"B12_21"                    
                , "B12_22"                    ,"ndvi_18"                    ,"ndvi_19"                    ,"ndvi_20"                    ,"ndvi_21"                   
                , "ndvi_22"                   ,"ndvi_23"                    ,"ndvi_24"                    ,"ndvi_25"                    ,"ndvi_26"                   
                , "ndvi_27"                   ,"ndvi_28"                    ,"ndvi_29"                    ,"ndvi_30"                    ,"ndvi_31"                   
                , "ndvi_32"                   ,"ndvi_33"                    ,"ndvi_34"                    ,"ndvi_35"                    ,"ndvi_36"                   
                , "ndvi_37"                   ,"ndvi_38"                    ,"ndvi_39"                    
                , "hmean_first"               ,"h25_first"                  ,"h50_first"                  ,"h75_first"                  ,"h90_first"
                , "h95_first"                 ,"cc5"                        ,"cc10")

# Variabler i modell IV)
variabler4 <- c("dted10_hoh"                  ,"B02_1"                      ,"B02_2"                      ,"B02_3"                      ,"B02_4"                      ,"B02_5"                      ,"B02_6"                     
                , "B02_7"                     ,"B02_8"                      ,"B02_9"                      ,"B02_10"                     ,"B02_11"                    
                , "B02_12"                    ,"B02_13"                     ,"B02_14"                     ,"B02_15"                     ,"B02_16"                    
                , "B02_17"                    ,"B02_18"                     ,"B02_19"                     ,"B02_20"                     ,"B02_21"                    
                , "B02_22"                    ,"B03_1"                      ,"B03_2"                      ,"B03_3"                      ,"B03_4"                     
                , "B03_5"                     ,"B03_6"                      ,"B03_7"                      ,"B03_8"                      ,"B03_9"                     
                , "B03_10"                    ,"B03_11"                     ,"B03_12"                     ,"B03_13"                     ,"B03_14"                    
                , "B03_15"                    ,"B03_16"                     ,"B03_17"                     ,"B03_18"                     ,"B03_19"                    
                , "B03_20"                    ,"B03_21"                     ,"B03_22"                     ,"B04_1"                      ,"B04_2"                     
                , "B04_3"                     ,"B04_4"                      ,"B04_5"                      ,"B04_6"                      ,"B04_7"                     
                , "B04_8"                     ,"B04_9"                      ,"B04_10"                     ,"B04_11"                     ,"B04_12"                    
                , "B04_13"                    ,"B04_14"                     ,"B04_15"                     ,"B04_16"                     ,"B04_17"                    
                , "B04_18"                    ,"B04_19"                     ,"B04_20"                     ,"B04_21"                     ,"B04_22"                    
                , "B8A_1"                     ,"B8A_2"                      ,"B8A_3"                      ,"B8A_4"                      ,"B8A_5"                     
                , "B8A_6"                     ,"B8A_7"                      ,"B8A_8"                      ,"B8A_9"                      ,"B8A_10"                    
                , "B8A_11"                    ,"B8A_12"                     ,"B8A_13"                     ,"B8A_14"                     ,"B8A_15"                    
                , "B8A_16"                    ,"B8A_17"                     ,"B8A_18"                     ,"B8A_19"                     ,"B8A_20"                    
                , "B8A_21"                    ,"B8A_22"                     ,"B11_1"                      ,"B11_2"                      ,"B11_3"                     
                , "B11_4"                     ,"B11_5"                      ,"B11_6"                      ,"B11_7"                      ,"B11_8"                     
                , "B11_9"                     ,"B11_10"                     ,"B11_11"                     ,"B11_12"                     ,"B11_13"                    
                , "B11_14"                    ,"B11_15"                     ,"B11_16"                     ,"B11_17"                     ,"B11_18"                    
                , "B11_19"                    ,"B11_20"                     ,"B11_21"                     ,"B11_22"                     ,"B12_1"                     
                , "B12_2"                     ,"B12_3"                      ,"B12_4"                      ,"B12_5"                      ,"B12_6"                     
                , "B12_7"                     ,"B12_8"                      ,"B12_9"                      ,"B12_10"                     ,"B12_11"                    
                , "B12_12"                    ,"B12_13"                     ,"B12_14"                     ,"B12_15"                     ,"B12_16"                    
                , "B12_17"                    ,"B12_18"                     ,"B12_19"                     ,"B12_20"                     ,"B12_21"                    
                , "B12_22"                    ,"ndvi_18"                    ,"ndvi_19"                    ,"ndvi_20"                    ,"ndvi_21"                   
                , "ndvi_22"                   ,"ndvi_23"                    ,"ndvi_24"                    ,"ndvi_25"                    ,"ndvi_26"                   
                , "ndvi_27"                   ,"ndvi_28"                    ,"ndvi_29"                    ,"ndvi_30"                    ,"ndvi_31"                   
                , "ndvi_32"                   ,"ndvi_33"                    ,"ndvi_34"                    ,"ndvi_35"                    ,"ndvi_36"                   
                , "ndvi_37"                   ,"ndvi_38"                    ,"ndvi_39"                    
                , "hmean_first"               ,"h25_first"                  ,"h50_first"                  ,"h75_first"                  ,"h90_first"
                , "h95_first"                 ,"cc5"                        ,"cc10"                      
                , "slope"                     ,"avstkyst"
                , "temp_jan"                  ,"temp_feb"                   ,"temp_mar"                   ,"temp_apr"                   ,"temp_may"                  
                , "temp_jun"                  ,"temp_jul"                   ,"temp_aug"                   ,"temp_sep"                   ,"temp_oct"                  
                , "temp_nov"                  ,"temp_dec"                   ,"prec_jan"                  
                , "prec_feb"                  ,"prec_mar"                   ,"prec_apr"                   ,"prec_may"                   ,"prec_jun"                  
                , "prec_jul"                  ,"prec_aug"                   ,"prec_sep"                   ,"prec_oct"                   ,"prec_nov"              
                , "prec_dec")




##### Klargj??r data #####

# Lager oversikt over alle treslagene som er representert i for hver pr??veflate, 
# og andelen volum hvert av treslagene opptar p?? pr??veflata
agg <- 
  dat %>%
  group_by(FLATEID, TRESLAG) %>% 
  summarise(VMTPRHA=sum(VMTPRHA)) %>% 
  group_by(FLATEID) %>% 
  mutate(VMTPRHA_all=sum(VMTPRHA),
         prop=VMTPRHA/VMTPRHA_all)

# Lager en oversikt over antall pr??veflater der det er 75% volumdominans for hvert treslag
info <- 
  agg %>% 
  filter(prop>0.75) %>% # Velger pr??veflater og tilh??rende treslag med 75% eller mer volumdominans
  group_by(TRESLAG) %>% 
  summarize(n=n()) %>% 
  full_join(species_lookup %>% select(TRESLAG, trslno, beskrivelse)) %>% 
  data.frame()
info

sum(info$n, na.rm=T) # Antall pr??veflater med 75% dominans av ett treslag = 9076

# Oversikt over aktuelle pr??veflater med tilh??rende treslagsinformasjon
spec.prop <- 
  agg %>% 
  filter(prop>0.75) %>%
  left_join(species_lookup %>% select(TRESLAG, trslno, beskrivelse))

# Lager om til en data frame til modellering
spec.fla <-
  spec.prop %>%
  data.frame()
nrow(spec.fla)

# Hoveddatasettet med S2-data og NDVI, klimatiske variabler og laser for hver av flatene med volumdominans
mdf <- 
  spec.fla %>% 
  inner_join(fla, by=c("FLATEID")) %>% # Tiln??rmede koordinater, hoh. og antall ulike deler med arealtyper per pr??veflate
  filter(n.parts==1) %>% # Velger ut pr??veflater som kun inneholder skog, og ikke er splittet med andre arealtyper
  inner_join(s2w, by=c("FLATEID")) %>% # Sentinel-2 tidsserier
  inner_join(s2w_ndvi, by=c("FLATEID")) %>% # NDVIer til S2-tidsserier
  inner_join(fla.si) %>% # Klimatiske variabler og laser
  droplevels() %>%
  mutate(trslno=as.factor(trslno)) %>% # Gj??r treslag til en factor for RF-klassifisering
  select(variabler)
nrow(mdf) # Antall pr??veflater med 1 del og 75 % volumdominans = 7977


##### Plotter kart med treslagsforekomster #####
mdf.prop <- 
  spec.prop %>% 
  inner_join(fla %>% select(FLATEID, approx_latitude, approx_longitude, dted10_hoh), 
             by="FLATEID") # Plassering p?? pr??veflater til visualisering

# Gj??r om lenge- og breddegrader til koordinater i plot
dist.map <- 
  mdf.prop %>%
  filter(prop>0.75) %>%
  ggplot(aes(y=approx_latitude, x=approx_longitude, col=trslno, alpha=prop)) + 
  geom_point() + coord_fixed() + scale_colour_discrete(guide = FALSE)+
  facet_wrap(~trslno) #+ theme(legend.position = "none")


# Henter inn polygon over Norge
Norway <- rworldmap::getMap(resolution = "low") %>% .[!is.na(.$NAME),] %>% 
  .[.$NAME=="Norway",] %>%
  sf::st_as_sf() %>% sf::st_cast("MULTIPOLYGON") 
ggplot(Norway)+geom_sf()


# Plasserer pr??veflatekoordinatene i Norgepolygonet
fla.geo <-
  mdf.prop %>%
  st_as_sf(coords=c("approx_longitude","approx_latitude"),crs=4326)%>%
  cbind(.,st_coordinates(.))
nrow(fla.geo)

# Plotter pr??veflatenes plassering for hvert av treslagene
ggplot(fla.geo)+
  geom_sf(data=Norway, fill=NA) +
  geom_sf(aes(fill=trslno, col=trslno, alpha=prop)) + 
  facet_wrap(~trslno)+
  theme_bw()+
  theme(legend.position = "none")+
  coord_sf(xlim = c(3, 33), ylim = c(57.5, 72), expand = FALSE) # Avgrensninger utelater Svalbard



##### Variabelkorrelasjoner #####

library(corrplot)
# begrenset utvalg av variabler for modellering
var_beg = c("dted10_hoh"                   ,"approx_latitude"            ,"approx_longitude"                           
            , "B02_1"                      ,"B02_2"                   
            , "B03_1"                      ,"B03_2"
            , "B04_1"                      ,"B04_2"
            , "B8A_1"                      ,"B8A_2"
            , "B11_1"                      ,"B11_2"
            , "B12_1"                      ,"B12_2"
            , "ndvi_18"                    ,"ndvi_19"                    ,"h25_first"                  ,"h50_first"                 
            , "h75_first"                  ,"h95_first"                  ,"cc5"                        ,"cc10"                      
            , "slope"                      ,"avstkyst"                   
            , "temp_jan"                   ,"temp_feb"
            , "prec_jan"                   ,"prec_feb")

mdf_beg <- mdf %>% 
  select(var_beg)


res2_beg <- rcorr(as.matrix(mdf_beg))

r_matrise_df_beg <- data.frame(res2_beg$r)
r_matrise_beg <- as.matrix(r_matrise_df_beg)


corrplot(r_matrise_beg, method = 'color')



##### Gjennomsnittstidsserier per treslag #####

# Finner gjennomsnittlige verdier for hvert treslag for hver uke
mean.bands <- 
  mdf %>% 
  group_by(trslno) %>%
  summarise(#n=n(),
    across(B02_1:B12_22, mean)) %>%
  pivot_longer(cols=-trslno) %>% 
  separate(col=name, into=c("bands","week_id")) %>% 
  mutate(week=as.numeric(week_id)+17) %>%
  group_by(bands)
nrow(mean.bands)

# Velger ut treslag med mange observasjoner
selected <- c("Gran","Introdusert gran (Picea spp.)","Furu", "Dunbj\u00F8rk","Lavlandsbj\u00F8rk","Osp","Eik","Gr\u00E5or","Selje","Rogn")
mean.bands.selected <- mean.bands %>% 
  filter(trslno %in% selected)

## Plotter gjennomsnittet for utvalgte treslag for hvert b??nd per uke
ggplot(mean.bands.selected) +
  aes(x = week, y = value, colour = trslno, group = trslno) +
  geom_line(size = 0.8) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(bands), scales = "free_y")




##### Selje #####

# Henter ut gjennomsnittet til selje per uke
mean.band.selje <-
  mean.bands %>% 
  filter(trslno=="Selje") %>% 
  mutate(FLATEID = 1) %>% 
  select(c(FLATEID, bands, week_id, value, week))

# Henter alle seljepr??veflatene og verdier per uke
band.selje <- mdf %>%
  filter(trslno == "Selje") %>% 
  select(FLATEID, B02_1:B12_22) %>%
  pivot_longer(cols=-c(FLATEID)) %>%
  separate(col=name, into=c("bands","week_id")) %>% 
  mutate(week=as.numeric(week_id)+17)

# Legger disse med ulike verdier i samme datasett
selje <- rbind(band.selje, mean.band.selje) %>% 
  mutate(mean = as.factor(ifelse(FLATEID == 1, 2, 1))) %>% 
  group_by(mean)

# Lager grupperinger for gjennomsnitt og ikke
selje$group2 <- ifelse(selje$mean == 2, "2", "1")

# Plotter gjennomsnittet i annen farge enn enkeltflatene, og p?? toppen av dem s?? den synes
ggplot(selje) +
  aes(x = week, y = value, colour = factor(mean), group = paste(group2, FLATEID, sep = "_")) +
  geom_line(size = 1) +  # set default line thickness
  scale_color_manual(
    values = c(`1` = "#555353", `2` = "#14CEFD")
  ) +
  theme_minimal() +
  facet_wrap(vars(bands))



##### Finner verdier med mange NAs #####

# Teller opp antall observasjoner med variabler med NA-verdier 
k <- 0
tab <- data.frame()

for (i in 1:ncol(mdf)){
  kolonne <- mdf[,i] # For hver variabel
  if (any(is.na(kolonne))) { # Hvis variabel inneholder en NA legges kolonnenavnet og antallet NA til i tabellen
    tab <- rbind(tab, c(names(mdf)[i], sum(is.na(kolonne))))
    k <- k+1 # Antall NA-variabler telles opp
  }
}

print(k) # Antall variabler med NA-verdier
print(tab) # Oversikt over variabler med manglende verdier og for antall observasjoner

# Finner antallet observasjoner for hvert treslag etter fjerning av NA-verdier for dtw.
mdf_na_sum <- mdf %>% 
  select(trslno, dtw) %>% 
  na.omit() %>% 
  group_by(trslno) %>% 
  summarise(n = n())
nrow(mdf_na_sum)
print(mdf_na_sum)

# Finner antallet observasjoner for hvert treslag etter fjerning av NA-verdier for andre variabler
mdf_na_sum1 <- mdf %>% 
  select(trslno,variabler4) %>% 
  na.omit() %>% 
  group_by(trslno) %>% 
  summarise(n = n())
nrow(mdf_na_sum1)
print(mdf_na_sum1)

# Antall pr??veflater for hvert treslag f??r og etter NA-fjerning for dtw og for andre variabler
mdf_sum <- mdf %>%
  select(variabler) %>% 
  group_by(trslno) %>% 
  summarise(n = n()) %>% 
  mutate(na_laser = mdf_na_sum1$n) %>% 
  mutate(na_dtw = mdf_na_sum$n)

print(mdf_sum)


##### Plukker ut outliere #####

# Terskelverdi for mistenkt skydekke = 2000

# Lager data frame der hver uke for hvert b??nd innad i hver pr??veflate har en rad hver
s2w_uker <- mdf %>% 
  group_by(trslno) %>%
  pivot_longer(cols = c(B02_1:B12_22)) %>% 
  separate(col=name, into=c("bands","week_id")) %>% 
  mutate(week=as.numeric(week_id)+17) %>% 
  select(c(FLATEID, bands, week_id, week, value))
s2w_uker1 <- s2w_uker

# verdier i b??nd 2,3 eller 4 byttes ut med NA for alle b??nd den uka for pr??veflata
s2w_na_long <- s2w_uker1 %>% 
  group_by(FLATEID, week, bands) %>% 
  mutate(value = ifelse(bands %in% c("B02", "B03", "B04") & any(value > 2000), 
                        NA, value)) %>% 
  ungroup()

# Gj??r om til samme format som tidligere med en rad per flateID
s2w_na <- s2w_na_long %>% 
  group_by(FLATEID) %>% 
  select(-week) %>% 
  pivot_wider(names_from = c(bands, week_id), values_from = value) %>% 
  select(-trslno)

#Lager en data frame for bare uker og b??nd per pr??veflate
s2w_na1 <- s2w_na %>% 
  select(-FLATEID)


##### Tilh??rende dataframe for ndvi #####

# Lager navn for ndvi-matrise
ndvi_na1 <- data.frame(matrix(nrow = nrow(s2w_na1), ncol = 22))
colnames(ndvi_na1) <- paste0("ndvi_", 18:39)

#For hver av de 22 ukene (18-39)

for (i in 1:22){ #Hver uke ligger etter hverandre fra 18 til 39 for hvert b??nd.
  # variabler nr. 69 er b??nd B8A_1 og variable nr. 47 er B04_1.
  ndvi <- (s2w_na1[,i+67]-s2w_na1[,i+45])/(s2w_na1[,i+67]+s2w_na1[,i+45]) 
  ndvi_na1[,i] <- ndvi
}

# Legger til flateidene til hver av ukene med ndvier.
ndvi_na <- ndvi_na1 %>% 
  mutate(FLATEID = s2w_na$FLATEID)



##### Interpolerer for NA-verdier #####

# Line??r interpolasjon for manglende verdier
s2w_int_long <- s2w_uker1 %>% 
  group_by(FLATEID, week, bands) %>% 
  mutate(value = ifelse(bands %in% c("B02", "B03", "B04") & any(value > 2000), 
                        NA, value)) %>% 
  ungroup() %>% 
  group_by(FLATEID, bands) %>% 
  mutate(value = na.approx(value, na.rm = FALSE)) %>% 
  mutate(value = na.locf(value, fromLast = TRUE, na.rm = FALSE)) %>% 
  mutate(value = na.locf(value, fromLast = FALSE, na.rm = FALSE)) %>% 
  mutate(value = approx(week, value, xout = week)$y) %>% 
  ungroup()


##### Lager tilh??rende dataframe for ndvi #####

# Gj??r om til samme format som s2-tidsserien.
s2w_int <- s2w_int_long %>% 
  group_by(FLATEID) %>% 
  select(-week) %>% 
  pivot_wider(names_from = c(bands, week_id), values_from = value) %>% 
  select(-trslno)

#Lager en data frame for bare uker og b??nd per pr??veflate
s2w_int1 <- s2w_int %>% 
  select(-FLATEID)

# Lager navn for ndvi-matrise
ndvi_int1 <- data.frame(matrix(nrow = nrow(s2w_int1), ncol = 22))
colnames(ndvi_int1) <- paste0("ndvi_", 18:39)

#For hver av de 22 ukene (18-39)

for (i in 1:22){ #Hver uke ligger etter hverandre fra 18 til 39 for hvert b??nd.
  # variabler nr. 69 er b??nd B8A_1 og variable nr. 47 er B04_1.
  ndvi <- (s2w_int1[,i+67]-s2w_int1[,i+45])/(s2w_int1[,i+67]+s2w_int1[,i+45]) 
  ndvi_int1[,i] <- ndvi
}

# Legger til flateidene til hver av ukene med ndvier.
ndvi_int <- ndvi_int1 %>% 
  mutate(FLATEID = s2w_int$FLATEID)


##### Ny mdf uten outliere med na-verdier #####

# Hoved data frame med manglende verdier for outliere
mdf_na <- 
  spec.fla %>% 
  inner_join(fla, by=c("FLATEID")) %>% 
  filter(n.parts==1) %>% 
  inner_join(s2w_na, by=c("FLATEID")) %>%
  inner_join(ndvi_na, by=c("FLATEID")) %>%
  inner_join(fla.si) %>% 
  droplevels() %>%
  mutate(beskrivelse=as.factor(beskrivelse)) %>% 
  select(variabler)
nrow(mdf_na)


##### Ny mdf med interpolerte verdier over outliere #####

# Hoved data frame med interpolerte verdier for outliere
mdf_int <- 
  spec.fla %>% 
  inner_join(fla, by=c("FLATEID")) %>% 
  filter(n.parts==1) %>% 
  inner_join(s2w_int, by=c("FLATEID")) %>%
  inner_join(ndvi_int, by=c("FLATEID")) %>%
  inner_join(fla.si) %>% 
  droplevels() %>%
  mutate(beskrivelse=as.factor(beskrivelse)) %>% 
  select(variabler)
nrow(mdf_int) # n = 7977





##### Reduserer antall obs. for furu, gran og dunbj??rk - RF for alle treslag #####

# Plukker ut 50 tilfeldige dunbj??rkobservasjoner
mdf_bjork <- mdf_int %>%
  filter(trslno == "Dunbj\u00F8rk") %>%
  sample_n(size = 50, replace = FALSE)

# Plukker ut 50 tilfeldige furuobservasjoner
mdf_furu <- mdf_int %>%
  filter(trslno == "Furu") %>%
  sample_n(size = 50, replace = FALSE)

# Plukker ut 50 tilfeldige granobservasjoner
mdf_gran <- mdf_int %>%
  filter(trslno == "Gran") %>%
  sample_n(size = 50, replace = FALSE)


# Utvalgte observasjoner lagt til i et redusert datasett
mdf_int_red <- mdf_int %>% 
  filter(!trslno %in% c("Dunbj\u00F8rk","Furu","Gran")) %>% # Fjerne alle observasjoner for furu, gran og dunbj??rk
  rbind(mdf_bjork) %>% # Henter inn redusert antall
  rbind(mdf_furu) %>%
  rbind(mdf_gran) %>% 
  select(TRESLAG, trslno, variabler4) %>% # Dette endres for hver RF-klassifisering
  mutate(trslno=as.factor(trslno)) %>% 
  na.omit()
nrow(mdf_int_red)


# RF-klassifisering for alle treslag med modell I) med 1000 beslutningstr??r
rf_mdf_red1 <- randomForest(x=mdf_int_red %>% dplyr::select(variabler1),
                            y = as.factor(mdf_int_red$TRESLAG), ntree = 1000)

# RF-klassifisering for modell II)
rf_mdf_red2 <- randomForest(x=mdf_int_red %>% dplyr::select(variabler2),
                            y = as.factor(mdf_int_red$TRESLAG), ntree = 1000)

# RF-klassifisering for modell III)
rf_mdf_red3 <- randomForest(x=mdf_int_red %>% dplyr::select(variabler3),
                            y = as.factor(mdf_int_red$TRESLAG), ntree = 1000)

# RF-klassifiserin for modell IV)
rf_mdf_red4 <- randomForest(x=mdf_int_red %>% dplyr::select(variabler4),
                            y = as.factor(mdf_int_red$TRESLAG), ntree = 1000)

# Forvirringsmatrisa for alle treslag med modell IV)
r4 <- rf_mdf_red4$confusion
r4

# Henter ut UA for alle treslag for de fire modellene
rf_mdf_red <- data.frame(RF1 = c(rf_mdf_red1$confusion[,26]),RF2 = c(rf_mdf_red2$confusion[,26]),RF3 = c(rf_mdf_red3$confusion[,26]),
                         RF4 = c(rf_mdf_red4$confusion[,26]))
rf_mdf_red



##### RF for hver av de to sammensetningene av lauvtr??r #####

# Lager en data frame med l??vtreslag og fjerner NA-verdier
mdf_int_lauv <- mdf_int_red %>% 
  filter(TRESLAG>29) %>% 
  select(TRESLAG, trslno, variabler4) %>% 
  na.omit()
nrow(mdf_int_lauv)

# Alle l??vtreslag
x <- c(unique(mdf_int_lauv$TRESLAG))

# Alle kombinasjoner av to l??vtreslag
kombinasjoner <- data.frame(expand.grid(x,x))

# Lager tomme data frames for
kvalitet_int_lauv1 <- data.frame()
kvalitet_int_lauv2 <- data.frame()
kvalitet_int_lauv3 <- data.frame()
kvalitet_int_lauv4 <- data.frame()

# Gj??r en RF-klassifisering av alle kombinasjoner av to l??vtreslag
for (i in 1:nrow(kombinasjoner)){
  kombo <- c(kombinasjoner[i,1],kombinasjoner[i,2]) # Alle kombinasjoner
  subset_data <- mdf_int_lauv %>% # Lager et datasett for hver iterasjon med alle observasjoner av de to treslagene i komboen
    filter(TRESLAG == kombo[1] | TRESLAG == kombo[2]) %>% 
    mutate(treslag = as.factor(TRESLAG))
  print(kombo)
  
  # For hvert treslag i kombinasjonen gj??res en klassifisering for alle modellene hvis det er to forskjellige treslag i komboen
  for (k in 1:length(kombo)){
    komb <- kombo[k]
    
    if (length(unique(kombo))<2){next}else{
      
      rf_kombo1 <- randomForest(x=subset_data %>% dplyr::select(variabler1),
                                y = as.factor(subset_data$treslag))
      rf_kombo2 <- randomForest(x=subset_data %>% dplyr::select(variabler2),
                                y = as.factor(subset_data$treslag))
      rf_kombo3 <- randomForest(x=subset_data %>% dplyr::select(variabler3),
                                y = as.factor(subset_data$treslag))
      rf_kombo4 <- randomForest(x=subset_data %>% dplyr::select(variabler4),
                                y = as.factor(subset_data$treslag))
    }
    
  }
  # Hvis det er to forskjellig treslag i komboen legges en rad til med RF-klassifiseringens to treslag og deres feilklassifiseringsandel
  # For alle 4 modeller
  if (length(unique(kombo))<2){next}else{
    kvalitet_int_lauv1 <- rbind(kvalitet_int_lauv1, data.frame(i = i, Treslag1 = rf_kombo1$classes[1],
                                                               Treslag2 = rf_kombo1$classes[2],
                                                               Feil1_1 = rf_kombo1$confusion[1,3],
                                                               Feil2_1 = rf_kombo1$confusion[2,3]))
    kvalitet_int_lauv2 <- rbind(kvalitet_int_lauv2, i=i, data.frame(i = i, Treslag1 = rf_kombo2$classes[1],
                                                                    Treslag2 = rf_kombo2$classes[2],
                                                                    Feil1_2 = rf_kombo2$confusion[1,3],
                                                                    Feil2_2 = rf_kombo2$confusion[2,3]))
    kvalitet_int_lauv3 <- rbind(kvalitet_int_lauv3, data.frame(i = i, Treslag1 = rf_kombo3$classes[1],
                                                               Treslag2 = rf_kombo3$classes[2],
                                                               Feil1_3 = rf_kombo3$confusion[1,3],
                                                               Feil2_3 = rf_kombo3$confusion[2,3]))
    kvalitet_int_lauv4 <- rbind(kvalitet_int_lauv4, data.frame(i = i, Treslag1 = rf_kombo4$classes[1],
                                                               Treslag2 = rf_kombo4$classes[2],
                                                               Feil1_4 = rf_kombo4$confusion[1,3],
                                                               Feil2_4 = rf_kombo4$confusion[2,3]))
  }
}

# Alle modellene legges til i samme data frame
kvalitet_int_lauv <- kvalitet_int_lauv1 %>% 
  left_join(kvalitet_int_lauv2) %>% 
  left_join(kvalitet_int_lauv3) %>% 
  left_join(kvalitet_int_lauv4)

# Filtrerer vekk der et treslag er klassifisert mot seg selv
kvalitet_int_lauv_red <- kvalitet_int_lauv %>% 
  filter(!is.na(Feil1_1)) %>% 
  filter(!is.na(Feil2_1)) %>% 
  filter(Treslag1 != Treslag2)

kvalitet_int_lauv
kvalitet_int_lauv_red


##### Gran avgrenset omr??de og klassifisering med redusert antall granobs. #####

# Henter ut introduserte granarter og avstanden til kyst for hver av pr??veflatene
avstkyst_introgran <- mdf_int %>% 
  select(FLATEID, trslno, avstkyst) %>% 
  filter(trslno %in% c('Introdusert gran (Picea spp.)', 'Edelgranarter (Abies spp.)'))
# 3 outliere over 10500
avstkyst_introgran

# Filtrerer vekk granobservasjoner lengre enn 50 km unna kysten, og ??st for 8 grader ??st og s??r for 61 grader nord
mdf_redgran_avstkyst <- mdf_int %>%
  filter(avstkyst < 50000) %>% 
  filter(approx_longitude < 20 & !
           (approx_latitude < 61 & approx_longitude > 8)) %>% 
  filter(trslno == "Gran") %>%
  sample_n(size = 50, replace = FALSE) # 50 tilfeldig granobservasjoner plukkes ut fra inni det avgrensede omr??det
nrow(mdf_redgran_avstkyst)

# Henter introduserte granobservasjoner, edelgranarter og lerk
mdf_red_g_avstkyst <- mdf_int %>% 
  filter(TRESLAG %in% c(2,3,21))

# Legger de utplukkede granobservasjonene til resten av granartene som hentet ut.
mdf_redg_avstkyst <- mdf_red_g_avstkyst %>% 
  rbind(mdf_redgran_avstkyst) %>% 
  select(FLATEID, TRESLAG, trslno, variabler4) %>%
  mutate(treslag = ifelse(TRESLAG %in% c(2,3,21),2,1)) %>% # Samler introdusert gran, edelgranarter og lerk i ??n klasse (2)
  na.omit()
nrow(mdf_redg_avstkyst)

## Plotter kart over hvor pr??veflatene for gran i RF hentes fra
# Henter inn lengde- og breddegrader for grantypene
mdf.prop <- 
  spec.prop %>% 
  inner_join(fla %>% select(FLATEID, approx_latitude, approx_longitude, dted10_hoh), 
             by=c("FLATEID")) %>% 
  inner_join(mdf_redg_avstkyst)
nrow(mdf.prop)

# Plasserer pr??veflatekoordinatene i Norgepolygonet
fla.geo <-
  mdf.prop %>%
  filter(treslag == ifelse(TRESLAG %in% c(2,3),2,1))%>%
  st_as_sf(coords=c("approx_longitude","approx_latitude"),crs=4326)%>%
  cbind(.,st_coordinates(.))
nrow(fla.geo)

# Plotter pr??veflatene i RF-klassifiseringa for hver av klassene
ggplot(fla.geo)+
  geom_sf(data=Norway, fill=NA) +
  geom_sf(aes(fill=trslno, col=trslno, alpha=prop, size = 1)) + 
  facet_wrap(~treslag) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_sf(xlim = c(3, 33), ylim = c(57.5, 72), expand = FALSE)

# RF-klassifisering for grantyper for modell I) med 1000 beslutningstr??r
rf_gran_red_avstkyst1 <- randomForest(x=mdf_redg_avstkyst %>% dplyr::select(variabler1),
                                      y = as.factor(mdf_redg_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for grantyper for modell II)
rf_gran_red_avstkyst2 <- randomForest(x=mdf_redg_avstkyst %>% dplyr::select(variabler2),
                                      y = as.factor(mdf_redg_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for grantyper for modell III)
rf_gran_red_avstkyst3 <- randomForest(x=mdf_redg_avstkyst %>% dplyr::select(variabler3),
                                      y = as.factor(mdf_redg_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for grantyper for modell IV)
rf_gran_red_avstkyst4 <- randomForest(x=mdf_redg_avstkyst %>% dplyr::select(variabler4),
                                      y = as.factor(mdf_redg_avstkyst$treslag), ntree = 1000)


rf_gran_red_avstkyst1
rf_gran_red_avstkyst2
rf_gran_red_avstkyst3
rf_gran_red_avstkyst4


# Set up 2x2 plot layout
par(mfrow = c(1,2))

# Plotter to og to variabelviktighetsplot for granmodellene
varImpPlot(rf_gran_red_avstkyst1)
varImpPlot(rf_gran_red_avstkyst2)

par(mfrow = c(1,2))
varImpPlot(rf_gran_red_avstkyst3)
varImpPlot(rf_gran_red_avstkyst4)




##### Kryssvalidering for alle granobservasjoner - RF for alle granutvalgene #####

# Lager en data frame for grantyper med avgrensninger
mdf_granobs <- mdf_int %>% 
  filter(TRESLAG %in% c(1,2,3,21)) %>%
  filter(avstkyst<50000) %>%
  filter(approx_longitude < 20 & !
           (approx_latitude < 61 & approx_longitude > 8)) %>% 
  mutate(treslag = ifelse(TRESLAG == 1, 1, 2)) %>% 
  mutate(cv_gran=ifelse(TRESLAG == 1, sample.int(10, n(), replace=T), 0)) %>% # Deler granobservasjoner inn i 10 grupper
  select(treslag, variabler, cv_gran)

# Lager tomme data frames og liste for modellene
rf_gcv <- data.frame()
rf_granliste <- list()

rf_gcv_best <- data.frame()
gran_UAplot <- data.frame()

for (i in 1:10){
  # Nytt datasett for hver iterasjon med granobservasjonene i hver gruppe
  mdf_grancv <- mdf_granobs %>% 
    filter(cv_gran %in% c(i, 0)) %>% 
    select(treslag, variabler4, cv_gran) %>% 
    na.omit()
  
  set.seed(123)
  
  # RF-klassifisering med modell IV)
  rf_grancv <- randomForest(x=mdf_grancv %>% dplyr::select(variabler4),
                            y = as.factor(mdf_grancv$treslag), ntree = 1000)
  
  # Legger til modellen i en liste
  rf_granliste[[i]] <- rf_grancv
  
  # N??yaktigheter for modellen (UA, PA og OA)
  rf_gcv <- rbind(rf_gcv, data.frame(tr1_1 = rf_grancv$confusion[1,1],
                                     tr1_2 = rf_grancv$confusion[1,2],
                                     tr2_1 = rf_grancv$confusion[2,1],
                                     tr2_2 = rf_grancv$confusion[2,2],
                                     UA1 = rf_grancv$confusion[1,1]/(sum(rf_grancv$confusion[1,])),
                                     UA2 = rf_grancv$confusion[2,2]/(sum(rf_grancv$confusion[2,])),
                                     PA1 = rf_grancv$confusion[1,1]/(sum(rf_grancv$confusion[,1])),
                                     PA2 = rf_grancv$confusion[2,2]/(sum(rf_grancv$confusion[,2])),
                                     OA = (rf_grancv$confusion[1,1]+rf_grancv$confusion[2,2])/sum(rf_grancv$confusion[,])))

  # Modellen testes p?? granobservasjonene utenom de brukt for ?? lage modellen
  # Lager nytt datasett med resten av granobservasjonene
  mdf_grani <- mdf_granobs %>% 
    filter(!cv_gran %in% c(0,i)) %>% 
    mutate(treslag = as.factor(treslag)) %>% 
    select(treslag, variabler4) %>% 
    na.omit()
  
  # Predikerer granobservasjonene med modellen i iterasjonen
  pred <- predict(rf_grancv, mdf_grani)
  mdf_grani$pred <- pred
  
  # Lager forvirringsmatrise for grantestdataene
  cfm_best <- table(mdf_grani$treslag, mdf_grani$pred)
  print(cfm_best)
  
  # Regner ut UA for forvirringsmatrisa til testdataene
  rf_gcv_best <- rbind(rf_gcv_best, data.frame(gran = cfm_best[1,1],
                                               introdusert = cfm_best[1,2],
                                               UA = (cfm_best[1,1]/sum(cfm_best[,]))))
  
  # Lagrer UA fra OOB og utelatte data i en data frame
  gran_UAplot <- rbind(data.frame(UA_OOB = rf_gcv$UA1,
                                  UA_utelatt = rf_gcv_best$UA))
  
  
}

# Ser etter beste iterasjon blant modellene:
rf_gcv

# Printer beste iterasjon
print(rf_granliste[[10]]$confusion) # Endres ut ifra beste iterasjon

# Gjennomsnittlige n??yaktigheter samlet for alle iterasjoner
resultater <- data.frame(
  UA1 = sum(rf_gcv$UA1)/nrow(rf_gcv),
  UA2 = sum(rf_gcv$UA2)/nrow(rf_gcv),
  PA1 = sum(rf_gcv$PA1)/nrow(rf_gcv),
  PA2 = sum(rf_gcv$PA2)/nrow(rf_gcv),
  OA = sum(rf_gcv$OA)/nrow(rf_gcv),
  sd_UA1 = sd(rf_gcv$UA1),
  sd_UA2 = sd(rf_gcv$UA2),
  sd_PA1 = sd(rf_gcv$PA1),
  sd_PA2 = sd(rf_gcv$PA2),
  sd_OA = sd(rf_gcv$OA))
resultater

rf_gcv_best

# Plotter OOB-UA for gran mot UA for usette granobservasjoner
ggplot(gran_UAplot) +
  aes(x = UA_OOB, y = UA_utelatt) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  labs(x = "OOB", y = "Utelatt") +
  theme_minimal() +
  xlim(0.7, 1) +
  ylim(0.7, 1)


##### Eik avgrenset omr??de og klassifisering med redusert antall dunbj??rkobs. #####

# Henter ut eikeobservasjoner og tilh??rende avstander fra kystlinja
avstkyst_eik <- mdf_int %>% 
  filter(trslno == 'Eik') %>% 
  select(FLATEID, avstkyst)
# 3 outliere over 13000
avstkyst_eik

# Lager data frame med 50 utplukkede dunbj??rkobservasjoner fra det avgrensede omr??det
mdf_redbjork_avstkyst <- mdf_int %>%
  filter(avstkyst < 32000) %>% 
  filter(approx_latitude < 60) %>% 
  filter(trslno == "Dunbj\u00F8rk") %>% 
  sample_n(size = 50, replace = FALSE)
nrow(mdf_redbjork_avstkyst)

# Henter ut l??vtr??r utenom dunbj??rk
mdf_red_e_avstkyst <- mdf_int %>% 
  filter(TRESLAG > 30)

# Legger til utplukkede dunbj??rkobservasjoner til resten av l??vtreobservasjonene
mdf_rede_avstkyst <- mdf_red_e_avstkyst %>% 
  rbind(mdf_redbjork_avstkyst) %>% 
  filter(avstkyst < 32000) %>% # Avgrenser omr??de for data frame til max 32 km unna kysten
  filter(approx_latitude < 60) %>% # Avgrenser til s??r for 60 grader nord
  select(FLATEID, TRESLAG, trslno, variabler4) %>%
  mutate(treslag = ifelse(TRESLAG == 40,40,30)) %>% # Annet l??v samles i ??n klasse (30)
  na.omit()
nrow(mdf_rede_avstkyst)

## Plotter kart over hvor pr??veflatene i RF hentes fra
# Henter inn lengde- og breddegrader for l??vtr??r
mdf.prop <- 
  spec.prop %>% #filter(prop>0) %>% #not needed - not a factor?
  inner_join(fla %>% select(FLATEID, approx_latitude, approx_longitude, dted10_hoh), 
             by=c("FLATEID")) %>% 
  inner_join(mdf_rede_avstkyst)
nrow(mdf.prop)

# Plasserer pr??veflatekoordinatene i Norgepolygonet
fla.geo <-
  mdf.prop %>%
  filter(treslag == ifelse(TRESLAG == 40,40,30))%>%
  st_as_sf(coords=c("approx_longitude","approx_latitude"),crs=4326)%>%
  cbind(.,st_coordinates(.))
nrow(fla.geo)

# Plotter pr??veflatene i RF-klassifiseringa for hver av klassene
ggplot(fla.geo)+
  geom_sf(data=Norway, fill=NA) +
  geom_sf(aes(fill=prop, col=treslag, alpha=prop, size = 1)) + 
  facet_wrap(~treslag) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_sf(xlim = c(3, 15), ylim = c(57.5, 61), expand = FALSE)


# RF-klassifisering for eikedeteksjon med modell I) og 1000 beslutningstr??r
Eik_modell1 <- randomForest(x=mdf_rede_avstkyst %>% dplyr::select(variabler1),
                                     y = as.factor(mdf_rede_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for eikedeteksjon med modell II)
Eik_modell2 <- randomForest(x=mdf_rede_avstkyst %>% dplyr::select(variabler2),
                                     y = as.factor(mdf_rede_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for eikedeteksjon med modell III)
Eik_modell3 <- randomForest(x=mdf_rede_avstkyst %>% dplyr::select(variabler3),
                                     y = as.factor(mdf_rede_avstkyst$treslag), ntree = 1000)

# RF-klassifisering for eikedeteksjon med modell IV)
Eik_modell4 <- randomForest(x=mdf_rede_avstkyst %>% dplyr::select(variabler4),
                                     y = as.factor(mdf_rede_avstkyst$treslag), ntree = 1000)

rf_eik_red_avstkyst1
rf_eik_red_avstkyst2
rf_eik_red_avstkyst3
rf_eik_red_avstkyst4


# Plotter to og to variabelviktighetsplot for eikemodellene
par(mfrow = c(1,2))
varImpPlot(Eik_modell1)
varImpPlot(Eik_modell2)

par(mfrow = c(1,2))
varImpPlot(Eik_modell3)
varImpPlot(Eik_modell4)



##### Kryssvalidering eik #####

# Lager en data frame for l??vtr??r med eikeavgrensninger
mdf_eikobs <- mdf_int %>% 
  filter(TRESLAG > 29) %>%
  filter(avstkyst < 32000) %>%
  filter(approx_latitude < 61) %>% 
  mutate(cv_eik=ifelse(TRESLAG == 30, sample.int(3, n(), replace=T), 0)) %>%
  mutate(treslag = as.factor(ifelse(TRESLAG == 40, 40, 30))) %>% 
  select(treslag, variabler, cv_eik)

# Lager en tom data frame
rf_ecv <- data.frame()

for (i in 1:3){
  # Nytt datasett for hver iterasjon med l??vtreobservasjonene i hver gruppe
  mdf_eikcv <- mdf_eikobs %>% 
    filter(cv_eik %in% c(i, 0)) %>% 
    select(treslag, variabler4) %>% 
    na.omit()
  
  set.seed(42)
  
  # RF-klassifisering for modell IV) med 1000 beslutningstr??r for hver iterasjon
  rfm_e <- randomForest(treslag ~. , data = mdf_eikcv, ntree = 1000)
  
  # Forvirringsmatrise og utregninger av UA, PA og OA
  rf_ecv <- rbind(rf_ecv, data.frame(tr1_1 = rfm_e$confusion[1,1],
                                     tr1_2 = rfm_e$confusion[1,2],
                                     tr2_1 = rfm_e$confusion[2,1],
                                     tr2_2 = rfm_e$confusion[2,2],
                                     UA1 = rfm_e$confusion[1,1]/(sum(rfm_e$confusion[1,])),
                                     UA2 = rfm_e$confusion[2,2]/(sum(rfm_e$confusion[2,])),
                                     PA1 = rfm_e$confusion[1,1]/(sum(rfm_e$confusion[,1])),
                                     PA2 = rfm_e$confusion[2,2]/(sum(rfm_e$confusion[,2])),
                                     OA = (rfm_e$confusion[1,1]+rfm_e$confusion[2,2])/sum(rfm_e$confusion[,])))
  
}

# For hver iterasjon
rf_ecv

# Gjennomsnitt av UA, PA og OA for alle iterasjoner
resultater_e <- data.frame(
  UA1 = sum(rf_ecv$UA1)/nrow(rf_ecv),
  UA2 = sum(rf_ecv$UA2)/nrow(rf_ecv),
  PA1 = sum(rf_ecv$PA1)/nrow(rf_ecv),
  PA2 = sum(rf_ecv$PA2)/nrow(rf_ecv),
  OA = sum(rf_ecv$OA)/nrow(rf_ecv),
  # Standardavvik
  sd_UA1 = sd(rf_ecv$UA1),
  sd_UA2 = sd(rf_ecv$UA2),
  sd_PA1 = sd(rf_ecv$PA1),
  sd_PA2 = sd(rf_ecv$PA2),
  sd_OA = sd(rf_ecv$OA))
resultater_e



##### Gr??or avgrenset omr??de og klassifisering med redusert antall gr??orobs. #####

# Henter ut 50 tilfeldige dunbj??rkobservasjoner
mdf_redgraor <- mdf_int %>%
  filter(trslno == "Dunbj\u00F8rk") %>%
  sample_n(size = 50, replace = FALSE) %>% 
  mutate(treslag = 30) %>% 
  select(treslag, variabler4)


# Henter ut l??vtr??r og samler l??vtr??r utenom gr??or i ??n klasse
mdf_red_graor <- mdf_int %>%
  filter(TRESLAG > 30) %>% 
  mutate(treslag = ifelse(TRESLAG == 50 , 50, 30)) %>% 
  select(treslag, variabler4) %>%
  rbind(mdf_redgraor) %>% 
  na.omit()
nrow(mdf_red_graor)



## Plotter kart over hvor pr??veflatene i RF hentes fra

# Henter inn alle plasseringene for gr??ordatasettet
mdf.prop <- 
  spec.prop %>% 
  inner_join(fla %>% select(FLATEID, approx_latitude, approx_longitude, dted10_hoh), 
             by=c("FLATEID")) %>% 
  inner_join(mdf_red_graor)
nrow(mdf.prop)

# Plasserer pr??veflatekoordinatene i Norgepolygonet
fla.geo <-
  mdf.prop %>%
  filter(treslag == ifelse(TRESLAG == 50, 50, 30))%>%
  st_as_sf(coords=c("approx_longitude","approx_latitude"),crs=4326)%>%
  cbind(.,st_coordinates(.))
nrow(fla.geo)

# Plotter pr??veflatene i RF-klassifiseringa for hver av klassene
ggplot(fla.geo)+
  geom_sf(data=Norway, fill=NA) +
  geom_sf(aes(fill=trslno, col=treslag, alpha=prop, size = 1)) + 
  facet_wrap(~treslag) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_sf(xlim = c(3, 33), ylim = c(57.5, 72), expand = FALSE)

###


# RF-klassifisering for gr??or med modell I)
Graor_modell1 <- randomForest(x=mdf_red_graor %>% dplyr::select(variabler1),
                                       y = as.factor(mdf_red_graor$treslag), ntree = 1000)

# RF-klassifisering for gr??or med modell II)
Graor_modell2 <- randomForest(x=mdf_red_graor %>% dplyr::select(variabler2),
                                       y = as.factor(mdf_red_graor$treslag), ntree = 1000)

# RF-klassifisering for gr??or med modell III)
Graor_modell3 <- randomForest(x=mdf_red_graor %>% dplyr::select(variabler3),
                                       y = as.factor(mdf_red_graor$treslag), ntree = 1000)

# RF-klassifisering for gr??or med modell IV)
Graor_modell4 <- randomForest(x=mdf_red_graor %>% dplyr::select(variabler4),
                                       y = as.factor(mdf_red_graor$treslag), ntree = 1000)


Graor_modell1
Graor_modell2
Graor_modell3
Graor_modell4


# Plotter to og to variabelviktighetsplott for gr??ormodellene
par(mfrow = c(1,2))
varImpPlot(Graor_modell1)
varImpPlot(Graor_modell2)

par(mfrow = c(1,2))
varImpPlot(Graor_modell3)
varImpPlot(Graor_modell4)


##### Kryssvalidering gr??or #####

mdf_graorcv <- mdf_int %>%
  filter(TRESLAG > 29) %>% 
  select(FLATEID, TRESLAG, trslno, variabler) %>%
  mutate(cv_graor = ifelse(TRESLAG == 30, sample.int(60, n(), replace=T), 0)) %>% # 60 deler for dunbj??rkobservasjoner tilfeldig fordelt
  mutate(treslag = as.factor(ifelse(TRESLAG == 50, 50, 30))) %>% 
  select(treslag, variabler4, cv_graor)

rf_gocv <- data.frame()


for (i in 1:60){
  # Lager data frame for hver iterasjon med innhentet ??n av dunbj??rkgruppe
  mdf_graorcv <- mdf_graorobs %>% 
    filter(cv_graor %in% c(i, 0)) %>% # Resten av l??vtrelagene er med i alle iterasjoner
    select(treslag, variabler4) %>% 
    na.omit()
  
  set.seed(42)
  
  # RF-klassifisering for hver iterasjon
  rfm_go <- randomForest(treslag ~. , data = mdf_graorcv, ntree = 1000)
  
  # Forvirringsmatriser og UA, PA og OA legges til for hver iterasjon
  rf_gocv <- rbind(rf_gocv, data.frame(tr1_1 = rfm_go$confusion[1,1],
                                       tr1_2 = rfm_go$confusion[1,2],
                                       tr2_1 = rfm_go$confusion[2,1],
                                       tr2_2 = rfm_go$confusion[2,2],
                                       UA1 = rfm_go$confusion[1,1]/(sum(rfm_go$confusion[1,])),
                                       UA2 = rfm_go$confusion[2,2]/(sum(rfm_go$confusion[2,])),
                                       PA1 = rfm_go$confusion[1,1]/(sum(rfm_go$confusion[,1])),
                                       PA2 = rfm_go$confusion[2,2]/(sum(rfm_go$confusion[,2])),
                                       OA = (rfm_go$confusion[1,1]+rfm_go$confusion[2,2])/sum(rfm_go$confusion[,])))
  
}

rf_gocv

# Gjennomsnittlige n??yaktigheter og standardavvik for alle modellene
resultater_go <- data.frame(
  UA1 = sum(rf_gocv$UA1)/nrow(rf_gocv),
  UA2 = sum(rf_gocv$UA2)/nrow(rf_gocv),
  PA1 = sum(rf_gocv$PA1)/nrow(rf_gocv),
  PA2 = sum(rf_gocv$PA2)/nrow(rf_gocv),
  OA = sum(rf_gocv$OA)/nrow(rf_gocv),
  sd_UA1 = sd(rf_gocv$UA1),
  sd_UA2 = sd(rf_gocv$UA2),
  sd_PA1 = sd(rf_gocv$PA1),
  sd_PA2 = sd(rf_gocv$PA2),
  sd_OA = sd(rf_gocv$OA))
resultater_go



##### Helt ny inndeling basert p?? forvirringsmatrise for alle treslag #####

# For reduserte antall observasjoner for klasse 1
mdfl <- mdf_int_red %>% 
  filter(TRESLAG>29) %>% 
  mutate(treslag = ifelse(TRESLAG %in% c(50, 43, 55, 54, 31, 32, 49, 52, 30, 53, 59), 1, 2)) %>% # Ny klasseinndeling
  filter(treslag == 1) 
#  sample_n(size = 100, replace = FALSE)

# Observasjoner i klasse 2
mdfl1 <- mdf_int_red %>%  
  filter(TRESLAG>29) %>% 
  mutate(treslag = ifelse(TRESLAG %in% c(50, 43, 55, 54, 31, 32, 49, 52, 30, 53, 59), 1, 2)) %>% 
  filter(treslag == 2)

# Legger sammen klasse 1 og 2
mdfl2 <- mdfl1 %>% 
  rbind(mdfl)


# RF-klassifisering for de to klassene med 1000 beslutningstr??r
rfm_to <- randomForest(x=mdfl2 %>% dplyr::select(variabler4),
                       y = as.factor(mdfl2$treslag), ntree = 1000)
rfm_to

par(mfrow = c(1,1))
varImpPlot(rfm_to)


# Deler inn dunbj??rkobservasjoner i 60 grupper
mdfl_cv <- mdf_int %>%
  filter(TRESLAG > 29) %>% 
  select(FLATEID, TRESLAG, trslno, variabler) %>%
  mutate(cv_to=ifelse(TRESLAG == 30, sample.int(60, n(), replace=T), 0)) %>%
  select(TRESLAG, variabler4, cv_to)


rf_tocv <- data.frame()

for (i in 1:60){
  # Lager data frame for hver iterasjon
  mdfl_cv1 <- mdfl_cv %>% 
    filter(cv_to %in% c(i, 0)) %>% 
    mutate(treslag = as.factor(ifelse(TRESLAG %in% c(50, 43, 55, 54, 31, 32, 49, 52, 30, 53, 59), 1, 2))) %>% 
    select(treslag, variabler4) %>% 
    na.omit()
  
  set.seed(42)
  
  # RF-klassifisering for de to klassene med 1000 beslutningstr??r
  rfm_to <- randomForest(treslag ~. , data = mdfl_cv1, ntree = 1000)
  
  # Forvirringsmatrise og n??yaktigheter regnet ut for hver iterasjon
  rf_tocv <- rbind(rf_tocv, data.frame(tr1_1 = rfm_to$confusion[1,1],
                                       tr1_2 = rfm_to$confusion[1,2],
                                       tr2_1 = rfm_to$confusion[2,1],
                                       tr2_2 = rfm_to$confusion[2,2],
                                       UA1 = rfm_to$confusion[1,1]/(sum(rfm_to$confusion[1,])),
                                       UA2 = rfm_to$confusion[2,2]/(sum(rfm_to$confusion[2,])),
                                       PA1 = rfm_to$confusion[1,1]/(sum(rfm_to$confusion[,1])),
                                       PA2 = rfm_to$confusion[2,2]/(sum(rfm_to$confusion[,2])),
                                       OA = (rfm_to$confusion[1,1]+rfm_to$confusion[2,2])/sum(rfm_to$confusion[,])))
  
}

# Forvirringsmatrise og n??yaktigheter for alle iterasjoner
rf_tocv

# Gjennomsnittlige n??yaktigheter og standardavvik for to-klassemodellen
resultater_to <- data.frame(
  UA1 = sum(rf_tocv$UA1)/nrow(rf_tocv),
  UA2 = sum(rf_tocv$UA2)/nrow(rf_tocv),
  PA1 = sum(rf_tocv$PA1)/nrow(rf_tocv),
  PA2 = sum(rf_tocv$PA2)/nrow(rf_tocv),
  OA = sum(rf_tocv$OA)/nrow(rf_tocv),
  sd_UA1 = sd(rf_tocv$UA1),
  sd_UA2 = sd(rf_tocv$UA2),
  sd_PA1 = sd(rf_tocv$PA1),
  sd_PA2 = sd(rf_tocv$PA2),
  sd_OA = sd(rf_tocv$OA))
resultater_to



##### Dominansgrad for hvert treslag #####

# Treslag og antall for tre grader av dominans
dominans <- mdf_int %>% 
  select(trslno, prop) %>% # Velger treslag og volumandel for hver pr??veflate
  mutate(dekning = ifelse(prop > 0.75 & prop < 0.85, 3,
                          ifelse(prop >= 0.85 & prop < 0.95, 2, 1))) %>% # Lager tre kategorier med grader av dominans
  group_by(dekning, trslno) %>% 
  summarize(n = n()) # Summerer antall flater per treslag i hver av dekningsgradene 1, 2 og 3
dominans  
  
  



##### Referanser i skriptet #####
citation("randomForest")

