library(dplyr)

load("GbyE.RData")

Dataset <- read.csv("Group3.csv")



group3 <- Dataset |> 
                mutate(RecID = Rec_ID,
                       ExpID = Experiment_ID, 
                       EnvID = YrLocID, 
                       HybID = Hyb_ID, 
                       HybType = Entry_Type,
                       TesterID = Tester_ID,
                       MST = MOIST,
                       HybPed = Hyb_Ped) |>
                select(RecID, 
                       ExpID, 
                       LocID, 
                       EnvID,
                       HybPed,
                       HybID,
                       HybType, 
                       TesterID, 
                       YLD,
                       MST,
                       TWT,
                       PLTHT,
                       EARHT)


group3_yld <- group3 |> 
                    select(RecID, ExpID, LocID, EnvID, HybPed, HybID, HybType, TesterID, YLD) |>
                    filter(!is.na(YLD)) 



group3_mst <- group3 |> 
                    select(RecID, ExpID, LocID, EnvID, HybPed, HybID, HybType, TesterID, MST) |>
                    filter(!is.na(MST))

group3_twt <- group3 |> 
                    select(RecID, ExpID, LocID, EnvID, HybPed, HybID, HybType, TesterID, TWT) |>
                    filter(!is.na(TWT))

group3_pltht <- group3 |> 
                    select(RecID, ExpID, LocID, EnvID, HybPed, HybID, HybType, TesterID, PLTHT) |>
                    filter(!is.na(PLTHT))


group3_earht <- group3 |> 
                    select(RecID, ExpID, LocID, EnvID, HybPed, HybID, HybType, TesterID, EARHT) |>
                    filter(!is.na(EARHT))



##################################################################
######################### Final Project ##########################
##################################################################
library(dplyr)


# Read data
g2f_2021 <- read.csv("g2f_2021_phenotypic_clean_data.csv")
g2f_2022 <- read.csv("g2f_2022_phenotypic_clean_data.csv")
g2f_2023 <- read.csv("g2f_2023_phenotypic_clean_data.csv")



# Select locations to work with
g2f_2021_vec <-  g2f_2021 %>% filter(State %in% c("Nebraska", "Minnesota", "Iowa"))
g2f_2022_vec <-  g2f_2022 %>% filter(State %in% c("NE", "MN", "IA"))
g2f_2023_vec <-  g2f_2023 %>% filter(State %in% c("NE", "MN", "IA"))



# Recode State name to short version in 2021 to match other years
g2f_2021_vec$State <- recode(g2f_2021_vec$State,
                             "Nebraska" = "NE",
                             "Minnesota" = "MN",
                             "Iowa" = "IA",
                             .default = "")

# correct errors with city names
g2f_2021_vec$City <- recode(g2f_2021_vec$City, "Noth Platte" = "North Platte", "Lincoln " = "Lincoln")
g2f_2022_vec$City <- recode(g2f_2022_vec$City, "Noth Platte" = "North Platte", "Lincoln " = "Lincoln")
g2f_2023_vec$City <- recode(g2f_2023_vec$City, "Noth Platte" = "North Platte", "Lincoln " = "Lincoln")



# Rename columns
colnames(g2f_2021_vec)[2] <- "FieldLocation"
colnames(g2f_2021_vec)[19] <- "PlotID"
colnames(g2f_2021_vec)[26] <- "AnthesisDays"
colnames(g2f_2021_vec)[27] <- "SilkingDays"
colnames(g2f_2021_vec)[28] <- "PlantHeight"
colnames(g2f_2021_vec)[29] <- "EarHeight"
colnames(g2f_2021_vec)[32] <- "StalkLodging"
colnames(g2f_2021_vec)[33] <- "GrainMoisture"
colnames(g2f_2021_vec)[36] <- "GrainYield"


colnames(g2f_2022)
colnames(g2f_2022_vec)[2] <- "FieldLocation"
colnames(g2f_2022_vec)[18] <- "PlotID"
colnames(g2f_2022_vec)[25] <- "AnthesisDays"
colnames(g2f_2022_vec)[26] <- "SilkingDays"
colnames(g2f_2022_vec)[27] <- "PlantHeight"
colnames(g2f_2022_vec)[28] <- "EarHeight"
colnames(g2f_2022_vec)[31] <- "StalkLodging"
colnames(g2f_2022_vec)[32] <- "GrainMoisture"
colnames(g2f_2022_vec)[35] <- "GrainYield"



colnames(g2f_2023)
colnames(g2f_2023_vec)[2] <- "FieldLocation"
colnames(g2f_2023_vec)[18] <- "PlotID"
colnames(g2f_2023_vec)[25] <- "AnthesisDays"
colnames(g2f_2023_vec)[26] <- "SilkingDays"
colnames(g2f_2023_vec)[27] <- "PlantHeight"
colnames(g2f_2023_vec)[28] <- "EarHeight"
colnames(g2f_2023_vec)[31] <- "StalkLodging"
colnames(g2f_2023_vec)[32] <- "GrainMoisture"
colnames(g2f_2023_vec)[35] <- "GrainYield"



# Normalize datatypes
g2f_2021_vec$Plot <- as.integer(g2f_2021_vec$Plot)
g2f_2022_vec$Plot <- as.integer(g2f_2022_vec$Plot)
g2f_2023_vec$Plot <- as.integer(g2f_2023_vec$Plot)

g2f_2021_vec$PlotID <- as.character(g2f_2021_vec$PlotID)
g2f_2022_vec$PlotID <- as.character(g2f_2022_vec$PlotID)
g2f_2023_vec$PlotID <- as.character(g2f_2023_vec$PlotID)


###################### Yield ######################

# Select relevant columns and filter out missing rows
g2f_yld_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainYield) %>%
                      filter(!is.na(GrainYield) & !is.na(EnvID))



g2f_yld_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainYield) %>%
                      filter(!is.na(GrainYield) & !is.na(EnvID))

                 


g2f_yld_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainYield) %>%
                      filter(!is.na(GrainYield) & !is.na(EnvID))



            
# Combine dataset
g2f_yld_list <- list(g2f_yld_2021_df, g2f_yld_2022_df, g2f_yld_2023_df)
g2f_yld_df <- bind_rows(g2f_yld_list)


#convert to dataframe
g2f_yld_df <- as.data.frame(g2f_yld_df)




###################### Anthesis Days ######################

# Select relevant columns and filter out missing rows
g2f_anth_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             AnthesisDays) %>%
                      filter(!is.na(AnthesisDays) & !is.na(EnvID))



g2f_anth_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             AnthesisDays) %>%
                      filter(!is.na(AnthesisDays) & !is.na(EnvID))

                 


g2f_anth_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             AnthesisDays) %>%
                      filter(!is.na(AnthesisDays) & !is.na(EnvID))



            
# Combine dataset
g2f_anth_list <- list(g2f_anth_2021_df, g2f_anth_2022_df, g2f_anth_2023_df)
g2f_anth_df <- bind_rows(g2f_anth_list)


#convert to dataframe
g2f_anth_df <- as.data.frame(g2f_anth_df)





###################### Silking Days ######################

# Select relevant columns and filter out missing rows
g2f_silk_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             SilkingDays) %>%
                      filter(!is.na(SilkingDays) & !is.na(EnvID))



g2f_silk_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             SilkingDays) %>%
                      filter(!is.na(SilkingDays) & !is.na(EnvID))

                 


g2f_silk_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             SilkingDays) %>%
                      filter(!is.na(SilkingDays) & !is.na(EnvID))



            
# Combine dataset
g2f_silk_list <- list(g2f_silk_2021_df, g2f_silk_2022_df, g2f_silk_2023_df)
g2f_silk_df <- bind_rows(g2f_silk_list)


#convert to dataframe
g2f_silk_df <- as.data.frame(g2f_silk_df)



g2f_2021_df <- g2f_2021_vec %>% 
  mutate(EnvID = paste0(City, Year)) %>%
  select(Year, 
         FieldLocation, 
         State, 
         City,
         EnvID,
         Pedigree,
         Experiment,
         Plot, 
         PlotID,
         Block, 
         Replicate,
         AnthesisDays,
         SilkingDays,
         PlantHeight,
         EarHeight,
         StalkLodging,
         GrainMoisture,
         GrainYield) %>%
  filter(if_all(everything(), ~ !is.na(.)))






g2f_2022_df <- g2f_2022_vec %>% 
                    mutate(EnvID = paste0(City, Year)) %>%
                    select(Year, 
                           FieldLocation, 
                           State, 
                           City,
                           EnvID,
                           Pedigree,
                           Experiment,
                           Plot, 
                           PlotID,
                           Block, 
                           Replicate,
                           AnthesisDays,
                           SilkingDays,
                           PlantHeight,
                           EarHeight,
                           StalkLodging,
                           GrainMoisture,
                           GrainYield) %>%
                        filter(if_all(everything(), ~ !is.na(.)))



g2f_2023_df <- g2f_2023_vec %>% 
                    mutate(EnvID = paste0(City, Year)) %>%
                    select(Year, 
                           FieldLocation, 
                           State, 
                           City,
                           EnvID,
                           Pedigree,
                           Experiment,
                           Plot, 
                           PlotID,
                           Block, 
                           Replicate,
                           AnthesisDays,
                           SilkingDays,
                           PlantHeight,
                           EarHeight,
                           StalkLodging,
                           GrainMoisture,
                           GrainYield) %>%
                      filter(if_all(everything(), ~ !is.na(.)))



# combine dataset
g2f_combined_list <- list(g2f_2021_df, g2f_2022_df, g2f_2023_df)
g2f_combined_df <- bind_rows(g2f_combined_list)


#convert to dataframe
g2f_combined_df <- as.data.frame(g2f_combined_df)






###################### Plant Height ######################

# Select relevant columns and filter out missing rows
g2f_pltht_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             PlantHeight) %>%
                      filter(!is.na(PlantHeight) & !is.na(EnvID))



g2f_pltht_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             PlantHeight) %>%
                      filter(!is.na(PlantHeight) & !is.na(EnvID))

                 


g2f_pltht_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             PlantHeight) %>%
                      filter(!is.na(PlantHeight) & !is.na(EnvID))



            
# Combine dataset
g2f_pltht_list <- list(g2f_pltht_2021_df, g2f_pltht_2022_df, g2f_pltht_2023_df)
g2f_pltht_df <- bind_rows(g2f_pltht_list)


#convert to dataframe
g2f_pltht_df <- as.data.frame(g2f_pltht_df)






###################### Ear Height ######################

# Select relevant columns and filter out missing rows
g2f_earht_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             EarHeight) %>%
                      filter(!is.na(EarHeight) & !is.na(EnvID))



g2f_earht_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             EarHeight) %>%
                      filter(!is.na(EarHeight) & !is.na(EnvID))

                 


g2f_earht_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             EarHeight) %>%
                      filter(!is.na(EarHeight) & !is.na(EnvID))



            
# Combine dataset
g2f_earht_list <- list(g2f_earht_2021_df, g2f_earht_2022_df, g2f_earht_2023_df)
g2f_earht_df <- bind_rows(g2f_earht_list)


#convert to dataframe
g2f_earht_df <- as.data.frame(g2f_earht_df)






###################### Stalk Lodging ######################

# Select relevant columns and filter out missing rows
g2f_stlk_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             StalkLodging) %>%
                      filter(!is.na(StalkLodging) & !is.na(EnvID))



g2f_stlk_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             StalkLodging) %>%
                      filter(!is.na(StalkLodging) & !is.na(EnvID))

                 


g2f_stlk_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             StalkLodging) %>%
                      filter(!is.na(StalkLodging) & !is.na(EnvID))



            
# Combine dataset
g2f_stlk_list <- list(g2f_stlk_2021_df, g2f_stlk_2022_df, g2f_stlk_2023_df)
g2f_stlk_df <- bind_rows(g2f_stlk_list)


#convert to dataframe
g2f_stlk_df <- as.data.frame(g2f_stlk_df)





###################### Grain moisture ######################

# Select relevant columns and filter out missing rows
g2f_moist_2021_df <- g2f_2021_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainMoisture) %>%
                      filter(!is.na(GrainMoisture) & !is.na(EnvID))



g2f_moist_2022_df <- g2f_2022_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainMoisture) %>%
                      filter(!is.na(GrainMoisture) & !is.na(EnvID))

                 


g2f_moist_2023_df <- g2f_2023_vec %>% 
                      mutate(EnvID = paste0(City, Year)) %>%
                      select(Year, 
                             FieldLocation, 
                             State, 
                             City,
                             EnvID,
                             Pedigree,
                             Experiment,
                             Plot, 
                             PlotID,
                             Block, 
                             Replicate,
                             GrainMoisture) %>%
                      filter(!is.na(GrainMoisture) & !is.na(EnvID))



            
# Combine dataset
g2f_moist_list <- list(g2f_moist_2021_df, g2f_moist_2022_df, g2f_moist_2023_df)
g2f_moist_df <- bind_rows(g2f_moist_list)


#convert to dataframe
g2f_moist_df <- as.data.frame(g2f_moist_df)









#######################################################################
g2f_2021_df <- g2f_2021_vec %>% 
  mutate(EnvID = paste0(City, Year)) %>%
  select(Year, 
         FieldLocation, 
         State, 
         City,
         EnvID,
         Pedigree,
         Experiment,
         Plot, 
         PlotID,
         Block, 
         Replicate,
         AnthesisDays,
         SilkingDays,
         PlantHeight,
         EarHeight,
         StalkLodging,
         GrainMoisture,
         GrainYield) %>%
  filter(if_all(everything(), ~ !is.na(.)))



g2f_2022_df <- g2f_2022_vec %>% 
                    mutate(EnvID = paste0(City, Year)) %>%
                    select(Year, 
                           FieldLocation, 
                           State, 
                           City,
                           EnvID,
                           Pedigree,
                           Experiment,
                           Plot, 
                           PlotID,
                           Block, 
                           Replicate,
                           AnthesisDays,
                           SilkingDays,
                           PlantHeight,
                           EarHeight,
                           StalkLodging,
                           GrainMoisture,
                           GrainYield) %>%
                        filter(if_all(everything(), ~ !is.na(.)))



g2f_2023_df <- g2f_2023_vec %>% 
                    mutate(EnvID = paste0(City, Year)) %>%
                    select(Year, 
                           FieldLocation, 
                           State, 
                           City,
                           EnvID,
                           Pedigree,
                           Experiment,
                           Plot, 
                           PlotID,
                           Block, 
                           Replicate,
                           AnthesisDays,
                           SilkingDays,
                           PlantHeight,
                           EarHeight,
                           StalkLodging,
                           GrainMoisture,
                           GrainYield) %>%
                      filter(if_all(everything(), ~ !is.na(.)))



# combine dataset
g2f_combined_list <- list(g2f_2021_df, g2f_2022_df, g2f_2023_df)
g2f_combined_df <- bind_rows(g2f_combined_list)


unique(g2f_combined_df$City)




#convert to dataframe
g2f_combined_df <- as.data.frame(g2f_combined_df)



# combine dataset
g2f_combined_list <- list(g2f_2021_df, g2f_2022_df, g2f_2023_df)
g2f_combined_df <- bind_rows(g2f_combined_list)


#convert to dataframe
g2f_combined_df <- as.data.frame(g2f_combined_df)


# filter into locations
g2f_MN  <- g2f_combined_df %>% filter(State == "MN")
g2f_NE <- g2f_combined_df %>% filter(State == "NE")
g2f_IA <- g2f_combined_df %>% filter(State == "IA")


# export as csv
write.csv(g2f_MN, file = "g2f_MN_phenotypic_clean_data.csv",  row.names = FALSE)
write.csv(g2f_NE, file = "g2f_NE_phenotypic_clean_data.csv",  row.names = FALSE)
write.csv(g2f_IA, file = "g2f_IA_phenotypic_clean_data.csv",  row.names = FALSE)

write.csv(g2f_combined_df, file = "g2f_combined_clean_data.csv",  row.names = FALSE)

#######################################################################




