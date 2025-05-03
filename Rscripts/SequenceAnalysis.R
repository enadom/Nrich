#remotes::install_github("palderman/DSSAT", ref = "develop")
#install.packages("DSSAT")
library(DSSAT)
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(parallel)
library(Rmpi)
library(tools)
options(DSSAT.CSM = "C:\\DSSAT48\\DSCSM048.EXE")


'This simulation for a farmer that uses Nrich on the field and applies only 20kg/ha preplant N
Nrich strip is used to determine topn in-season using NFOA
'
'STEP ONE
RUN the nrich strip simulation with unlimited N level
use data to calculate NDVI
The FileX for the Nrich strip is OKLA9712.SQX
'

filex <- c("OKLA9712.SQX")
write_dssbatch(x = filex, trtno = 1, sq = 1:1)
run_dssat(run_mode = "Q")

data <- read.table("OKLA9712.OPG", header = FALSE, skip = 10, fill = TRUE)# Read the daily output data from the nrich simulation
# Clean and organize the data
daily_nrich <- data[!(data$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_nrich) <- colnames(varnames_opg)
daily_nrich$trt <- 12 # Assign treatment numbers and simulation IDs
daily_nrich <- daily_nrich %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_nrich$YEAR2 <- daily_nrich$sim + 1997# Create a year variable
daily_nrich$TMEAN <- as.numeric(daily_nrich$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_nrich <- daily_nrich %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()



"STEP TWO: Function to calculate NDVI
This function is based on Choudhury et al., 1994 method to calculate NDVI
and cross-referenced from Thorp et al.2012. 
"
NDVI_FUNC <- function(LAI_values, NDVI_MAX, NDVI_MIN, E, B) {
  #NDVI <- NDVI_MAX - (1-(1-exp(-B*LAI_values))^E)*(NDVI_MAX - NDVI_MIN)
  NDVI <- ((1-exp(-B*LAI_values))^E)*(NDVI_MAX - NDVI_MIN) + NDVI_MAX - (1^E * NDVI_MAX - NDVI_MIN)
  return(NDVI)
}
# Define your parameters
NDVI_MAX <- 0.99    # NDVI at full vegetation cover
NDVI_MIN <- 0.150   # NDVI at bare soil from Bushong et al., 2016
E <- 1.3         # function of canopy leaf angle distribution with values near 1.4 for erectophile canopies and near 0.8 for planophile canopies
B <- 0.66         # 

" Get LAI values from the DSSAT simulated data as input for the NDVI_FUNC
Calculate the daily NDVI values and add it to the daily output data
"
LAI_values <- daily_nrich$LAID
daily_nrich$NDVI <- NDVI_FUNC(LAI_values, NDVI_MAX, NDVI_MIN, E, B)
write.csv(daily_nrich, "daily_nrich.csv", row.names = FALSE) # Save the processed data to a CSV file

#Clean NRICH simulation annual data
data <- read_output("OKLA9712.OSU")
annual_nrich <- subset(data, CR != "FA") #remove the observations for Fallow
annual_nrich$trt <- 12
ndvi_155 <- daily_nrich[daily_nrich$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_nrich <- annual_nrich[, c(6, 99, 23, 25, 26, 50)]
names(annual_nrich)[names(annual_nrich) == "HYEAR"] <- "YEAR"
#annual_nrich <- merge(annual_nrich, ndvi_155, by=c("YEAR"), all = TRUE)
annual_nrich <- cbind(annual_nrich, ndvi_155)
write.csv(annual_nrich, "annual_nrich.csv", row.names = FALSE)


'STEP 3
RUN simulation with 20kg/ha level
The FileX for this treatment is OKLA9705.SQX
'
NP20 <- c("OKLA9705.SQX")
write_dssbatch(x = NP20, trtno = 1, sq = 1:1)
run_dssat(run_mode = "Q")

data <- read.table("OKLA9705.OPG", header = FALSE, skip = 10, fill = TRUE)# Read the daily output data from the NP20 simulation
# Clean and organize the data
daily_NP20 <- data[!(data$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP20) <- colnames(varnames_opg)
daily_NP20$trt <- 7# Assign treatment numbers and simulation IDs
daily_NP20 <- daily_NP20 %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP20$YEAR2 <- daily_NP20$sim + 1997# Create a year variable
daily_NP20$TMEAN <- as.numeric(daily_NP20$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP20 <- daily_NP20 %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()


" Get LAI values from the DSSAT simulated data as input for the NDVI_FUNC
Calculate the daily NDVI values and add it to the daily output data
"
LAI_values <- daily_NP20$LAID
daily_NP20$NDVI <- NDVI_FUNC(LAI_values, NDVI_MAX, NDVI_MIN, E, B)
write.csv(daily_NP20, "daily_NP20.csv", row.names = FALSE) # Save the processed data to a CSV file

#Clean NP20 simulation annual data
data <- read_output("OKLA9705.OSU")
annual_NP20 <- subset(data, CR != 2) #remove the observations for Fallow
annual_NP20$trt <- 7

annual_NP20 <- annual_NP20 %>%
  group_by(`P#`) %>%
  mutate(yr = rep(1:1000, length.out = n()))
ndvi_155 <- daily_NP20[daily_NP20$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP20 <- annual_NP20[, c(6, 99, 23, 25, 26, 50,100)]
names(annual_NP20)[names(annual_NP20) == "HYEAR"] <- "YEAR"
#annual_NP20 <- merge(annual_NP20, ndvi_155, by=c("YEAR"), all = TRUE)
annual_NP20 <- cbind(annual_NP20, ndvi_155)
write.csv(annual_NP20, "annual_NP20.csv", row.names = FALSE)


"Nitrogen Fertilization Optimization Algorithm Function
"
TOPN_func <- function(NDVI_Nrich, NDVI_NP, GDD, rho = 0.3) {
  # Parameters
  beta <- 137.2
  alpha <- 1711
  TOPN_factor <- 23.9  
  INSEY <- NDVI_NP / GDD # In-Season Estimated Yield (INSEY)
  YP0 <- alpha * exp(beta * INSEY) # Estimate Potential yield with no added fertilization (YP0)
  RI <- pmax(1.69*(NDVI_Nrich / NDVI_NP)-0.7, 1)  # Estimate in-season nitrogen Response Index (RI)
  #YPN <- pmin(YP0 * RI, 6725)     # Estimate Potential yield with added fertilization (YP_N) but cannot exceed 100bu/ac
  YPN <- YP0 * RI
  TOPN <- TOPN_factor * (((YPN - YP0)/1000)/ rho) # Determine topdress N (TOPN)
  result_df <- data.frame(INSEY = INSEY, YP0 = YP0, RI = RI, YPN = YPN, TOPN = TOPN, GDD=GDD, NDVI_Nrich=NDVI_Nrich, NDVI_NP = NDVI_NP)
  return(result_df)
}

NDVI_Nrich <- annual_nrich$NDVI
NDVI_NP <- annual_NP20$NDVI
GDD <- annual_NP20$gdd

estimated_TOPN <- TOPN_func(NDVI_Nrich, NDVI_NP, GDD)
df <- data.frame(estimated_TOPN)
df$yr <- annual_NP20$yr
df$sim <- rep(1:1000)
df$trt <- 7
df$TOPN <- ifelse(df$TOPN < 0, 0, round(df$TOPN))
write.csv(df, "topndata_20.csv", row.names = FALSE)
df$topn <- df$TOPN 
df <- df %>%
  group_by(yr) %>%
  summarize(topn = mean(topn, na.rm = TRUE))
summary(df)


'STEP4
After obtaining the NDVI from Nrich and NP20, and using TOPN_func to caculate the amount of TOPN required for each
This vector of TOPN is stored in the dataset called df
In this step4, I re-run the simulation for treatment 5, that is 20kg/ha but this time, I run the simulation of 
20kg/ha plus the recommended TOPN from the vector df
To achieve this I re-run the simulation for each year, that 1926/1927 ...... 2021/2022 growing seasons
I modify the original FileX (OKLA9705) to OKLA9715 to set number of simulations from 96 to 1

'
# Read in the modified fileX
file_x <- read_filex('OKLA9715.SQX')

# Loop through the dataframe and update 'topn'
for (i in 1997:2996) {
  file_x$'FERTILIZERS (INORGANIC)' <- 
    file_x$'FERTILIZERS (INORGANIC)' %>%
    mutate_cond(FACD == 'AP001', FAMN = as.integer(df$topn[i-1996]))
  
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLA', i, '.SQX'))
  files <- paste0('OKLA', i, '.SQX') # Create a list of file_X names
  
  # Loop through the years and update SDATE (Simulation start date)
  year <-  i  # Assuming 'i' corresponds to the years starting from 1926
  new_date <- as.POSIXct(paste0(year, '-06-07'))
  file_x$'SIMULATION CONTROLS' <- 
    file_x$'SIMULATION CONTROLS' %>%
    mutate_cond(GENERAL == 'GE', SDATE = new_date)
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLA', i, '.SQX'))
  
  new_date1 <- as.POSIXct(paste0(year, '-10-30'))
  files <- paste0('OKLA', i, '.SQX') # Create a list of file_X names
  file_x$'PLANTING DETAILS' <-
    file_x$'PLANTING DETAILS' %>%
    mutate_cond(PLME=='S', PDATE = new_date1)
  write_filex(file_x, paste0('OKLA', i, '.SQX'))# Overwrite the original file with new values
  
  new_date2 <- as.POSIXct(paste0(year, '-10-07'))
  files <- paste0('OKLA', i, '.SQX') # Create a list of file_X names
  file_x$'INITIAL CONDITIONS' <-
    file_x$'INITIAL CONDITIONS' %>%
    mutate_cond(PCR=='WH', ICDAT = new_date2)
  write_filex(file_x, paste0('OKLA', i, '.SQX'))# Overwrite the original file with new values
  
  files <- paste0('OKLA', i, '.SQX')# Create a list of file_X names
  # Run DSSAT
  write_dssbatch(x = files, trtno = 1, sq = 1)
  run_dssat(run_mode = "Q")
}

#Collect and clean annual data
all_datasets1 <- list() # Initialize an empty list to store individual datasets
alldaily1 <- list()

# Loop through the files .OSU annual files
for (i in 1997:2996) {
  #formatted_i <- sprintf("%02d", i)# Format i with leading zeros
  output_file <- read_output(paste0('OKLA', i, '.OSU'))
  dailyoutput <- data <- read.table(paste0('OKLA', i, '.OPG'), header = FALSE, skip = 10, fill = TRUE)
  
  all_datasets1[[i]] <- output_file # Add the dataset to the list
  alldaily1[[i]]    <- dailyoutput
}

# Combine all datasets into one
combined_daily1 <- do.call(rbind, alldaily1)
daily_NP20_rerun <- combined_daily1[!(combined_daily1$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP20_rerun) <- colnames(varnames_opg)
daily_NP20_rerun$trt <- 7# Assign treatment numbers and simulation IDs
daily_NP20_rerun <- daily_NP20_rerun %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP20_rerun$YEAR2 <- daily_NP20_rerun$sim + 1997# Create a year variable
daily_NP20_rerun$TMEAN <- as.numeric(daily_NP20_rerun$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP20_rerun <- daily_NP20_rerun %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()
write.csv(daily_NP20_rerun, "daily_NP20_rerun.csv", row.names = FALSE)

#Collect and clean Annual data
combined_dataset1 <- do.call(rbind, all_datasets1)
annual_NP20_rerun <- subset(combined_dataset1, RUNNO != 2) #deleting observations for Fallow
annual_NP20_rerun$trt <- 7
ndvi_155 <- daily_NP20[daily_NP20$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP20_rerun <- annual_NP20_rerun[, c(6, 5, 99, 12, 17, 18, 19, 20, 21, 22, 23, 25, 26, 50)]
names(annual_NP20_rerun)[names(annual_NP20_rerun) == "HYEAR"] <- "YEAR"
#annual_NP20_rerun <- cbind(annual_NP20_rerun, ndvi_155)
summary(annual_NP20_rerun$HWAM)
write.csv(annual_NP20_rerun, "annual_NP20_rerun.csv", row.names = FALSE)



'STEP 5
RUN simulation with 30kg/ha level treatment
The FileX for this treatment is OKLA9706.SQX
'
NP30 <- c("OKLA9706.SQX")
write_dssbatch(x = NP30, trtno = 1, sq = 1)
run_dssat(run_mode = "Q")

data <- read.table("OKLA9706.OPG", header = FALSE, skip = 10, fill = TRUE)# Read the daily output data from the NP30 simulation
# Clean and organize the data
daily_NP30 <- data[!(data$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP30) <- colnames(varnames_opg)
daily_NP30$trt <- 8 # Assign treatment numbers and simulation IDs
daily_NP30 <- daily_NP30 %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP30$YEAR2 <- daily_NP30$sim + 1997# Create a year variable
daily_NP30$TMEAN <- as.numeric(daily_NP30$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP30 <- daily_NP30 %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()


" Get LAI values from the DSSAT simulated data as input for the NDVI_FUNC
Calculate the daily NDVI values and add it to the daily output data
"
LAI_values <- daily_NP30$LAID
daily_NP30$NDVI <- NDVI_FUNC(LAI_values, NDVI_MAX, NDVI_MIN, E, B)
write.csv(daily_NP30, "daily_NP30.csv", row.names = FALSE) # Save the processed data to a CSV file

#Clean NP30 simulation annual data
data <- read_output("OKLA9706.OSU")
annual_NP30 <- subset(data, CR != 2) #remove the observations for Fallow
annual_NP30$trt <- 8

annual_NP30 <- annual_NP30 %>%
  group_by(`P#`) %>%
  mutate(yr = rep(1:1000, length.out = n()))
ndvi_155 <- daily_NP30[daily_NP30$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP30 <- annual_NP30[, c(6, 99, 23, 25, 26, 50,100)]
names(annual_NP30)[names(annual_NP30) == "HYEAR"] <- "YEAR"
#annual_NP30 <- merge(annual_NP30, ndvi_155, by=c("YEAR"), all = TRUE)
annual_NP30 <- cbind(annual_NP30, ndvi_155)
write.csv(annual_NP30, "annual_NP30.csv", row.names = FALSE)

#Get NDVI AND CALCULATE TOPN
NDVI_Nrich <- annual_nrich$NDVI
NDVI_NP <- annual_NP30$NDVI
GDD <- annual_NP30$gdd

estimated_TOPN <- TOPN_func(NDVI_Nrich, NDVI_NP, GDD)
df <- data.frame(estimated_TOPN)
df$yr <- annual_NP30$yr
df$trt <- 8
df$TOPN <- ifelse(df$TOPN < 0, 0, round(df$TOPN))
write.csv(df, "topndata_30.csv", row.names = FALSE)
df$topn <- df$TOPN 
df <- df %>%
  group_by(yr) %>%
  summarize(topn = mean(topn, na.rm = TRUE))
summary(df)


'STEP6
After obtaining the NDVI from Nrich and NP30, and using TOPN_func to caculate the amount of TOPN required for each
This vector of TOPN is stored in the dataset called df
In this step6, I re-run the simulation for treatment 8, that is 30kg/ha but this time, I run the simulation of 
30kg/ha plus the recommended TOPN from the vector df
To achieve this I re-run the simulation for each year, that 1926/1927 ...... 2021/2022 growing seasons
I modify the original FileX (OKLA9705) to OKLA9716 to set number of simulations from 96 to 1

'
# Read in the modified fileX
file_x <- read_filex('OKLA9716.SQX')

# Loop through the dataframe and update 'topn'
for (i in 1997:2996) {
  file_x$'FERTILIZERS (INORGANIC)' <- 
    file_x$'FERTILIZERS (INORGANIC)' %>%
    mutate_cond(FMCD == 'FE038', FAMN = as.integer(df$topn[i-1996]))
  
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLO', i, '.SQX'))
  files <- paste0('OKLO', i, '.SQX') # Create a list of file_X names
  
  # Loop through the years and update SDATE (Simulation start date)
  year <-  i  # Assuming 'i' corresponds to the years starting from 1926
  new_date <- as.POSIXct(paste0(year, '-06-07'))
  file_x$'SIMULATION CONTROLS' <- 
    file_x$'SIMULATION CONTROLS' %>%
    mutate_cond(GENERAL == 'GE', SDATE = new_date)
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLO', i, '.SQX'))
  
  new_date1 <- as.POSIXct(paste0(year, '-10-30'))
  files <- paste0('OKLO', i, '.SQX') # Create a list of file_X names
  file_x$'PLANTING DETAILS' <-
    file_x$'PLANTING DETAILS' %>%
    mutate_cond(PLME=='S', PDATE = new_date1)
  write_filex(file_x, paste0('OKLO', i, '.SQX'))# Overwrite the original file with new values
  
  new_date2 <- as.POSIXct(paste0(year, '-10-07'))
  files <- paste0('OKLO', i, '.SQX') # Create a list of file_X names
  file_x$'INITIAL CONDITIONS' <-
    file_x$'INITIAL CONDITIONS' %>%
    mutate_cond(PCR=='WH', ICDAT = new_date2)
  write_filex(file_x, paste0('OKLO', i, '.SQX'))# Overwrite the original file with new values
  
  files <- paste0('OKLO', i, '.SQX')# Create a list of file_X names
  # Run DSSAT
  write_dssbatch(x = files, trtno = 1, sq = 1)
  run_dssat(run_mode = "Q")
}


#Collect and clean annual data
all_datasets2 <- list() # Initialize an empty list to store individual datasets
alldaily2 <- list()

# Loop through the files .OSU annual files
for (i in 1997:2996) {
  #formatted_i <- sprintf("%02d", i)# Format i with leading zeros
  output_file <- read_output(paste0('OKLO', i, '.OSU'))
  dailyoutput <- data <- read.table(paste0('OKLO', i, '.OPG'), header = FALSE, skip = 10, fill = TRUE)
  
  all_datasets2[[i]] <- output_file # Add the dataset to the list
  alldaily2[[i]]    <- dailyoutput
}

# Combine all datasets into one
combined_daily2 <- do.call(rbind, alldaily2)
daily_NP30_rerun <- combined_daily2[!(combined_daily2$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP30_rerun) <- colnames(varnames_opg)
daily_NP30_rerun$trt <- 8 # Assign treatment numbers and simulation IDs
daily_NP30_rerun <- daily_NP30_rerun %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP30_rerun$YEAR2 <- daily_NP30_rerun$sim + 1997# Create a year variable
daily_NP30_rerun$TMEAN <- as.numeric(daily_NP30_rerun$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP30_rerun <- daily_NP30_rerun %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()
write.csv(daily_NP30_rerun, "daily_NP30_rerun.csv", row.names = FALSE)

# Combine all datasets into one
combined_dataset2 <- do.call(rbind, all_datasets2)
annual_NP30_rerun <- subset(combined_dataset2, RUNNO != 2) #deleting observations for Fallow
annual_NP30_rerun$trt <- 8
ndvi_155 <- daily_NP30[daily_NP30$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP30_rerun <- annual_NP30_rerun[, c(6, 5, 99, 12, 17, 18, 19, 20, 21, 22, 23, 25, 26, 50)]
names(annual_NP30_rerun)[names(annual_NP30_rerun) == "HYEAR"] <- "YEAR"
#annual_NP30_rerun <- cbind(annual_NP30_rerun, ndvi_155)
summary(annual_NP30_rerun$HWAM)
write.csv(annual_NP30_rerun, "annual_NP30_rerun.csv", row.names = FALSE)


####################################################################################################################
'STEP 7
RUN simulation with 40kg/ha level treatment
The FileX for this treatment is OKLA9707.SQX
'
NP40 <- c("OKLA9707.SQX")
write_dssbatch(x = NP40, trtno = 1, sq = 1)
run_dssat(run_mode = "Q")

data <- read.table("OKLA9707.OPG", header = FALSE, skip = 10, fill = TRUE)# Read the daily output data from the NP30 simulation
# Clean and organize the data
daily_NP40 <- data[!(data$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP40) <- colnames(varnames_opg)
daily_NP40$trt <- 9 # Assign treatment numbers and simulation IDs
daily_NP40 <- daily_NP40 %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP40$YEAR2 <- daily_NP40$sim + 1997# Create a year variable
daily_NP40$TMEAN <- as.numeric(daily_NP40$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP40 <- daily_NP40 %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()


" Get LAI values from the DSSAT simulated data as input for the NDVI_FUNC
Calculate the daily NDVI values and add it to the daily output data
"
LAI_values <- daily_NP40$LAID
daily_NP40$NDVI <- NDVI_FUNC(LAI_values, NDVI_MAX, NDVI_MIN, E, B)
write.csv(daily_NP40, "daily_NP40.csv", row.names = FALSE) # Save the processed data to a CSV file

#Clean NP40 simulation annual data
data <- read_output("OKLA9707.OSU")
annual_NP40 <- subset(data, CR != 2) #remove the observations for Fallow
annual_NP40$trt <- 9

annual_NP40 <- annual_NP40 %>%
  group_by(`P#`) %>%
  mutate(yr = rep(1:1000, length.out = n()))
ndvi_155 <- daily_NP40[daily_NP40$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP40 <- annual_NP40[, c(6, 99, 23, 25, 26, 50,100)]
names(annual_NP40)[names(annual_NP40) == "HYEAR"] <- "YEAR"
#annual_NP40 <- merge(annual_NP40, ndvi_155, by=c("YEAR"), all = TRUE)
annual_NP40 <- cbind(annual_NP40, ndvi_155)
write.csv(annual_NP40, "annual_NP40.csv", row.names = FALSE)

#Get NDVI AND CALCULATE TOPN
NDVI_Nrich <- annual_nrich$NDVI
NDVI_NP <- annual_NP40$NDVI
GDD <- annual_NP40$gdd

estimated_TOPN <- TOPN_func(NDVI_Nrich, NDVI_NP, GDD)
df <- data.frame(estimated_TOPN)
df$yr <- annual_NP40$yr
df$trt <- 9
df$TOPN <- ifelse(df$TOPN < 0, 0, round(df$TOPN))
write.csv(df, "topndata_40.csv", row.names = FALSE)
df$topn <- df$TOPN 
df <- df %>%
  group_by(yr) %>%
  summarize(topn = mean(topn, na.rm = TRUE))
summary(df)


'STEP8
After obtaining the NDVI from Nrich and NP40, and using TOPN_func to caculate the amount of TOPN required for each
This vector of TOPN is stored in the dataset called df
In this step8, I re-run the simulation for treatment 9, that is 40kg/ha but this time, I run the simulation of 
40kg/ha plus the recommended TOPN from the vector df
To achieve this I re-run the simulation for each year, that 1926/1927 ...... 2021/2022 growing seasons
I modify the original FileX (OKLA9705) to OKLA9717 to set number of simulations from 96 to 1

'
# Read in the modified fileX
file_x <- read_filex('OKLA9717.SQX')

# Loop through the dataframe and update 'topn'
for (i in 1997:2996) {
  file_x$'FERTILIZERS (INORGANIC)' <- 
    file_x$'FERTILIZERS (INORGANIC)' %>%
    mutate_cond(FACD == 'AP001', FAMN = as.integer(df$topn[i-1996]))
  
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLU', i, '.SQX'))
  files <- paste0('OKLU', i, '.SQX') # Create a list of file_X names
  
  # Loop through the years and update SDATE (Simulation start date)
  year <-  i  # Assuming 'i' corresponds to the years starting from 1926
  new_date <- as.POSIXct(paste0(year, '-06-07'))
  file_x$'SIMULATION CONTROLS' <- 
    file_x$'SIMULATION CONTROLS' %>%
    mutate_cond(GENERAL == 'GE', SDATE = new_date)
  # Overwrite the original file with new values
  write_filex(file_x, paste0('OKLU', i, '.SQX'))
  
  new_date1 <- as.POSIXct(paste0(year, '-10-30'))
  files <- paste0('OKLU', i, '.SQX') # Create a list of file_X names
  file_x$'PLANTING DETAILS' <-
    file_x$'PLANTING DETAILS' %>%
    mutate_cond(PLME=='S', PDATE = new_date1)
  write_filex(file_x, paste0('OKLU', i, '.SQX'))# Overwrite the original file with new values
  
  new_date2 <- as.POSIXct(paste0(year, '-10-07'))
  files <- paste0('OKLU', i, '.SQX') # Create a list of file_X names
  file_x$'INITIAL CONDITIONS' <-
    file_x$'INITIAL CONDITIONS' %>%
    mutate_cond(PCR=='WH', ICDAT = new_date2)
  write_filex(file_x, paste0('OKLU', i, '.SQX'))# Overwrite the original file with new values
  
  files <- paste0('OKLU', i, '.SQX')# Create a list of file_X names
  # Run DSSAT
  write_dssbatch(x = files, trtno = 1, sq = 1)
  run_dssat(run_mode = "Q")
}

#Collect and clean annual data
all_datasets3 <- list() # Initialize an empty list to store individual datasets
alldaily3 <- list()

# Loop through the files .OSU annual files
for (i in 1997:2996) {
  #formatted_i <- sprintf("%02d", i)# Format i with leading zeros
  output_file <- read_output(paste0('OKLU', i, '.OSU'))
  dailyoutput <- data <- read.table(paste0('OKLU', i, '.OPG'), header = FALSE, skip = 10, fill = TRUE)
  
  all_datasets3[[i]] <- output_file # Add the dataset to the list
  alldaily3[[i]]    <- dailyoutput
}

# Combine all datasets into one
combined_daily3 <- do.call(rbind, alldaily3)
daily_NP40_rerun <- combined_daily3[!(combined_daily3$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_NP40_rerun) <- colnames(varnames_opg)
daily_NP40_rerun$trt <- 9 # Assign treatment numbers and simulation IDs
daily_NP40_rerun <- daily_NP40_rerun %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_NP40_rerun$YEAR2 <- daily_NP40_rerun$sim + 1996# Create a year variable
daily_NP40_rerun$TMEAN <- as.numeric(daily_NP40_rerun$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_NP40_rerun <- daily_NP40_rerun %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()
write.csv(daily_NP40_rerun, "daily_NP40_rerun.csv", row.names = FALSE)

# Combine all datasets into one
combined_dataset3 <- do.call(rbind, all_datasets3)
annual_NP40_rerun <- subset(combined_dataset3, RUNNO != 2) #deleting observations for Fallow
annual_NP40_rerun$trt <- 9
ndvi_155 <- daily_NP40[daily_NP40$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_NP40_rerun <- annual_NP40_rerun[, c(6, 5, 99, 12, 17, 18, 19, 20, 21, 22, 23, 25, 26, 50)]
names(annual_NP40_rerun)[names(annual_NP40_rerun) == "HYEAR"] <- "YEAR"
#annual_NP30_rerun <- cbind(annual_NP30_rerun, ndvi_155)
summary(annual_NP40_rerun$HWAM)
write.csv(annual_NP40_rerun, "annual_NP40_rerun.csv", row.names = FALSE)


###############################################################################################################
'STEP 9
RUN SIMULATIONS for farmer conventional practice
I consider six different combinations of N level treatments
'

# Define the file names
file_names <- paste0("OKLA960", 4:5, ".SQX")

# Loop over the file names
for (filex in file_names) {
  # Generate DSSAT batch file
  write_dssbatch(x = filex, trtno = 1:1, sq = 1:1)
  # Run DSSAT simulations
  run_dssat(run_mode = "Q")
}

#Collect and clean annual data
all_datasets4 <- list() # Initialize an empty list to store individual datasets
alldaily4 <- list()

# Loop through the files .OSU annual files
for (i in 9604:9605) {
  #formatted_i <- sprintf("%02d", i)# Format i with leading zeros
  output_file <- read_output(paste0('OKLA', i, '.OSU'))
  dailyoutput <- data <- read.table(paste0('OKLA', i, '.OPG'), header = FALSE, skip = 10, fill = TRUE)
  
  all_datasets4[[i]] <- output_file # Add the dataset to the list
  alldaily4[[i]]    <- dailyoutput
}

# Combine all daily datasets into one
combined_daily4 <- do.call(rbind, alldaily4)
daily_conv <- combined_daily4[!(combined_daily4$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_conv) <- colnames(varnames_opg)
daily_conv$trt <- rep(4:5, each=215895) # Assign treatment numbers and simulation IDs
daily_conv <- daily_conv %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_conv$YEAR2 <- daily_conv$sim + 1996# Create a year variable
daily_conv$TMEAN <- as.numeric(daily_conv$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_conv <- daily_conv %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()
write.csv(daily_conv, "daily_conv.csv", row.names = FALSE)

# Combine all annual datasets into one
combined_dataset4 <- do.call(rbind, all_datasets4)
annual_conv <- subset(combined_dataset4, CR != "FA") #deleting observations for Fallow
annual_conv$trt <- rep(5:7, each=100)
#ndvi_155 <- daily_conv[daily_conv$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_conv <- annual_conv[, c(6, 5, 99, 12, 17, 18, 19, 20, 21, 22, 23, 25, 26, 50)]
names(annual_conv)[names(annual_conv) == "HYEAR"] <- "YEAR"
write.csv(annual_conv, "annual_2.csv", row.names = FALSE)

summary(annual_conv$HWAM)
summary(annual_NP20_rerun$HWAM)
summary(annual_NP30_rerun$HWAM)
summary(annual_NP40_rerun$HWAM)


#Combined annual yield data
annualdata <- rbind(annual_conv, annual_NP20_rerun, annual_NP30_rerun, annual_NP40_rerun)
names(annualdata) <- c("crop", "rep", "trt", "wyear", "sdat", "pdat", "edat", "adat", "mdat", "hdat", "year", "biomass", "yield", "totalN")
annualdata$preplantN <- c(100, 120, 140, 160, 90, 80, 20, 30, 40)[annualdata$trt]
annualdata$topn <- annualdata$totalN - annualdata$preplantN
annualdata$system <- ifelse(annualdata$trt < 7, "conventional", "NRS")
write.csv(annualdata, "annualdata.csv", row.names = FALSE)

summary_by_trt <- annualdata %>%
  group_by(trt) %>%
  summarise(mean_HWAM = mean(yield),
            median_HWAM = median(yield),
            min_HWAM = min(yield),
            max_HWAM = max(yield),
            sd_HWAM = sd(yield),
            n = n())

summary_by_trt

dailydata <- rbind(daily_conv, daily_NP20_rerun, daily_NP30_rerun, daily_NP40_rerun)
write.csv(dailydata, "dailydata.csv", row.names = FALSE)



'STEP 10
RUN SIMULATIONS for farmer conventional practice using measured data
I consider six different combinations of N level treatments
'

# Define the file names
file_names <- c("OKLA9607.SQX")#, "OKLA9608.SQX", "OKLA9609.SQX", "OKLA9610.SQX", "OKLA9611.SQX", "OKLA9612.SQX")

# Loop over the file names
for (filex in file_names) {
  # Generate DSSAT batch file
  write_dssbatch(x = filex, trtno = 1:1, sq = 1:1)
  # Run DSSAT simulations
  run_dssat(run_mode = "Q")
}

#Collect and clean annual data
all_datasets5 <- list() # Initialize an empty list to store individual datasets
alldaily5 <- list()

# Loop through the files .OSU annual files
for (i in 9607:9607) {
  #formatted_i <- sprintf("%02d", i)# Format i with leading zeros
  output_file <- read_output(paste0('OKLA', i, '.OSU'))
  dailyoutput <- data <- read.table(paste0('OKLA', i, '.OPG'), header = FALSE, skip = 10, fill = TRUE)
  
  all_datasets5[[i]] <- output_file # Add the dataset to the list
  alldaily5[[i]]    <- dailyoutput
}

# Combine all daily datasets into one
combined_daily5 <- do.call(rbind, alldaily5)
daily_conv_m <- combined_daily5[!(combined_daily5$V1 %in% c("*DSSAT", "*RUN", "MODEL", "EXPERIMENT", "DATA", "TREATMENT", "@YEAR")), ]
varnames_opg <- read_excel("varnames_opg.xlsx") # Assign variable names
colnames(daily_conv_m) <- colnames(varnames_opg)
daily_conv_m$trt <- 7#rep(7:12, each=5790) # Assign treatment numbers and simulation IDs
daily_conv_m <- daily_conv_m %>%
  group_by(trt) %>%
  mutate(sim = cumsum(DAP == 0))
daily_conv_m$YEAR2 <- daily_conv_m$sim + 1996# Create a year variable
daily_conv_m$TMEAN <- as.numeric(daily_conv_m$TMEAN)
#if mean temp greater > 4.4 growth possible, calculate gdd as cummulative sum of days the growth was possible
daily_conv_m <- daily_conv_m %>%
  group_by(sim) %>%
  mutate(gddc = as.numeric(TMEAN) - 4.4 > 0,
         gdd = cumsum(gddc)) %>%
  ungroup()
write.csv(daily_conv_m, "daily_trt7.csv", row.names = FALSE)

# Combine all annual datasets into one
combined_dataset5 <- do.call(rbind, all_datasets5)
annual_conv_m <- subset(combined_dataset5, CR != 2) #deleting observations for Fallow
annual_conv_m$trt <- 7#rep(7:12, each=135)
#ndvi_155 <- daily_conv[daily_conv$DAP==156, c(54,55)] #NDVI reading at 180 days after planting
annual_conv_m <- annual_conv_m[, c(6, 5, 99, 12, 17, 18, 19, 20, 21, 22, 23, 25, 26, 50)]
names(annual_conv_m) <- c("crop", "rep", "trt1", "wyear1", "sdat1", "pdat1", "edat1", "adat1", "mdat1", "hdat1", "year1", "biomass", "yield", "totalN1")
write.csv(annual_conv_m, "annual_conv_trt7.csv", row.names = FALSE)
summary(annual_conv_m$yield)


comparisondata <- cbind(annual_conv, annual_conv_m)
comparisondata$sim <- comparisondata$WYEAR-1996
write.csv(comparisondata, "comparisondata.csv", row.names = FALSE)
library(ggplot2)
ggplot(comparisondata, aes(x = yield, y = HWAM)) +
  geom_point() +  # Add points
  labs(x = "Yield (kg/ha) using observed weather data", y = "Yield (kg/ha) using simulated weather data") +  # Label axes
  ggtitle("Scatter plot of simulated yield using observed weather data vs simulated data") +  # Add title
  scale_x_continuous(limits = c(1000, max(comparisondata$yield))) +  # Set x-axis limits
  theme_minimal() +  # Set theme (optional)
  theme(axis.line = element_line(color = "gray", size = 0.5))  # Add axis line

ggplot(comparisondata) +
  geom_point(aes(x = sim, y = HWAM, shape = "WGEN data"), color = "blue") +  # Add points for HWAM
  geom_point(aes(x = sim, y = yield, shape = "Observed data"), color = "red") +  # Add points for yield
  scale_shape_manual(name = "Weather data", values = c("WGEN data" = 16, "Observed data" = 17)) +  # Define custom shapes and legend
  labs(x = "Harvest year", y = "Yield (kg/ha)", shape = "Variable") +  # Label axes and legend
  ggtitle("Comparison of simulated yield using observed weather data vs simulated data") +  # Add title
  theme_minimal() +  # Set theme (optional)
  theme(axis.line = element_line(color = "gray", size = 0.5),  # Add axis line
        legend.position = "bottom")  # Change legend position to bottom

ggplot(annual_NP20_rerun, aes(x = WYEAR, y = totalN, color = system)) +
  geom_point(size = 2, alpha = 0.8) +  # Adjust point size and transparency
  labs(x = "Simulation", y = "Total N (kg/ha)", color = "System") +
  theme_minimal() +  # Use minimal theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Title appearance
    axis.title = element_text(size = 12),  # Axis title appearance
    axis.text = element_text(size = 10),  # Axis text appearance
    legend.title = element_text(size = 12),  # Legend title appearance
    legend.text = element_text(size = 10),  # Legend text appearance
    axis.line = element_line(color = "gray", size = 1) 
  )
