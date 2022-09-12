# Load PPA data

library(zoo)
library(xts)
library(lubridate)

colname_in = c("datetime_in" ,"mac_address_in" ,"firmware_ver_in", "hardware_in" ,"current_temp_f_in",    
               "current_humidity_in",   "current_dewpoint_f_in", "pressure_in" ,          "adc_in" ,               "mem_in",               
               "rssi_in" ,              "uptime_in"  ,           "pm1_0_cf_1_in" ,        "pm2_5_cf_1_in"  ,       "pm10_0_cf_1_in" ,      
               "pm1_0_atm_in"  ,        "pm2_5_atm_in"  ,        "pm10_0_atm_in" ,        "pm2.5_aqi_cf_1_in",     "pm2.5_aqi_atm_in" ,    
               "p_0_3_um_in" ,          "p_0_5_um_in" ,          "p_1_0_um_in"  ,         "p_2_5_um_in"   ,        "p_5_0_um_in"   ,       
               "p_10_0_um_in" ,         "pm1_0_cf_1_b_in" ,      "pm2_5_cf_1_b_in"  ,     "pm10_0_cf_1_b_in"  ,    "pm1_0_atm_b_in" ,      
               "pm2_5_atm_b_in" ,       "pm10_0_atm_b_in" ,      "pm2.5_aqi_cf_1_b_in",   "pm2.5_aqi_atm_b_in",    "p_0_3_um_b_in"  ,      
               "p_0_5_um_b_in"  ,       "p_1_0_um_b_in"  ,       "p_2_5_um_b_in"   ,      "p_5_0_um_b_in" ,        "p_10_0_um_b_in"  ,     
               "gas_in"  ,              "dateNumbers_in" ,       "DT_year_in"  ,          "DT_month_in" ,          "DT_day_in"  ,          
               "DT_hr_in" ,             "daynight_idx_in" ,      "site_id_in" ,           "site_name_in"  ,        "season_id_in" ,        
               "inout_id_in" ,          "R_pm2_5_in")



colname_out = c("datetime_out" ,"mac_address_out" ,"firmware_ver_out", "hardware_out" ,"current_temp_f_out",    
                "current_humidity_out",   "current_dewpoint_f_out", "pressure_out" ,          "adc_out" ,               "mem_out",               
                "rssi_out" ,              "uptime_out"  ,           "pm1_0_cf_1_out" ,        "pm2_5_cf_1_out"  ,       "pm10_0_cf_1_out" ,      
                "pm1_0_atm_out"  ,        "pm2_5_atm_out"  ,        "pm10_0_atm_out" ,        "pm2.5_aqi_cf_1_out",     "pm2.5_aqi_atm_out" ,    
                "p_0_3_um_out" ,          "p_0_5_um_out" ,          "p_1_0_um_out"  ,         "p_2_5_um_out"   ,        "p_5_0_um_out"   ,       
                "p_10_0_um_out" ,         "pm1_0_cf_1_b_out" ,      "pm2_5_cf_1_b_out"  ,     "pm10_0_cf_1_b_out"  ,    "pm1_0_atm_b_out" ,      
                "pm2_5_atm_b_out" ,       "pm10_0_atm_b_out" ,      "pm2.5_aqi_cf_1_b_out",   "pm2.5_aqi_atm_b_out",    "p_0_3_um_b_out"  ,      
                "p_0_5_um_b_out"  ,       "p_1_0_um_b_out"  ,       "p_2_5_um_b_out"   ,      "p_5_0_um_b_out" ,        "p_10_0_um_b_out"  ,     
                "gas_out"  ,              "dateNumbers_out" ,       "DT_year_out"  ,          "DT_month_out" ,          "DT_day_out"  ,          
                "DT_hr_out" ,             "daynight_idx_out" ,      "site_id_out" ,           "site_name_out"  ,        "season_id_out" ,        
                "inout_id_out" ,          "R_pm2_5_out")




# Load daily PPA files

t_data1 <- read.csv("20220223_s17_W_In.csv")
nrow(t_data1)
t_data2 <- read.csv("20220224_s17_W_In.csv")
nrow(t_data2)
t_data3 <- read.csv("20220225_s17_W_In.csv")
nrow(t_data3)
t_data4 <- read.csv("20220226_s17_W_In.csv")
nrow(t_data4)
t_data5 <- read.csv("20220227_s17_W_In.csv")
nrow(t_data5)
t_data6 <- read.csv("20220228_s17_W_In.csv")
nrow(t_data6)
t_data7 <- read.csv("20220304_s17_W_In.csv")
nrow(t_data7)


# combined PPA files

t_data <- rbind(t_data1, t_data2,t_data3,t_data4,t_data5,t_data6,t_data7)
nrow(t_data)

# change UTC_time to local time
Sys.setenv(TZ="Asia/Dhaka")    # function to get different time zone name : OlsonNames()

colnames(t_data)[1] <- "datetime" # rename the column_1 name  "UTCDateTime" to "datetime"
t_data[-1] <- lapply(t_data[-1], function(x) as.numeric(gsub(",", "", x)))
t_data$datetime <- as.POSIXct(t_data$datetime, format ="%Y/%m/%dT%H:%M:%Sz", tz='UTC')   # format for ramp data: format ='%m/%d/%Y %H:%M:%S', tz=''
t_data$datetime <- with_tz(t_data$datetime, "Asia/Dhaka")
t_data <- subset(t_data,!is.na(t_data$datetime))
nrow(t_data)


t_data$dateNumbers <- as.numeric(t_data$datetime)
t_data$DT_year <- format (t_data$datetime, format = '%Y')
t_data$DT_month <- format (t_data$datetime, format = '%m')
t_data$DT_day <- format (t_data$datetime, format = '%d')
t_data$DT_hr <- format (t_data$datetime, format = '%H')


t_data.xts <- xts(t_data[, -1], order.by=as.POSIXct(t_data$datetime))
ends <- endpoints(t_data.xts,'hours',1)
t_data_hrly <- period.apply(t_data.xts,ends ,mean)
t_data_hrly <- align.time(t_data_hrly, 3600)
t_data_hrly$daynight_idx <- 1
t_data_hrly$daynight_idx <- ifelse((t_data_hrly$DT_hr < 7 | t_data_hrly$DT_hr > 19), 0, 1)

write.zoo(t_data_hrly,file="temp_zoo.csv",index.name="datetime",row.names=FALSE,col.names=TRUE,sep=",")
t_data_hrly <- read.csv("temp_zoo.csv")


# add site specific information

t_data_hrly$site_id <- 17      # give the site_id
t_data_hrly$site_name <- "mirpur"
t_data_hrly$season_id <- "winter"   #  "monsoon"; "winter"
t_data_hrly$inout_id <-  "indoor"    # "outdoor"; "indoor"

t_data_hrly$datetime <- as_datetime(t_data_hrly$datetime)
t_data_hrly$R_pm2_5 <- t_data_hrly$pm2_5_cf_1/t_data_hrly$pm2_5_cf_1_b

par(mfrow=c(2,2))
plot(t_data_hrly$datetime, t_data_hrly$pm2_5_cf_1)
plot(t_data_hrly$datetime, t_data_hrly$pm2_5_cf_1_b)
plot(t_data_hrly$datetime, t_data_hrly$R_pm2_5)

t_data_4_save <- t_data_hrly

colnames(t_data_4_save) <- colname_in  # change here

write.csv(t_data_4_save, "PPA_s17_mirpur_winter_indoor.csv")


