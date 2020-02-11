##################################################################################
#Name: BiodiversiTREE Data Orgigami (V3) 
#Coder: C. Nathan Jones
#Date: 11/29/2018
#Purpose: Organize raw soil moisture sensor data
##################################################################################

##################################################################################
#Step 1:  Setup Workspace---------------------------------------------------------
##################################################################################
#Clear Memory
rm(list=ls(all=TRUE))

#add appropriate libarary
library(rodm2) #devtools::install_github("khondula/rodm2", force =T) 
library(DBI)
library(data.table)
library(tidyverse)
library(lubridate)
library(parallel)

#Define working directory
working_dir<-"C:/Users/pullenj/Dropbox (Smithsonian)/BDT_Sensor_Data/"
#db_loc <- file.path(working_dir, "BiodiversiTREE.sqlite")
#db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))

##################################################################################
#Step 2:  Setup Database  (Only run this once!) ----------------------------------
##################################################################################
#Create SQLight databse~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db <- create_sqlite(dir = working_dir,
                    filename="BiodiversiTREE",
                    connect = F)
db_loc <- file.path(working_dir, "BiodiversiTREE.sqlite")

#Connect to database
db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))

#Create list of sensors in the database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of sensors
sensors<-read.csv(paste0(working_dir,"Experimental Design Table/plot.csv")) %>%
  select(plot, subplot, Sensor) %>%
  distinct(.)

#Create function to describe equipment in db
fun<-function(n){
  #describe equipment
  db_describe_equipment(db,
                        equip_name    =   paste0(sensors$plot[n], sensors$subplot[n], "_",sensors$Sensor[n]),
                        serial_no     =   123456789,
                        model_name    =   "model1",
                        vendor        =   "vendor name",
                        owner_first   =   "owner_first",
                        owner_last    =   "owner_last",
                        owner_email   =   "owner_email",
                        equipment_type=   "Sensor",
                        manufacturer  =   "Sentek")}

#Run function
lapply(seq(1,length(sensors[,1])), fun)

#Clean up workspace
remove(fun, sensors)

#Create list of sites in the database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of plots
sites<-read.csv(paste0(working_dir,"Experimental Design Table/plot.csv")) %>%
  select(plot, subplot) %>%
  distinct(.)

#Create function to describe sites the database
fun<-function(n){
  #describe site
  db_describe_site(db, site_code = paste0(sites$plot[n], sites$subplot[n]))
}

#run function
lapply(seq(1, length(sites[,1])), fun)


#Desicribe Method~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_describe_method(db,
                   methodname =   "BDT Data Download",
                   methodcode =   "BDT Data Download",
                   methodtypecv = "Instrument deployment")

#Describe Variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create vector of sensor depths and measurement codes
depth<-c(2,12,22,32,42,52,62,75,82)

#Create function to describe variable
fun<-function(n){
  db_describe_variable(db,
                       variabletypecv = "Hydrology",
                       variablecode   = paste0("VWC_",depth[n]),
                       variablenamecv = "volumetricWaterContent")
  #Othe Variables to add later
  # db_describe_variable(db,
  #                      variabletypecv = "Hydrology",
  #                      variablecode   = paste0("sal_",depth[n]),
  #                      variablenamecv = "salinity")
  # db_describe_variable(db,
  #                      variabletypecv = "Hydrology",
  #                      variablecode   = paste0("temp_",depth[n]),
  #                      variablenamecv = "temperatureSensor")
}

#Run function
lapply(seq(1, length(depth)), fun)


#Disconnect from database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RSQLite::dbDisconnect(db)

##################################################################################
#Step 3:  Create function to insert data into database----------------------------
##################################################################################
#List files
files<-list.files(paste0(working_dir, "Raw_data/raw_dir/"))

#Create function 
reader.fun<-function(n){
  
  #Read data into R memory~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Save file name for output
  file<-files[n]
  
  #For now, direct read
  df<-fread(paste0(working_dir, "Raw_data/raw_dir/", files[n]), skip=4)
  colnames(df)<-colnames(fread(paste0(working_dir, "Raw_data/raw_dir/", files[n]), skip=1))
  
  #Convert to tibble
  df<-as_tibble(df)
  
  #Define threshold date to not except rows from
  download_date<-as.POSIXct(paste0(substr(files[n], 12, 15), "-", 
                                   substr(files[n], 16, 17), "-", 
                                   substr(files[n], 18, 19)))
  threshold_date<-download_date-(30*86400)
  
  #Remove mislabled timestamps (will need to deal with this later...)
  df<-df %>%
    mutate(TIMESTAMP = as.POSIXct(df$TIMESTAMP)) %>%
    filter(TIMESTAMP > threshold_date) %>%   #Some values printed from year 2000?
    distinct(TIMESTAMP, .keep_all = TRUE)    #Some values taken 2x
  
  #Data wrangling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Find width for gather function
  width<-dim(df)[2]
  
  #Create long dataframe
  df<-df %>% 
    #create long fomrat data frame
    gather(.,"Loggernet_name","val", 6:width) %>%  
    #Select relevant collumns
    select(TIMESTAMP, STATNAME, Loggernet_name, val) %>%   
    #For now, lets filter for volumetric soil water content
    filter(str_detect(Loggernet_name, c("CS215"))) %>%
    #Deal with Loggernet_name inconsistancies [e.g., missing "_Avg" in some files starting fall 2018]
    mutate(Loggernet_name = if_else(str_detect(Loggernet_name, c("_Avg"))==F, 
                                    #If no "_Avg", splice into string
                                    paste0(substr(Loggernet_name, 
                                                  1,
                                                  str_locate(Loggernet_name,"\\(")[,1]-1),
                                           "_Avg",
                                           substr(Loggernet_name, 
                                                  str_locate(Loggernet_name,"\\(")[,1],
                                                  nchar(Loggernet_name))), 
                                    #If not, keep the same name
                                    Loggernet_name)) %>%
    #Convert "BDT_" to "BDT" in df if needed
    mutate(STATNAME = if_else(substr(STATNAME,1,4)=="BDT_",
                              paste0(substr(STATNAME,1,3), 
                                     substr(STATNAME,5,nchar(STATNAME))), 
                              STATNAME))
  
  #Add plot data to df
  key<-read_csv(paste0(working_dir,"Experimental Design Table/sensors.csv")) %>%
    #Filter for only VWC
    filter(Research_variable=="AirTemp"|Research_variable=="RawRH"|Research_variable=="CleanRH"|Research_variable=="VP"|Research_variable=="SVP"|Research_variable=="VPD") %>%
    #Select varibles of interest
    select(logger, plot, Loggernet_name, Research_variable) %>%
    #define variable address
    mutate(variable.address=substr(Loggernet_name,
                                   str_locate(Loggernet_name, "_")[,1]+1,
                                   str_locate(Loggernet_name, "_")[,1]+2))
  
  #join 
  df<-left_join(df, key, by=c("STATNAME" = "logger",
                              'Loggernet_name'='Loggernet_name'))
  
  #Add subplot data to df
  key<-read_csv(paste0(working_dir,"Experimental Design Table/plot.csv")) %>%
    #Select soil mositure sensors [b/c that's what we care about right now]
    filter(Sensor=='CS215') %>%
    #tidy variable address
    rename(variable.address='variable address') %>%
    #select variables of interest
    select(logger, plot, variable.address)
  #Correct key
  # key$variable.address[key$plot==4 & key$subplot=="a"]<-"d1"
  # key$variable.address[key$plot==4 & key$subplot=="b"]<-"d2"
  # key$variable.address[key$plot==16 & key$subplot=="a"]<-"g1"
  # key$variable.address[key$plot==16 & key$subplot=="b"]<-"g2"
  
  #Join
  df<-left_join(df, key, by=c('STATNAME'          = 'logger' ,
                              'plot'              = 'plot'   , 
                              'variable.address'  = 'variable.address'))
  
  #Organize for input into database
  df<-df %>% 
    #Create var collumn
    #mutate(var=paste0(Research_variable,"1")) %>%  
    #Select collumns of interest
    select(TIMESTAMP, plot, val, Research_variable) %>%
    #spread data for input
    #spread(., var, val) %>%
    #turn timestamp into posix
    mutate(Timestamp = as.POSIXct(TIMESTAMP))%>% 
           #site=paste0(plot,subplot)) %>%
    #select final collumns
    select(Timestamp, plot, Research_variable, val) %>%
    mutate(download= file)
  
  #Export
  df
}


#Run function (note: mclappply does not work on windows machines.  Use parlapply.)

t0<-Sys.time()
x<-lapply(seq(1,length(files)), reader.fun)
output<-bind_rows(x) %>% 
  arrange(Timestamp, plot)%>%
  filter(Research_variable=="AirTemp"|Research_variable=="CleanRH"|Research_variable=="VPD")
tf<-Sys.time()
tf-t0


#Write output
write.csv(output, paste0(working_dir, "output_aboveground.csv"))


#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Open db connection
# db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))
# 
# #
# vars_list<-list("Volumetric water content" = list("VWC_2", "Percent"))
# 
# #Insert data into database
# tsrv<-df %>%
#  filter(site=="1a") %>%
#  select(Timestamp, VWC_2)
# 
# db_insert_results_ts(db = db, # database connecton
#                     datavalues = tsrv, # data frame of time series data
#                     method = "BDT Data Download",
#                     site_code = "1a",
#                     variables = vars_list,
#                     sampledmedium = "Water",
#                     actionby = "owner_first")
# 
# dbDisconnect(db)