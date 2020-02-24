##Jens Stevens
##stevensjt@gmail.com
##This script processes and visualizes automatic dendroband (Treehugger) data.
##Working on setting up an API import with Bianca.

####0. Load libraries and functions####
library(readxl)

####1. Import Automatic Dendroband data####
file_list <- dir("./Data")
csv_file_list <- file_list[grep("CSV",file_list)]
xlsx_file_list <- file_list[grep("tree list.xlsx",file_list)]
tree_list <- read_xlsx("./Data/tree list.xlsx", sheet = "Sheet1")

for(n in tree_list$csv_name){
  if(any(grep(n,file_list))){
    
    d <- read_csv(paste0("../Dendrobands/Treehugger data files/",
                         file_list[grep(n,file_list)]),
                  col_types = list(col_date(format = c("%Y-%m-%d")),col_time(format = "%H:%M"),
                                   col_double(),col_double(),col_double(),col_double(),
                                   col_double(),col_double(),col_double(),col_double()))
    start_date <- tree_list[tree_list$csv_name==n,"online"]
    start_dbh <- tree_list[tree_list$csv_name==n,"dbh"][[1]] #[[1]] removes tibble formatting
    #head(d$Date,10)
    
    d$Date[30]
    start_date[[1]]
    #d$Date <- as.POSIXlt.Date(d$Date, format = c("%Y-%m-%d"))
    d <- d[which(d$Date>=start_date[[1]]),]
    start_mm <- d$mm[1]
    d$dbh <- (d$mm- start_mm )+start_dbh
    #START HEREd_daily <- d[grep("12:",d$Time),]
    
    print(paste0("printing ",n))
    pdf(paste0("Figures/Treehugger/",n,".pdf"))
    print(
      ggplot(d)+
        geom_line(aes(x = Date, y =dbh))+
        ggtitle(n)
    )
    #ggplot(d)+
    #  geom_line(aes(x = Date, y =dbh))+
    #  scale_x_date(
    #               labels=date_format("%Y-%m-%d"),
    #               limits = as.Date(c('2020-01-01','2020-01-07')))
    #dev.copy2pdf(filename = paste0("Figures/Treehugger/",n,".pdf"))
    
    dev.off()
  } else(print(paste(n," no-go")))
  
}
