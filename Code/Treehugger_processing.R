##Howdy - test
##Jens Stevens
##stevensjt@gmail.com
##This script processes and visualizes automatic dendroband (Treehugger) data.
##Working on setting up an API import with Bianca.

####0. Load libraries and functions####
library(readxl)
library(vctrs)
library(tidyverse)
library(lubridate)
library(plotly)


####1. Import Automatic Dendroband data ####

file_list <- dir("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Data")
csv_file_list <- file_list[grep("csv",file_list)]
xlsx_file_list <- file_list[grep("tree list.xlsx",file_list)]
tree_list <- read_xlsx("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Data/tree list.xlsx", sheet = "Metadata")
visits <- read_xlsx("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Data/tree list.xlsx", sheet = "VisitDates",
                    col_types = "date")

#### Bianx edits ####

files_all <- list.files(path = "C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Data",     # Identify all csv files in folder
                        pattern = "*TH", full.names = TRUE) 

file <- paste0(getwd(), "/","Data","/" ,"TH-19035.csv")

### combining multiple file names from stackoverflow: 
data_all <- purrr::map_df(files_all, function(x) {
  
  data <-read_csv(x,col_types = list(col_character(),col_character(), 
                                     col_double(),col_double(),col_double(),col_double(),
                                     col_double(),col_double(),col_double(),col_double()))
  cbind(file_id = x, data)
  
})

# junk data at the end -- drop 
data_all<- data_all[1:11]

## fix date and time types 
data_all$Date <- as.Date(data_all$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))

data_all$Time <-parse_date_time(data_all$Time, "HM")

# simplify file id 
data_all$file_id<- cbind(file_id= substr(data_all$file_id, start= nchar(files_all[1])-11, stop = nchar(files_all)[1]-4))

# add a tree id to data all
data_all$tree <-tree_list$tree[pmatch(data_all$file_id, tree_list$csv_name, duplicates.ok =T)]

# add a site to data all 
data_all$site <-tree_list$Site[pmatch(data_all$file_id, tree_list$csv_name, duplicates.ok =T)]

# Kay wants to know: whats' the raw change in mm?
data_all$mm2<-lead(data_all$mm)
data_all <- data_all %>% mutate(raw_chg_mm = data_all$mm - data_all$mm2)

# select only Daily measurements @5am - second arg in grep is string to look in  -
# so select anything that has 05 in that string and return the col 
d_daily_all <- data_all[grep("05:",substr(data_all$Time,12,14)),]

# char dates to plot later 
d_daily_all$Time<- paste(hour(d_daily_all$Time),minute(d_daily_all$Time), sep=":")

# get rid of NAs that may be messing up the data -- usually csv files lack headers 
d_daily_all<- d_daily_all[complete.cases(d_daily_all), ]


#clear the dev before running script 
for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
  dev.off()
}

# individual tree plots
plot_list = list()

## Bianx generating multiple plots with standard x/y scales 
for (i in seq_along((unique(d_daily_all$tree)))) {
  
  # to get visit date
  # create df with only the tree type (13)
  temp = d_daily_all[grep(unique(d_daily_all$tree)[i], d_daily_all$tree),]
  
  # pass unique df to put in plot list 
  p = ggplot(temp, aes(x=ymd(Date), y=raw_chg_mm, group = 1)) +
    geom_line() + theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    geom_smooth()+ 
    # add plot visit dates -- breaks the code!! 
    # geom_vline(data = visits,
    #            aes(xintercept = as.Date(visits[which(unique(temp$tree)==names(visits))][[1]]),
    #                col = "Visit Dates"))+
    labs(y= "Change in mm", x = "Dates")+
    labs(title = unique(temp$tree))+
    ylim(-.015, .015)+
    xlim(ymd(min(temp$Date)), ymd(max(temp$Date)))
  
  # save plots 
  plot_list[[i]] = p
  #print(plot_list[i])
  
  # save plots to tiff using tiff function and filepath name
   file_name = paste0(getwd(),"/Figures/test/", unique(temp$tree), ".tiff", sep="")
   tiff(file_name)
   print(plot_list[[i]])
   print(i)
  # dev.off
}

olot# individual tree plots
# one tree per plot for all lower sites 
# facet is individual trees. 

bs_plots = list()

for (i in seq_along((unique(d_daily_all$site)))) {

  # create df with only site type
  temp = d_daily_all[grep(unique(d_daily_all$site)[i], d_daily_all$site),]
  
  # create ggplot
  b = ggplot(temp, aes(x=mdy(Date), y=raw_chg_mm, group = 1)) +
    geom_line() + theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    geom_smooth()+ 
    # add plot visit dates 
    # geom_vline(data = visits,
    #            aes(xintercept = as.Date(visits[which(unique(temp$tree)==names(visits))][[1]]),
    #                col = "Visit Dates"))+
    labs(y= "Change in mm", x = "Dates")+
    labs(title = unique(temp$tree))+
    ylim(-.015, .015)+
    xlim(mdy(min(temp$Date)), mdy(max(temp$Date)))+ 
    facet_wrap(~tree)
  # pass unique df to put in plot list 
 
  # save plots 
  bs_plots[[i]] = b
  
  # save plots to tiff
  file_name = paste0(getwd(),"/Figures/bianx/", unique(temp$site),"site", ".tiff", sep="")
  tiff(file_name)
  print(bs_plots[[i]])
  dev.off()
}


# Kay wants a plot of all trees with an average line of growth 
avg_all_plt <- ggplot(d_daily_all, aes(x=mdy(Date), y=raw_chg_mm, group = 1, col=tree)) +
  geom_line() + theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  # add plot visit dates 
  # geom_vline(data = visits,
  #            aes(xintercept = as.Date(visits[which(unique(d_daily_all$tree)==names(visits))][[1]]),
  #                col = "Visit Dates"))+
  labs(y= "Change in mm", x = "Dates")+
  stat_summary(fun.y=mean, aes(group=1), geom="line", colour="blue", size=1, shape=4)+ 
  facet_wrap(~site)+
  labs(title = "Change in Growth of Tree Hugger trees in Bandelier National Monument")+
  ylim(-.015,.015)+
  xlim(mdy(c("12/12/2019", "03/25/2020")))


file_name = paste0(getwd(),"/Figures/bianx/", "tree", unique(temp$tree), ".tiff", sep="")

ggplotly(avg_all_plt)



#### Jen's processing ####
for(n in tree_list$csv_name){
  if(any(grep(n,file_list))){ # if dendro band name (n) is in file list
    
    d <- read_csv(paste0("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Data/",
                         file_list[grep(n,file_list)]), #combine current wd with the file list (ex: th-10693)
                  # initalize columns in a list with date/time and 8 double cols
                  col_types = list(col_date(format = c("%m/%d/%Y")),col_time(format = "%H:%M"), 
                                   col_double(),col_double(),col_double(),col_double(),
                                   col_double(),col_double(),col_double(),col_double()))
  
    #initalize objects w/ start date info + DBH + visit dates
    start_date <- tree_list[tree_list$csv_name==n,"datalogger_online"]
    start_dbh <- tree_list[tree_list$csv_name==n,"dbh"][[1]] #[[1]] removes tibble formatting - to numeric
    visit_dates <- na.exclude(visits[,n]) #select col where n = treehugger band
    names(visit_dates) <- "Visits"
    #head(d$Date,10)
   

    d$Date[30]
    start_date[[1]]
    #d$Date <- as.POSIXlt.Date(d$Date, format = c("%Y-%m-%d"))
    
    d <- d[which(d$Date>=start_date[[1]]),]  #which indices are TRUE (rows that have start date after 10/30)
    start_mm <- d$mm[1] 
    d$delta_c <- (d$mm- start_mm ) #Change in circumference
    d$dbh <- ((start_dbh * 10 * pi) + #initial circumference plus
      d$delta_c) / pi/10 #change in circumference = new circumference, divided by pi, convert to cm
    
    #START HERE
    d_daily <- d[grep("05:",substr(d$Time,1,3)),]

    print(paste0("printing ",n))
    pdf(paste0("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Figures/",n,"_daily.pdf"))
    print(
      ggplot()+
        geom_line(data = d_daily, aes(x = Date, y =dbh))+
        ggtitle(paste (tree_list[tree_list$csv_name==n,"tree"], "daily"))+
        geom_vline(data = visit_dates, aes(xintercept = as.Date(Visits)),
                   col = "darkgreen")
    )
    dev.off()

    pdf(paste0("C:/Users/bgonzalez/DOI/NM Landscapes Field Station - Treehugger_BAND/Figures/",n,".pdf"))
    print(
      ggplot(d)+
        geom_line(aes(x = Date, y =dbh))+
        ggtitle(tree_list[tree_list$csv_name==n,"tree"])
    )
    #ggplot(d)+
    #  geom_line(aes(x = Date, y =dbh))+
    #  scale_x_date(
    #               labels=date_format("%Y-%m-%d"),
    #               limits = as.Date(c('2020-01-01','2020-01-07')))
    #dev.copy2pdf(filename = paste0("Figures/Treehugger/",n,".pdf"))
    
    dev.off() #close file
  } else(print(paste(n," no-go")))
  
}

