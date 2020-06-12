#clear environment
rm(list=ls())

library(dplyr)
library(tidyr)
library(readr)
library(XML)
library(rjson)
library(httr)
library(stringr)
library(rvest)
library(RCurl)

####################################################################################################################

###################################### ALLEGHENY COUNTY JAIL DATA ###############################################

####################################################################################################################
jail_url<-"https://data.wprdc.org/dataset/allegheny-county-jail-daily-census"


#fetch wprdc page for jail datasets
page<-read_html(getURL(jail_url,
                       .opts = list(ssl.verifypeer = FALSE)))

#use rvest to parse web-page and retrieve all download links
all_links<-page%>%
  #pull resource list
  html_node(xpath='//*[@id="dataset-resources"]/ul')%>%
  html_nodes("li")%>%
  html_nodes("div")%>%
  html_nodes("a.btn.btn-primary")%>%
  #pull rows in list
  xml_attr("href")

#limit just to monthly census links (starting at 5)
monthly_links<-all_links[5:length(all_links)]

#create a list of dataframes containing jail data
jail_data_list<-
  #read in the csvs for all of the links
  lapply(monthly_links,function(x)read_csv(getURL(x,
                                                  .opts = list(ssl.verifypeer = FALSE))))

# the "fix_date" function does the following 2 things:
  # 1.) the date formats are inconsistent, so detects if the date was read incorrectly, and respecify
  # 2.) since some csvs contain dates outside given month, leading to duplicates, limit all individual csvs to
  #     only one month of data (all data should be same month as first row)

fix_date<-function(x){
  # part 1: fix date formats that don't work
  if(class(x$Date)=="character"){
    x$Date<-as.Date(x$Date,format("%m/%d/%Y"))
  }
  # part 2: limit rows only to same month as the date of the first row
  # store the month and year of the first row's date
  first_row_month<-format(x[1,]$Date,"%m")%>%as.character()%>%as.numeric()
  first_row_year<-format(x[1,]$Date,"%Y")%>%as.character()%>%as.numeric()
  #define the date of the first day of the next month
  next_month_start<-as.Date(paste(first_row_month+1,"1",first_row_year,sep="-"),format("%m-%d-%Y"))
  
  #limit x to only rows with dates before next_month_start
  x2<-x%>%
    filter(Date<next_month_start)
  return(x2)
  
}

#apply fix date to jail data list
jail_data_list2<-lapply(jail_data_list,fix_date)
#combine rows into one dataframe
jail_data<-bind_rows(jail_data_list2)

####################################################################################################################

###################################### ALLEGHENY COUNTY ARREST DATA ###############################################

####################################################################################################################

#create a function that takes data frame, a column name, and vector of strings as an argument and returns a df with a new column
# flagging if any of those terms are present in the given row
flag_columns<-function(df,col_name,filter_strings,new_col_name){
  #add a counter column to df
  df$counter<-0
  #iterate through filter strings, if there's match, add 1 to counter
  for(s in filter_strings){
    #for rows where the strings in column col_name contain s, add 1 to counter
    df[grep(s,df[,col_name]%>%unlist(),ignore.case=TRUE),]$counter<-df[grep(s,df[,col_name]%>%unlist(),ignore.case=TRUE),]$counter+1
  }
  # create a new column equal to true if counter is greater than zero, otherwise equal to false
  df$flag<-df$counter>0
  #delete counter
  df$counter<-NULL
  #rename the flag column to "new_col_name"
  names(df)[length(df)]<-new_col_name
  return(df)
}

#create a list of strings that indicate a violent offense
violent_flags<-c("assault",
                 "rape",
                 "homicide",
                 "murder",
                 "endangering",
                 "idsi",
                 "involuntary deviate sexual intercourse",
                 "sexual exploitation",
                 "kidnapping",
                 "terroris",
                 "dui",
                 "arson",
                 "strangulation",
                 "ethnic intimidation",
                 "sex ")

#read in raw arrest data
arrest_data<-read_csv(getURL("https://data.wprdc.org/datastore/dump/e03a89dd-134a-4ee8-a2bd-62c40aeebc6f",
                             .opts = list(ssl.verifypeer = FALSE)
                             ))%>%
  #add a flag for whether or not the crime was violent based on violent flags
  flag_columns(col_name="OFFENSES",filter_strings = violent_flags, new_col_name="VIOLENT")

########### Since each arrest can have multiple violations, need to identify all of the unique violations

save(arrest_data,jail_data,file="arrest and jail data.Rdata")


################################################## Exploring offenses below here #############################

#create versions of regex functions that allow piping
pipe_gsub<-function(x,expression,replacement=""){
  gsub(expression,replacement,x)
}

pipe_grepl_remove<-function(x,expression){
  x[!grepl(expression,x)]
}

pipe_grepl_keep<-function(x,expression){
  x[grepl(expression,x)]
}


offenses<-unique(arrest_data$OFFENSES)%>%
  #remove periods
  pipe_gsub("\\.")%>%
  #regex expression to detect " / " followed by numbers (with possible parentheticals) to split to multple offenses
  strsplit(" / [0-9]+(\\([A-z]\\))?(\\([0-9]*\\))?\\)?")%>%
  unlist()%>%
  #remove leading spaces
  pipe_gsub("^ ")%>%
  #remove leading codes
  pipe_gsub("[0-9]+(\\([A-z]\\))?(\\([0-9]*\\))?(\\([A-z]\\))*")%>%
  #remove leading spaces again
  pipe_gsub("^ +")%>%
  unique()

non_violent<-offenses%>%
  tolower()%>%
  pipe_grepl_remove("assault")%>%
  pipe_grepl_remove("rape")%>%
  pipe_grepl_remove("homicide")%>%
  pipe_grepl_remove("murder")%>%
  pipe_grepl_remove("endangering")%>%
  pipe_grepl_remove("idsi")%>%
  pipe_grepl_remove("involuntary deviate sexual intercourse")%>%
  pipe_grepl_remove("sexual exploitation")%>%
  pipe_grepl_remove("kidnapping")%>%
  pipe_grepl_remove("terroris")%>%
  pipe_grepl_remove("dui")%>%
  pipe_grepl_remove("arson")%>%
  pipe_grepl_remove("strangulation")%>%
  pipe_grepl_remove("ethnic intimidation")%>%
  pipe_grepl_remove("sex ")

violent<-offenses%>%
  tolower()%>%
  pipe_grepl_select("assault")%>%
  pipe_grepl_select("rape")%>%
  pipe_grepl_select("homicide")%>%
  pipe_grepl_select("murder")%>%
  pipe_grepl_select("endangering")%>%
  pipe_grepl_select("idsi")%>%
  pipe_grepl_select("involuntary deviate sexual intercourse")%>%
  pipe_grepl_select("sexual exploitation")%>%
  pipe_grepl_select("kidnapping")%>%
  pipe_grepl_select("terroris")%>%
  pipe_grepl_select("dui")%>%
  pipe_grepl_select("arson")%>%
  pipe_grepl_select("strangulation")%>%
  pipe_grepl_select("ethnic intimidation")%>%
  pipe_grepl_select("sex ")
