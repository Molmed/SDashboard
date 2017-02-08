#-------------------------------------------------------
# "data.R" contains functions encapsulating the sql
# queries used to extract data from our data base.
# It loads its database configuration from "config.R".
#
# Please note that appropriate odbc drivers need to be
# installed and configured.
#
# Check the bottom of this script to see some potenitally
# interesting sql-queries to explore the database.
#
# Author: Johan Dahlberg, 2014
# RODBC need to be installed.
#
# TODO Release data base handle on exit
# TODO The database contains wired NAs. This needs to
#      be investigated.
#--------------------------------------------------------

library(RODBC)

# Load database settings from config.R
source("config.R")

connectionString <- paste('driver=', driver, ';', 
                          'server=', server, ';',
                          'database=', database, ';',
                          'uid=', uid, ';',
                          'pwd=', password, ';',
                          sep="")

dbhandle <- odbcDriverConnect(connectionString) 

filterOutNAs <- function(df) {
  # Removes rows with NAs
  #
  # Args:
  #   df: A data frame to remote rows contaning NAs from.  
  #
  # Returns:
  #   The same data frame without rows containing NAs.
  na.omit(df)
}

parseOutInstrumentName <- function(df) {    
  # Parses the instrument name from the runfolder name and
  # translates it to the "common names" of the instruments,
  # e.g. SN334 = HiSeq1
  #
  # Args:
  #   df: A data frame containing a column called "runfolder_name"
  #       on the standard Illumina format: "140314_D00458_0004_BH8CPTADXX",
  #       where the second element is the instrument identifer.
  #
  # Returns:
  #   The same data frame with a "Instrument" column with the instrument
  #   common names.
  
  instrument.translation.table <-
    data.frame(UniqueIdentifier=c("SN344", "SN866", "SN7001335", "D00118", "D00457", "D00458", "ST-E00215", "ST-E00216", "ST-E00274", "ST-E00279", "ST-E00280","M00485", "M00629", "M03379"),
               Instrument=c("HiSeq 1","HiSeq 2", "HiSeq 3", "HiSeq 4", "HiSeq 5", "HiSeq 6", "HiSeqX 1", "HiSeqX 2", "HiSeqX 3", "HiSeqX 4", "HiSeqX 5","MiSeq 1", "MiSeq 2", "MiSeq IMBIM"))
  
  ExtractSecondElement <- function(x) lapply(x, function(x) x[2])
  
  tmp.list <- strsplit(as.character(df$runfolder_name),"_")
  
  tmp.uniq.identifiers <- data.frame(UniqueIdentifier = do.call(rbind, ExtractSecondElement(tmp.list)))
  
  instrument <- Map(x=tmp.uniq.identifiers$UniqueIdentifier, function(x) {
    instrument.translation.table[which(instrument.translation.table$UniqueIdentifier %in% x),]$Instrument
  })
  
  df$Instrument <- unlist(instrument)
  
  df
}

queryGigaBasesPerMonth <- function(from.year, to.year) {
  # Gets the number of Gigabases sequenced per year.
  #
  # Args:
  #   from.year: the year to start collecting data from.
  #   to.year: the final year to get data from.
  #
  # Returns:
  #   A data frame on the following form:
  #   runfolder_name Month Year         GB Instrument
  #   120104_SN344_0158_AC06JEACXX     1 2012 278.475552    HiSeq 1
  #   120117_SN344_0161_AC0FRKACXX     1 2012  67.734264    HiSeq 1
  #   120119_SN866_0129_BC0935ACXX     1 2012 261.902582    HiSeq 2
  
  # Get total number of giga bases per month
  query.giga.bases.per.month.this.year <- paste(
    "select fr.runfolder_name as runfolder_name, 
    datepart(month, fr.run_date) as Month, datepart(year, fr.run_date) as Year, sum(flr.cycles*flr.pf_clusters/1e9) as GB
    from flowcell_lane_results flr
    left join flowcell_runfolder fr on(fr.flowcell_id=flr.flowcell_id)
    where fr.run_date >= '",from.year, "-1-1'
    and fr.run_date <= '",to.year, "-12-31'
    group by datepart(month, fr.run_date), datepart(year, fr.run_date), runfolder_name
    order by Month asc")
  
  data <- sqlQuery(dbhandle,query.giga.bases.per.month.this.year)
  parseOutInstrumentName(data)  
}

# TODO This contains weird NAs
queryGigaBasesPerWeek <- function(year){  
  # Gets the number of Gigabases sequenced per week.
  #
  # Args:
  #   year: the year to get per week data from
  #
  # Returns:
  #   A data frame on the following form:
  #   runfolder_name Week         GB Instrument
  #   130104_SN866_0197_AC1DLVACXX    1 347.690750    HiSeq 2
  #   130104_SN866_0198_BC1DAYACXX    1 347.744153    HiSeq 2
  #   130115_SN344_0231_BD0W8AACXX    3  75.160048    HiSeq 1
  #   130115_SN344_0232_AD0W5DACXX    3  80.029109    HiSeq 1
  
  query.giga.bases.per.week <- paste(
    "select fr.runfolder_name as runfolder_name, 
    datepart(week, fr.run_date) as Week, sum(flr.cycles*flr.pf_clusters/1e9) as GB
    from flowcell_lane_results flr
    left join flowcell_runfolder fr on(fr.flowcell_id=flr.flowcell_id)
    and fr.run_date >= '", year ,"-1-1'
    and fr.run_date <= '", year, "-12-31'
    group by datepart(week, fr.run_date), fr.runfolder_name
    ")
  
  data <- sqlQuery(dbhandle,query.giga.bases.per.week)   
  parseOutInstrumentName(filterOutNAs(data))
}


queryQualityValues <- function(from.date, to.date) {
  # Gets the quality metrics from the runs.
  #
  # Args:
  #   from.date: the earliest date to get data from.
  #   to.date: the latest date to get data from.
  #
  # Returns:
  #   A data frame on the following form:
  #   runfolder_name Month Year ErrorRate PercentQ30 MeanQuality Instrument
  #   130104_SN866_0197_AC1DLVACXX     1 2013      0.31  0.8794008    34.53935    HiSeq 2
  #   130104_SN866_0197_AC1DLVACXX     1 2013      0.31  0.8926922    35.02377    HiSeq 2
  #   130104_SN866_0197_AC1DLVACXX     1 2013      0.32  0.8834778    34.76706    HiSeq 2
  
  query.quality.values <- paste(
    "select fr.runfolder_name as runfolder_name, 
    datepart(month, fr.run_date) as Month, datepart(year, fr.run_date) as Year,
    error_rate as ErrorRate, pct_q30 as PercentQ30, mean_q as MeanQuality
    from flowcell_lane_results flr
    left join flowcell_runfolder fr on(fr.flowcell_id=flr.flowcell_id)
    where fr.run_date >= '",from.date, "'
    and fr.run_date <= '",to.date, "'
    group by fr.runfolder_name, datepart(month, fr.run_date), datepart(year, fr.run_date), error_rate, pct_q30, mean_q
    order by Month asc", seq="")
  
  data <- sqlQuery(dbhandle,query.quality.values)
  parseOutInstrumentName(data)
}


querySamples <- function() {  
  # Gets information on the samples from the database.
  #
  # Args:
  #
  # Returns:
  #   A data frame on the following form:
  #   flowcell_id                      runfolder_name   run_date   flowcell_id.1 project_id sample_name tag_seq lane_num read_num cycles  pct_lane pf_clusters
  #   000000000-A0KD9 120426_M00485_0002_AMS0010415-00300 2012-04-26 000000000-A0KD9    LD-0049       15890  CTCGGT        1        1    150 15.806417      748173
  #   000000000-A0KD9 120426_M00485_0002_AMS0010415-00300 2012-04-26 000000000-A0KD9    LD-0049       19540  TTACGG        1        1    150 13.646423      645933  
  #   pct_q30 pct_tag_err library_name   mean_q
  #   85.32888    3.932513              33.37124
  #   84.84964    2.017237              33.24309
  
  samples <- 
    "select *
    from flowcell_runfolder fr inner join sample_results s
      on(s.flowcell_id=fr.flowcell_id)"    
  
  sqlQuery(dbhandle,samples)
}

#---------------------------------------------
# Potentially useful sql queries for debugging
#---------------------------------------------

#sample.results <- sqlQuery(dbhandle,
#         "select * from sample_results")

# See all available tables
#sqlQuery(dbhandle,
#         "
#          select * from sys.Tables
#          ")

# sqlQuery(dbhandle,
#          "
#         select substring(fr.runfolder_name, 8, 5) as Instrument, *
#         from flowcell_lane_results flr
#         left join flowcell_runfolder fr on(fr.flowcell_id=flr.flowcell_id)
#         ")
