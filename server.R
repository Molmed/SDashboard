#-------------------------------------------------------
# Shiny server code to deploy SDashboard
#
# The purpose of this script is to serve up visulizations 
# of the ProjectMan database which tracks sequencing metrics
# at the SEQ&SEQ technology platform.
#
# Please note that the 'data.R' should defines functions to
# load data from the database and that global configurations
# such as database users, passwords, etc should be set in the
# 'config.R' file.
#
# Have fun adding plots!
#
# Author: Johan Dahlberg, 2014
#
# Requires that the shiny, ggplot2, reshape2 and RODBC
# libraries are installed.
#--------------------------------------------------------


library(shiny)
library(ggplot2)
library(reshape2)

source("config.R")
source("data.R")

# Suppress warnings. Make sure that this is reenabled when debugging.
# The production variable is loaded from the config.R file.
if(production)
  options(warn=-1)

shinyServer(function(input, output) {      
  
  # Plot the number of GB sequenced per week.
  output$weekplot <- renderPlot({    
    
    current.year <- as.numeric(format(Sys.time(), "%Y"))    
    x <- queryGigaBasesPerWeek(current.year)
    giga.bases.per.week.m <- melt(x , id.vars=c("Instrument", "Week"), measure.vars=c("GB"))
    
    p <- ggplot(data=giga.bases.per.week.m, aes(x = Week, y = value)) + 
      geom_bar(stat="identity", aes(fill=Instrument)) +
      ylab("Giga bases sequenced")
    
    print(p)
  })  
  
  # Plot the number of GB sequenced per month.
  output$monthplot <- renderPlot({
    
    current.year <- as.numeric(format(Sys.time(), "%Y"))    
    giga.bases.per.month.m <- melt(queryGigaBasesPerMonth(2012, current.year), id.vars=c("Instrument","Month","Year"), measure.vars=c("GB"))
    giga.bases.per.month.m$Year <- as.factor(giga.bases.per.month.m$Year)        
    aggregated.data <- aggregate(giga.bases.per.month.m$value, by=list(giga.bases.per.month.m$Year, giga.bases.per.month.m$Month), sum)
    colnames(aggregated.data) <- c("Year", "Month", "GB")
    
    p <- ggplot(data = aggregated.data, aes(x = Month, y = GB, fill = Year)) + 
      geom_bar(position="dodge", stat="identity") +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), limits = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      ylab("Giga bases sequenced")
    
    print(p)
  })
  
  # Plot the cumulative coverage per month and year.
  output$cumulativeplot <- renderPlot({    
    
    current.year <- as.numeric(format(Sys.time(), "%Y"))    
    giga.bases.per.month.m <- melt(queryGigaBasesPerMonth(2012, current.year), id.vars=c("Instrument","Month","Year"), measure.vars=c("GB"))
    giga.bases.per.month.m$Year <- as.factor(giga.bases.per.month.m$Year)        
    aggregated.data <- aggregate(giga.bases.per.month.m$value, by=list(giga.bases.per.month.m$Year, giga.bases.per.month.m$Month), sum)
    colnames(aggregated.data) <- c("Year", "Month", "GB")
    
    split.by.year <- split(aggregated.data, aggregated.data$Year)
    
    cumulative.results <-
      do.call(rbind,
              lapply(split.by.year, function(x) {                    
                x$cumulativeGB <- cumsum(x$GB)
                x
              }))
    
    cumulative.results
    
    p <- ggplot(data = cumulative.results, aes(x = Month, y = cumulativeGB)) + 
      geom_line(size = 2, aes(colour = Year)) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      ylab("Giga bases sequenced")
    
    print(p)
  })  
  
  cumulativeGBPerInstrumentData <- function() {  
    # A helper function to return the cumulative data per instrument
    #
    # Args:
    #
    # Returns: The cumulative data in the following format: 
    #                   Year Month Instrument         GB cumulativeGB
    #   2012.HiSeq 1.1  2012     1    HiSeq 1 1190.16157     1190.162
    #   2012.HiSeq 1.4  2012     2    HiSeq 1  369.35514     1559.517
    #
    
    current.year <- as.numeric(format(Sys.time(), "%Y"))    
    giga.bases.per.month.m <- melt(queryGigaBasesPerMonth(2012, current.year), id.vars=c("Instrument","Month","Year"), measure.vars=c("GB"))
    giga.bases.per.month.m$Year <- as.factor(giga.bases.per.month.m$Year)        
    aggregated.data <- aggregate(giga.bases.per.month.m$value, by=list(giga.bases.per.month.m$Year,
                                                                       giga.bases.per.month.m$Month,
                                                                       giga.bases.per.month.m$Instrument),
                                 sum)
    colnames(aggregated.data) <- c("Year", "Month", "Instrument", "GB")
    
    split.by.year.and.instrument <- split(aggregated.data, list(aggregated.data$Year,aggregated.data$Instrument))
    
    cumulative.results <-
      do.call(rbind,
              lapply(split.by.year.and.instrument, function(x) {                    
                x$cumulativeGB <- cumsum(x$GB)
                x
              }))    
    
    cumulative.results
  }
  
  # Plot the cumulativ data for the HiSeqs
  output$cumulativeplotperinstrument <- renderPlot({    
    
    cumulative.results <- cumulativeGBPerInstrumentData() 
    # Exclude the MiSeqs
    cumulative.results <- cumulative.results[!grepl("*MiSeq*", cumulative.results$Instrument),]
    
    p <- ggplot(data = cumulative.results, aes(x = Month, y = cumulativeGB)) +         
      geom_line(size = 2, aes(colour = Year)) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      ylab("Giga bases sequenced") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(. ~ Instrument)
    
    print(p)
  })  
  
  # Plot the cumulativ data for the MiSeqs
  output$cumulativeplotperinstrumentforMiSeq <- renderPlot({    
    
    cumulative.results <- cumulativeGBPerInstrumentData() 
    # Only include the MiSeqs
    cumulative.results <- cumulative.results[grepl("*MiSeq*", cumulative.results$Instrument),]
    
    p <- ggplot(data = cumulative.results, aes(x = Month, y = cumulativeGB)) +         
      geom_line(size = 2, aes(colour = Year)) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      ylab("Giga bases sequenced") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(. ~ Instrument)
    
    print(p)
  })  
  
  # Plot the error rate per instrument
  output$errorrateplot <- renderPlot({    
    
    current.date <- format(Sys.time(), "%Y-%m-%d")
    one.year.ago <- paste(as.numeric(format(Sys.time(), "%Y")) -1 ,format(Sys.time(), "%m-%d"), sep="-")    
    
    quality.metrics <- queryQualityValues(one.year.ago, current.date)[,c(1:4,7)]        
    
    quality.metrics$Year <- as.factor(quality.metrics$Year)
    quality.metrics$Month <- as.factor(quality.metrics$Month)
    quality.metrics.m <- melt(quality.metrics, id.vars=c("Instrument", "Year", "Month"), measure.vars=c("ErrorRate"))
    
    p <- ggplot(data = quality.metrics.m, aes(x = Month, y = value, colour = Year)) +
      geom_boxplot() +
      facet_grid(. ~ Instrument) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Error Rate")
    
    print(p)
  })  
  
  # Plot the percent Q30 per instrument
  output$percentq30plot <- renderPlot({    
    
    current.date <- format(Sys.time(), "%Y-%m-%d")
    one.year.ago <- paste(as.numeric(format(Sys.time(), "%Y")) -1 ,format(Sys.time(), "%m-%d"), sep="-")    
    
    quality.metrics <- queryQualityValues(one.year.ago, current.date)[,c(1:3,5,7)]
    quality.metrics$Year <- as.factor(quality.metrics$Year)
    quality.metrics$Month <- as.factor(quality.metrics$Month)
    quality.metrics.m <- melt(quality.metrics, id.vars=c("Instrument", "Year", "Month"), measure.vars=c("PercentQ30"))
    
    p <- ggplot(data = quality.metrics.m, aes(x = Month, y = value, colour = Year)) +
      geom_boxplot() +
      facet_grid(. ~ Instrument) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Percent Q30")
    
    print(p)
  })  
  
  # Plot the mean quality per instrument
  output$meanqualityplot <- renderPlot({    
    
    current.date <- format(Sys.time(), "%Y-%m-%d")
    one.year.ago <- paste(as.numeric(format(Sys.time(), "%Y")) -1 ,format(Sys.time(), "%m-%d"), sep="-")    
    
    quality.metrics <- queryQualityValues(one.year.ago, current.date)[,c(1:3,6,7)]
    quality.metrics$Year <- as.factor(quality.metrics$Year)
    quality.metrics$Month <- as.factor(quality.metrics$Month)
    quality.metrics.m <- melt(quality.metrics, id.vars=c("Instrument", "Year", "Month"), measure.vars=c("MeanQuality"))
    
    p <- ggplot(data = quality.metrics.m, aes(x = Month, y = value, colour = Year)) +
      geom_boxplot() +
      facet_grid(. ~ Instrument) +
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Mean Quality")
    
    print(p)
  })  
  
  # Plot the cumulative numver of samples per month
  output$cumulativesamples <- renderPlot({    
    
    samples <- querySamples()    
    
    # Select only the first read, otherwise all numbers will be doubled for paired end runs. 
    relevant.sample.info <- samples[samples$read_num == 1, c("run_date", "project_id","sample_name")]
    relevant.sample.info$counts <- 1
    
    aggregated.by.year.and.month <- aggregate(relevant.sample.info$counts, by=list(format(relevant.sample.info$run_date, "%Y-%m")), sum)
    aggregated.by.year <- aggregate(relevant.sample.info$counts, by=list(format(relevant.sample.info$run_date, "%Y")), sum)
    
    p <- ggplot(data = aggregated.by.year.and.month, aes(x = Group.1, y = x, fill = "#8dc63f")) +
      geom_bar(stat = "identity") +            
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Samples") +
      xlab("Year and month") +
      scale_fill_identity()
    
    print(p)
  })  
  
  # Plot the cumulative number of samples totaled per year.
  output$cumulativesamplesyeartotal <- renderPlot({          
    
    samples <- querySamples()    
    
    # Select only the first read, otherwise all numbers will be doubled for paired end runs. 
    relevant.sample.info <- samples[samples$read_num == 1, c("run_date", "project_id","sample_name")]
    
    split.per.project <- split(relevant.sample.info, list(relevant.sample.info$project_id))
    
    # Make the samples unique per project and year sequenced.
    unique.samples.per.year <-
      do.call(rbind,
              lapply(split.per.project, function(x) {
                x$year <- format(x$run_date, "%Y")
                unique(x[, c("year","sample_name")])
              }))
    
    unique.samples.per.year$counts <- 1
    
    aggregated.by.year <- aggregate(unique.samples.per.year$counts, by=list(unique.samples.per.year$year), sum)
    
    p <- ggplot(data = aggregated.by.year, aes(x = Group.1, y = x, fill = "#8dc63f")) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=x, y= x - 200)) +
      ylab("Samples") +
      xlab("Year") +
      scale_fill_identity()
    
    print(p)
  })  
  
  # Plot the cumulative number of projects per month
  output$cumulativeprojects <- renderPlot({    
    
    samples <- querySamples()    
    project.info <- samples[, c("run_date","project_id")]
    project.info$year <- format(project.info$run_date, "%Y")
    project.info <- unique(project.info[,c("year", "project_id")])
    project.info$counts <- 1            
    
    aggregated.by.year <- aggregate(project.info$counts, by=list(project.info$year), sum)
    
    p <- ggplot(data = aggregated.by.year, aes(x = Group.1, y = x, fill = "#8dc63f")) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=x, y = x - 5)) +
      ylab("Projects") +
      xlab("Year") +
      scale_fill_identity()
    
    print(p)
  })  
  
})
