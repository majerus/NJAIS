
library(readxl)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(scales)
library(pander)
library(rvest)

# update this file path to point toward appropriate folder on your computer
nj <- "/Users/majerus/Desktop/NJAIS/data/nj/"      
us <- "/Users/majerus/Desktop/NJAIS/data/us/"      
state <- "/Users/majerus/Desktop/NJAIS/data/state/"
results <- "/Users/majerus/Desktop/NJAIS/results/"    
labor <- "/Users/majerus/Desktop/NJAIS/data/dept_labor/age_lvl/"

# NJ county level data ------------------------------------------

# cleaner
cleaner <- function(df){
  # drop rows with all missing values 
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # keep only relevant rows
  df <- df[1:22,]
  df <- df[-c(1:4), ]
  
  df$Subject <- str_trim(as.character(df$Subject))
  names <- df$Subject
  
  # keep only relevant columns 
  df <- df[, seq(2, ncol(df), by = 2)] # keep total, male, female data 
  df <- df[, seq(1, ncol(df), by = 3)] # keep total only
  
  counties <- colnames(df)
  counties <- subset(counties, counties!='')
  counties <- str_replace(counties, ' County, New Jersey', '') 
  # counties <- rep(counties, 3) for total, male, female data 
  
  counties <- counties[order(counties)]
  
  colnames(df) <- counties
  
  df <- as.data.frame(t(df))
  colnames(df) <- names

  df$county <- rownames(df)
  
  df <- melt(df, id.vars=c("county"), variable.name="category")
  df <- df[order(df$county),]   
  return(df)
}


# read in each .txt file in file_list and rbind them into a data frame called data 
file_list <- list.files(path=nj, pattern="*.xls")  

data <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = str_sub(x, 5, 6),
                         cleaner(read_excel(paste(nj, x, sep=''))))))
                           
# prep data for graphs 
data$value <- as.numeric(sub("%","",data$value))/100
data$year <- as.numeric(as.character(data$year))
data$year <- ifelse(data$year<10,
                    paste('200', data$year, sep=''),
                    paste('20', data$year, sep=''))
data$year <- as.numeric(as.character(data$year)) 

# create county level graphs
county.graph <- function(df, na.rm = TRUE, ...){
  
  county_list <- unique(data$county)
  #county_list <- 'Morris'
    
  for (i in seq_along(county_list)) {
  
    plot <- 
    ggplot(subset(df, data$county==county_list[i]),
         aes(year, value, group = county, colour = category)) + 
    geom_line(size=2) +
    facet_wrap( ~  category, ncol=6) +
    theme_pander() +
    theme(legend.position="none") + 
    scale_y_continuous("Percent of County Population", labels = percent, limits=c(0, max(data$value[data$county==county_list[i]]))) +
    scale_x_continuous("Year") +
    ggtitle(paste(county_list[i], ' County \n', "Percent of County Population within Age Categories \n", sep=''))
    
    ggsave(plot, file=paste(results, 'county_graphs/' ,county_list[i], ".png", sep=''), scale=2)
    print(plot)
    }
}

county.graph(data)


# graph of school age population by county in North NJ at county level 
county.graph2 <- function(df, na.rm = TRUE, ...){
  
  keep <- c('Under 5 years', '5 to 9 years', '10 to 14 years', '15 to 19 years')
  df <- subset(df, df$category %in% keep)
  
  keep <- c("Bergen", "Essex", "Hudson", "Hunterdon",  "Mercer", "Middlesex", "Morris", "Passaic", "Somerset", "Union")      
  df <- subset(df, df$county %in% keep)
  
  plot <- 
      ggplot(df,
             aes(year, value, group = county, colour = county)) + 
      geom_line(size=2) +
      facet_wrap(category ~ county, ncol=10) +
      theme_pander() +
      theme(legend.position="none") + 
      scale_y_continuous("Percent of County Population", labels = percent, limits=c(0, max(data$value))) +
      scale_x_continuous("Year") +
      ggtitle(paste("Percent of County Population within Age Categories \n", sep=''))
    
    ggsave(plot, file=paste(results,  'county_graphs/', 'north_nj/', 'school_age_population_north_nj', ".png", sep=''), scale=3)
    print(plot)
}

county.graph2(data)


# Country level US data ------------------------------------------------------
# df <- data
# df$year <- NULL

# cleaner
cleaner <- function(df){
  # drop rows with all missing values 
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # keep only relevant rows
  df <- df[1:22,]
  df <- df[-c(1:2, 4), 1:2]
  
  df$Subject <- str_trim(as.character(df$Subject))
  names <- df$Subject
  df$Subject <- NULL
  
  df <- as.data.frame(t(df))

  colnames(df) <- names

  return(df)
}


# read in each .txt file in file_list and rbind them into a data frame called data 
file_list <- list.files(path=us, pattern="*.xls")  
#file_list <- file_list[1]

data <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = str_sub(x, 5, 6),
                         cleaner(read_excel(paste(us, x, sep=''))))))

data$year <- as.numeric(as.character(data$year))
data$year <- ifelse(data$year<10,
                      paste('200', data$year, sep=''),
                      paste('20', data$year, sep=''))
data$year <- as.numeric(as.character(data$year)) 

data[,2] <- as.numeric( as.character(gsub(",", "", data[,2])))

population_df <- data[ , 1:2]
colnames(population_df) <- c('year', 'population')
data <- data[ , -2]

data <- melt(data, id.vars=c("year"), variable.name="category")

data$value <- as.numeric(sub("%", "", data$value))/100

country.graph <- function(df, na.rm = TRUE, ...){
  
  plot <- 
    ggplot(df,
           aes(year, value, colour = category)) + 
    geom_line(size=2) +
    facet_wrap( ~ category, ncol=6) +
    theme_pander() +
    theme(legend.position="none") + 
    scale_y_continuous("Percent of US Population", labels = percent) +
    scale_x_continuous("Year") +
    ggtitle(paste("Percent of US Population within Age Categories \n", sep=''))
  
  ggsave(plot, file=paste(results, 'country_graphs/', 'us_population', ".png", sep=''), scale=2)
  print(plot)
  
}

country.graph(data)


country.pop.graph <- function(df, na.rm = TRUE, ...){
  
df$population <- df$population/1000000  
  
plot <- 
    ggplot(df,
           aes(year, population)) + 
    geom_line(size=2) +
    #facet_wrap( ~ category, ncol=6) +
    theme_pander() +
    theme(legend.position="none") + 
    scale_y_continuous("US Population (millions)") +
    scale_x_continuous("Year") +
    ggtitle(paste("US Population over Time \n", sep=''))
  
  ggsave(plot, file=paste(results, 'country_graphs/', 'us_count_population', ".png", sep=''), scale=2)
  print(plot)
  
}

country.pop.graph(population_df)


# NJ state level data ------------------------------------------
#df <- data
#df$year <- NULL

# cleaner
cleaner <- function(df){
  # drop rows with all missing values 
  df <- df[rowSums(is.na(df)) != ncol(df),]
  
  # keep only relevant rows
  df <- df[1:22,]
  df <- df[-c(1:4), 1:2]
  
  df$Subject <- str_trim(as.character(df$Subject))
  names <- df$Subject
  
  df <- as.data.frame(t(df))
  colnames(df) <- names
  df <- df[-1,]

  return(df)
}


# read in each .txt file in file_list and rbind them into a data frame called data 
file_list <- list.files(path=state, pattern="*.xls")  

data <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = str_sub(x, 5, 6),
                         cleaner(read_excel(paste(state, x, sep=''))))))


data$year <- as.numeric(as.character(data$year))
data$year <- ifelse(data$year<10,
                    paste('200', data$year, sep=''),
                    paste('20', data$year, sep=''))
data$year <- as.numeric(as.character(data$year)) 

data <- melt(data, id.vars=c("year"), variable.name="category")

data$value <- as.numeric(sub("%", "", data$value))/100


state.graph <- function(df, na.rm = TRUE, ...){
  plot <- 
      ggplot(subset(df),
             aes(year, value, group = category, colour = category)) + 
      geom_line(size=2) +
      facet_wrap( ~  category, ncol=6) +
      theme_pander() +
      theme(legend.position="none") + 
      scale_y_continuous("Percent of State Population", labels = percent) +
      scale_x_continuous("Year") +
      ggtitle("Percent of State Population within Age Categories \n")
    
    ggsave(plot, file=paste(results, 'state_graphs/', 'nj_state_level', ".png", sep=''), scale=2)
    print(plot)
}

state.graph(data)


state.graph2 <- function(df, na.rm = TRUE, ...){
  
  keep <- c('Under 5 years', '5 to 9 years', '10 to 14 years', '15 to 19 years')
  df <- subset(df, df$category %in% keep)
  
  plot <- 
    ggplot(subset(df),
           aes(year, value, group = category, colour = category)) + 
    geom_line(size=3) +
    facet_wrap( ~  category, ncol=2) +
    theme_pander() +
    theme(legend.position="none") + 
    scale_y_continuous("Percent of State Population", labels = percent, limits=c(0, max(df$value))) +
    scale_x_continuous("Year") +
    ggtitle("Percent of State Population within Age Categories \n")
  
  ggsave(plot, file=paste(results, 'state_graphs/', 'nj_state_level_school_age', ".png", sep=''), scale=2)
  print(plot)
}

state.graph2(data)



# department of labor population projections ------------------------------

file_list <- list.files(path=labor, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
data.labor <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = as.numeric(str_sub(x, 1, 4)),
                         read.csv(paste(labor, x, sep=''), 
                            stringsAsFactors = FALSE))))

# remove commas from numeric variables
data.labor[,c(3:12)] <- lapply(data.labor[,c(3:12)],function(x){as.numeric(gsub(",", "", x))})
# sapply(data.labor, class)

# drop 2010 from data then data and projections will occur in 5 year intervals 
data.labor <- subset(data.labor, data.labor$year!=2010)


#rename cols 
colnames(data.labor) <- c("Year",   "County", "Total",  
                          "Under 5",  '5 to 9 years', '10 to 14 years', '15 to 19 years', 
                          "X20.24", "X25.29", "X30.34", "X35.39", "X40.44")

keep <- c("Year", "County", "Total", 'Under 5', '5 to 9 years', '10 to 14 years', '15 to 19 years')
data.labor.temp <- data.labor
data.labor <- data.labor[keep]


data.labor.long <- melt(data.labor, id.vars=c("County", "Year"), variable.name="category")

# isolate total state estimate 
data.labor.long.total <- subset(data.labor.long, data.labor.long$category=='Total')
data.labor.long.total.nj <- subset(data.labor.long, data.labor.long$County=='New Jersey')
data.labor.long.total <- subset(data.labor.long, data.labor.long$County!='New Jersey')

# remove total estimate
data.labor.long <- subset(data.labor.long, data.labor.long$category!='Total')
data.labor.long.nj <- subset(data.labor.long, data.labor.long$County=='New Jersey')
data.labor.long <- subset(data.labor.long, data.labor.long$County!='New Jersey')
 
# total nj projection (total state population)

plot <- 
  ggplot(subset(data.labor.long.total.nj, data.labor.long.total.nj$category=='Total'), 
         aes(Year, value/1000000, group = County, colour = County)) + 
  geom_line(size=2) +
  #facet_wrap(category ~ County, ncol=1) +
  theme_pander() +
  theme(legend.position="none") + 
  scale_y_continuous("Total State Population Projection (millions)", limits=c(0, max(data.labor.long.total.nj$value/1000000))) +
  scale_x_continuous("Year") +
  ggtitle(paste("New Jersey Population Projection \n", sep=''))

ggsave(plot, file=paste(results, 'projection_graphs/', 'state_total_population_projection', ".png", sep=''), scale=2)
print(plot)

# total nj projection by age group (school age population only)

plot <- 
  ggplot(data.labor.long.nj, 
         aes(Year, value/1000000, group = category, colour = category)) + 
  geom_line(size=2) +
  facet_wrap(category ~ County, ncol=2) +
  theme_pander() +
  theme(legend.position="none") + 
  scale_y_continuous("State School Age Population Projection (millions)", limits=c(0, max(data.labor.long.nj$value/1000000))) +
  scale_x_continuous("Year") +
  ggtitle(paste("New Jersey School Age Population Projection \n", sep=''))

ggsave(plot, file=paste(results, 'projection_graphs/', 'state_school_age_projection_within_age_groups', ".png", sep=''), scale=2)
print(plot)


# plot <- 
#   ggplot(data.labor.long, 
#          aes(Year, value/1000000, group = County, colour = County)) + 
#   geom_line(size=2) +
#   facet_wrap(category ~ County, ncol=4) +
#   theme_pander() +
#   theme(legend.position="none") + 
#   scale_y_continuous("State School Age Population Projection (millions)", limits=c(0, max(data.labor.long$value/1000000))) +
#   scale_x_continuous("Year") +
#   ggtitle(paste("New Jersey School Age Population Projection \n", sep=''))
# 
# ggsave(plot, file=paste(results, 'projection_graphs/', 'state_school_age_projection_within_age_groups', ".png", sep=''), scale=2)
# print(plot)









# all counties 

plot <- 
  ggplot(data.labor.long,
         aes(Year, value/1000, group = County, colour = County)) + 
  geom_line(size=2) +
  facet_wrap(category ~ County, ncol=10) +
  theme_pander() +
  theme(legend.position="none") + 
  scale_y_continuous("County Population (thousands)") +
  scale_x_continuous("Year") +
  ggtitle(paste("County Population within Age Categories\n", sep=''))

ggsave(plot, file=paste(results, 'projection_graphs/', 'counties_school_age_population_projection', ".png", sep=''), scale=2)
print(plot)







# limit to north jersey counties  
keep <- c("Bergen", "Essex", "Hudson", "Hunterdon",  "Mercer", "Middlesex", "Morris", "Passaic", "Somerset", "Union")      
data.labor.long.north.nj <- subset(data.labor.long, data.labor.long$County %in% keep)
# north nj county level graph

  plot <- 
    ggplot(data.labor.long.north.nj,
           aes(Year, value/1000, group = County, colour = County)) + 
    geom_line(size=2) +
    facet_wrap(category ~ County, ncol=10) +
    theme_pander() +
    theme(legend.position="none") + 
    scale_y_continuous("County Population (thousands)", limits = c(0, max(data.labor.long.north.nj$value/1000))) +
    scale_x_continuous("Year") +
    ggtitle(paste("Northern NJ County Population within Age Categories\n", sep=''))
  
  ggsave(plot, file=paste(results, 'projection_graphs/', 'north_nj_counties_school_age_population_projection', ".png", sep=''), scale=3)
  print(plot)


# limit to north jersey counties all age groups 
colnames(data.labor.temp) <- c("Year",           "County",         "Total",          "Under 5",        "5 to 9 years",  
                               "10 to 14 years",  "15 to 19 years", "20 to 24 years",         "25 to 29 years",
                               "30 to 34 years",  "35 to 39 years",         "40 to 44 years"  )


data.labor.long.temp <- melt(data.labor.temp, id.vars=c("County", "Year"), variable.name="category")


keep <- c("Bergen", "Essex", "Hudson", "Hunterdon",  "Mercer", "Middlesex", "Morris", "Passaic", "Somerset", "Union")      
data.labor.long.temp <- subset(data.labor.long.temp, data.labor.long.temp$County %in% keep)
# north nj county level graph


data.labor.long.temp <- subset(data.labor.long.temp, data.labor.long.temp$category!='Total')
#data.labor.long.temp <- subset(data.labor.long.temp, data.labor.long.temp$County!='New Jersey')


plot <- 
  ggplot(data.labor.long.temp,
         aes(Year, value/1000, group = County, colour = County)) + 
  geom_line(size=2) +
  facet_wrap(category ~ County, ncol=10) +
  theme_pander() +
  theme(legend.position="none") + 
  scale_y_continuous("County Population (thousands)", limits = c(0, max(data.labor.long.temp$value/1000))) +
  scale_x_continuous("Year") +
  ggtitle(paste("Northern NJ County Population within Age Categories\n", sep=''))

ggsave(plot, file=paste(results, 'projection_graphs/', 'north_nj_counties_population_projection', ".png", sep=''), scale=3)
print(plot)














