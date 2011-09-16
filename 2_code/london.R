source('lib/utilities.R');

# Example 4: London Sport Participation
# Source: http://goo.gl/sL0N0 (Spatial Analysis Blog)
london <- readShapeSpatial('1_data/london/maps/london_sport.shp');
choro4gg = plotChoroplethB(.poly = london, 
  id1 = 'name', field = 'Partic_Per',
  title    = 'London Sport Participation',
  legtitle = 'Partic_Per', type = 'ggplot',
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5)) +
  opts(legend.position = c(0.85, 0.25));
  
choro4sp = plotChoroplethB(.poly = london, 
  id1 = 'name', field = 'Partic_Per',
  title    = 'London Sport Participation',
  legtitle = 'Partic_Per', type = 'spplot',
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5)); 

# Example 5: London Data Maps Visualization
# Source: http://goo.gl/VsIgo (Mark Bulling)

london <- readShapeSpatial('1_data/london/maps/london_sport.shp');
immig  <- read.csv('http://goo.gl/lhvDW');
choro5gg <- plotChoroplethB(.poly = london, .df = immig,
  id1 = 'name', id2 = 'Area',
  field = c('India', 'Pakistan'),
  title    = 'Immigrant Registrations (2009)',
  legtitle = '', type = 'ggplot', 
  colpal   = 'PuRd', n = 5) +
  opts(legend.position = 'top', legend.direction = 'horizontal');
  
choro5sp <- plotChoroplethB(.poly = london, .df = immig,
  id1 = 'name', id2 = 'Area',
  field = c('India', 'Pakistan'),
  title    = 'Immigrant Registrations (2009)',
  legtitle = '', type = 'spplot', 
  colpal   = 'PuRd', n = 5) 



# Example 3: Mapping London's Population Change From 1801 - 1830
# Source: http://goo.gl/FrZOm (Spatial Analysis Blog)

df <- read.csv("http://goo.gl/wPCr5")

# extract relevant columns, and add total column
df <- df[,c('Area.Code', 'Area.Name', 'Year', 'Total.Males', 'Total.Females')];
df$Total <- with(df, Total.Males + Total.Females);

# cast data with years as columns to input to plotChoropleth
df <- cast(df, Area.Code + Area.Name ~ Year, value = 'Total');

# add year on year population increase by area to data
# yoy  <- t(ldply(3:(ncol(df) - 1), function(n) {df[,(n + 1)] - df[,n]}));
# df   <- cbind(df[, 1:2], yoy);
# names(df)[3:ncol(df)] = as.character(2001:2030)

# Animated Choropleth of Population Density Change

df        = read.csv('http://goo.gl/yJQs8');
names(df) = sub('Persons.', '', names(df));

df_area   = read.xls('http://goo.gl/UJbKE', 
              sheet = 2, pattern = 'Code')[,c('Code', 'Total')];
df_area$Total = with(df_area, sapply(as.character(Total), sub, pa = ",", r = ''))
df_area$Total = with(df_area, sapply(Total, as.numeric))
              
df = merge(df_area, df, by.x = 'Code', by.y = 'Area.Code');
df[,4:ncol(df)] = df[,4:ncol(df)]/df$Total*100;

                                          
library(animation)
saveMovie(for (i in 4:ncol(df)) 
   print(plotChoropleth(.poly = london, .df = df, 
   id1 = 'ons_label',  id2 = 'Code', 
   field = as.character(names(df)[i]),   
   colpal = 'Oranges', fixed = T, 
   title = 'Londons Population Density',
   legtitle = 'Thousands per Sq Km',
   breaks = c(0, 100, 1000, 5000, 10000, 50000))), 
   clean = T)
     
