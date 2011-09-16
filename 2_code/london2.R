source('lib/utilities.R');
source('lib/choropleth_gg.R');
source('lib/choropleth_sp.R');

# Example 1: London Sport Participation ---------------------------------------
# Source: http://goo.gl/sL0N0 (Spatial Analysis Blog)

london <- readShapeSpatial('1_data/london/maps/london_sport.shp');

choro1_gg = choropleth(.poly = london, 
  id1 = 'name', field = 'Partic_Per',
  .title    = 'London Sport Participation',
  .legtitle = 'Partic_Per', type = 'ggplot',
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5)) +
  opts(legend.position = c(0.85, 0.25));
  
choro1_sp = choropleth(.poly = london, 
  id1 = 'name', field = 'Partic_Per',
  .title    = 'London Sport Participation',
  .legtitle = 'Partic_Per', type = 'spplot',
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5))
  
# Example 2: London Data Maps Visualization -----------------------------------
# Source: http://goo.gl/VsIgo (Mark Bulling)

london <- readShapeSpatial('1_data/london/maps/london_sport.shp');
immig  <- read.csv('http://goo.gl/lhvDW');

choro2_gg <- choropleth_gg(.poly = london, .df = immig,
  id1 = 'name', id2 = 'Area',
  field = c('India', 'Pakistan'),
  .title    = 'Immigrant Registrations (2009)',
  .legtitle = '', 
  colpal   = 'PuRd', n = 5) +
  opts(legend.position = 'top', legend.direction = 'horizontal');
  
# Example 3: Electricity Generation By Country --------------------------------
# Source : http://goo.gl/rVBZF

world <- readShapeZip('http://goo.gl/rZKt6');
elec  <- read.csv('1_data/electricity/electricity.csv', stringsAsFactors = F);
elec[,-1] <- sapply(elec[,-1], as.numeric);

choro3_gg = choropleth_gg(.poly = world, 
  id1 = 'NAME', .df = elec, id2 = 'Country', 
  field = 'X2007',
 .title = 'Electricity Generation (2007)',
  colpal = 'PuRd', n = 5);
