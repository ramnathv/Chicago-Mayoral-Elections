source('lib/readShapeZip.r');
source('lib/merge_poly_df_v2.r');
source('lib/find_fill_breaks.r');
source('lib/choropleth.r');
source('lib/utilities.r');

# Example 1: London Sport Participation
# Source: http://goo.gl/sL0N0 (Spatial Analysis Blog)
london <- readShapeZip('http://goo.gl/oF4b8');
choro1_sp = choropleth(.poly = london, 
  id1 = 'name', 
  field = 'Partic_Per',
 .title    = 'London Sport Participation',
 .legtitle = 'Partic_Per', .ggplot = F,
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5))
  
# Example 2: London Data Maps Visualization ----------------------------------
# Source: http://goo.gl/VsIgo (Mark Bulling)

london <- readShapeZip('http://goo.gl/oF4b8');
immig  <- read.csv('http://goo.gl/lhvDW');
choro2_gg <- choropleth(.poly = london, .df = immig,
   id1      = 'name', 
   id2      = 'Area',
   field    = c('India', 'Pakistan'),
  .title    = 'Immigrant Registrations (2009)',
  .legtitle = '', .ggplot = T,
   colpal   = 'PuRd', n = 5) +
   opts(legend.position = 'top', legend.direction = 'horizontal');
  
# Example 3: Mapping Chicago Mayoral Elections 2011 --------------------------

# download chicago map and election results from the web
chicago   = readShapeZip('http://goo.gl/XU3Vr');
election  = read.csv('http://goo.gl/kL5E5',
  colClasses = c('character', rep('numeric', 8)),
  col.names  = c('precinct', 'registered', 'ballots', 
     'walls', 'watkins', 'chico', 'braun', 'delvalle', 'emanuel')
);

election[,4:9] = election[,4:9]/election$ballots

# plot vote pct for Rahm Emanuel
choro3_sp = choropleth(.poly = chicago, .df = election,
   id1      = 'WARD_PRECI',
   id2      = 'precinct',
   field    = 'emanuel', 
  .title    = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', .ggplot = F,
   colpal   = 'PuRd', n = 5, style = 'pretty');
update(choro3_sp, aspect = 'iso');
                      
# Plot vote pct for Rahm Emnauel and Gery Chico 
choro4_sp = choropleth(.poly = chicago, .df = election, 
   id1      = 'WARD_PRECI',
   id2      = 'precinct',
   field     = c('emanuel', 'chico'), 
  .title    = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', .ggplot = F,
   colpal   = 'Oranges', n = 5, style = 'pretty') 
  
# Mapping Unemployment -------------------------------------------------------


# Load US county map and unemployment statistics
usmap <- readShapeSpatial('1_data/US/maps/co99_d00.shp');
unem  <- read.csv('http://goo.gl/2xsjE',
  colClasses = c(rep('character', 8), 'numeric'),
  col.names  = c('code', 'STATE', 'COUNTY', 'name', 'yr', 
    'x1', 'x2', 'x3', 'percentage')
)

choro5_sp = choropleth(.poly = usmap, .df = unem,
   id1   = list('STATE', 'COUNTY'), 
   field = 'percentage',
  .title = 'US Unemployment (2009)',
  .legtitle = '', .ggplot = F,
   colpal   = 'PuRd', style = 'fixed',
   fixedBreaks   = c(seq(0, 10, 2), 100))

choro5_sp = update(choro5_sp, xlim = c(-129,-61), ylim = c(21,53), 
               aspect = 'fill')