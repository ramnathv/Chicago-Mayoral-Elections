choropleth_sp <- function(
  
### Generic function to create a choropleth map using spplot. The minimum input required is to a shapefile, an id variable to define a region, a field variable to indicate the values to to be plotted and a set of arguments to classIntervals to define breaks. Since the function returns an object of class spplot, it is possible to further customize the  plot by modifying the plot object.

  .poly,                   ##<< shapefile containing the map to plot
   id1,                    ##<< list of fields in shapefile to match with data 
   .df      = .poly@data,  ##<< data frame containing data to plot
   id2      = id1,         ##<< list of fields in data to match with shapefile
   field,                  ##<< vector of fields with values to plot 
  .title    = "",          ##<< title of the plot
  .legtitle = "Values",    ##<< title of the plot legend
   colpal   = 'PuRd',      ##<< brewer color palette to be used
   ...){                   ##<< arguments to send to classIntervals
                            
     # load required libraries
     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
                            
     # create id to match shapefile with data
     id1 = llply(id1, as.name); 
     id2 = llply(id2, as.name);
    .poly@data$id = with(.poly@data, do.call('paste', c(id1, sep = "-")));
    .df$id        = with(.df,        do.call('paste', c(id2, sep = "-")));
     
      # merge shapefile with data
      map.data = merge(.poly@data, .df);
      
      # choose breaks for values
      map.data.m  = melt(map.data, measure.vars = field);
      breaks      = do.call("classIntervals", 
                      list(var = map.data.m$value, ...))$brks;
      
      # plot choropleth map
      colors    = brewer.pal(length(breaks) - 1, colpal);
     .poly@data = map.data[match(.poly@data$id, map.data$id),];
      p1 = spplot(.poly, field, at = breaks, col.regions = colors, 
        col = 'transparent', main = .title);
           
      return(p1)
}

# FUNCTION TO READ SHAPE FILE FROM A ZIPPED FOLDER ---------------------------
readShapeZip <- function(

### Reads a shapefile directly from a specified url containing a zip file. 
  
   url,                    ##<< url to a zip file containing shape file
  .tempfile = tempfile(),  ##<< filename to save downloaded files after unzip
  .tempdir = tempdir()){   ##<< folder to save downloaded files after unzip
                
   # needs maptools and gpclibPermit
   require(maptools); gpclibPermit();
   
   # download shapefiles from url and unzip them to .tempdir
   download.file(url = url, .tempfile, quiet = T);
  .files    = unzip(.tempfile, exdir = .tempdir);
  
   # find .shp file and read spatial polygon into .poly  
  .shpfile  = head(.files)[grep('shp', head(.files))];
  .poly     = readShapeSpatial(.shpfile);
  
   return(.poly)
   ### Returns a Spatial Polygons object
}

# Example 1: London Sport Participation
# Source: http://goo.gl/sL0N0 (Spatial Analysis Blog)
london <- readShapeZip('http://goo.gl/oF4b8');
choro1_sp = choropleth_sp(.poly = london, 
  id1 = 'name', 
  field = 'Partic_Per',
 .title    = 'London Sport Participation',
 .legtitle = 'Partic_Per',
  colpal   = 'Blues', style = 'fixed',
  fixedBreaks   = seq(0, 30, by = 5))
  
# Example 2: London Data Maps Visualization ----------------------------------
# Source: http://goo.gl/VsIgo (Mark Bulling)

london <- readShapeZip('http://goo.gl/oF4b8');
immig  <- read.csv('http://goo.gl/lhvDW');
choro2_sp <- choropleth_sp(.poly = london, .df = immig,
   id1      = 'name', 
   id2      = 'Area',
   field    = c('India', 'Pakistan'),
  .title    = 'Immigrant Registrations (2009)',
  .legtitle = '', 
   colpal   = 'PuRd', n = 5);
  
# Example 3: Mapping Chicago Mayoral Elections 2011 --------------------------

# Download chicago map and election results from the web
chicago   = readShapeZip('http://goo.gl/XU3Vr');
election  = read.csv('http://goo.gl/kL5E5',
  colClasses = c('character', rep('numeric', 8)),
  col.names  = c('precinct', 'registered', 'ballots', 
     'walls', 'watkins', 'chico', 'braun', 'delvalle', 'emanuel')
);

election[,4:9] = election[,4:9]/election$ballots

# Plot vote pct for Rahm Emanuel
choro3_sp = choropleth_sp(.poly = chicago, .df = election,
   id1      = 'WARD_PRECI',
   id2      = 'precinct',
   field    = 'emanuel', 
  .title    = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %',
   colpal   = 'Greens', n = 5, style = 'pretty')
                      
# Plot vote pct for Rahm Emnauel and Gery Chico 
choro4_sp = choropleth_sp(.poly = chicago, .df = election, 
   id1      = 'WARD_PRECI',
   id2      = 'precinct',
   field     = c('emanuel', 'chico'), 
  .title    = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %',
   colpal   = 'Oranges', n = 5, style = 'pretty') 
  
# Mapping Unemployment -------------------------------------------------------


# Load US county map and unemployment statistics
usmap <- readShapeSpatial('1_data/US/maps/co99_d00.shp');
unem  <- read.csv('http://goo.gl/2xsjE',
  colClasses = c(rep('character', 8), 'numeric'),
  col.names  = c('code', 'STATE', 'COUNTY', 'name', 'yr', 
    'x1', 'x2', 'x3', 'percentage')
)

choro5_sp = choropleth_sp(.poly = usmap, .df = unem,
   id1   = list('STATE', 'COUNTY'), 
   field = 'percentage',
  .title = 'US Unemployment (2009)',
  .legtitle = '',
   colpal   = 'PuRd', style = 'fixed',
   fixedBreaks   = c(seq(0, 10, 2), 100))

choro5_sp = update(choro5_sp, xlim = c(-129,-61), ylim = c(21,53), 
  aspect = 'iso')