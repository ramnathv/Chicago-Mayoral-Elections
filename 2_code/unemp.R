fac2num <- function(x) as.numeric(as.character(x));

  
usmap <- readShapeSpatial('1_data/US/maps/co99_d00.shp');
unemp <- read.csv('1_data/US/unemp.csv');

usmap@data$STATE = sapply(usmap$STATE, fac2num)
usmap@data$STATE = sapply(usmap$STATE, fac2num)

choro4 = choropleth(.poly = usmap, .df = unemp,
 id1 = list('STATE', 'COUNTY'), 
 id2 = list('sfips', 'cfips'), 
 field = 'unemprate',
 .title    = 'US Unemployment (2009)',
 .legtitle = '', .ggplot = FALSE,
 colpal   = 'PuRd', style = 'fixed',
 fixedBreaks   = c(seq(0, 10, 2), 100))


## read the unemployment data:
# code from barry rowlinson

unem <- read.csv('http://goo.gl/2xsjE',
  colClasses = c(rep('character', 8), 'numeric'),
  col.names  = c('code', 'STATE', 'COUNTY', 'name', 'yr', 'x1', 'x2', 'x3', 'percentage')
)


choro4 = plotChoroplethB(.poly = usmap, .df = unem,
 id1 = list('STATE', 'COUNTY'), field = 'percentage',
 title    = 'US Unemployment (2009)',
 legtitle = '',
 colpal   = 'PuRd', style = 'fixed',
 fixedBreaks   = c(seq(0, 10, 2), 100)) +
 opts(legend.position = c(0.85, 0.25));


