# Mapping Chicago Mayoral Elections 2011

source('lib/utilities.R');

# Load Data

# chicago   = readShapeZip('http://goo.gl/XU3Vr');
chicago  =  readShapeSpatial('1_data/chicago/maps/Precincts.shp');
election =  read.csv('1_data/chicago/precincts.csv');   
election = election[,-grep('count', names(election))];         
names(election) = sub('_pct', '', names(election));

# Plot Choropleth

# 1. Only Rahm Emanuel
choro1_gg = plotChoroplethB(.poly = chicago, .df = election,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  field = 'emanuel',  
  .title = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', type = 'ggplot',
  colpal = 'Blues', n = 5, style = 'pretty') +
  opts(legend.position = c(0.15, 0.25));
  
choro1_sp = choropleth(.poly = chicago, .df = election,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  field = 'emanuel',  
  .title = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', type = 'spplot',
  colpal = 'Blues', n = 5, style = 'pretty')

                      
# 2. Rahm Emnauel and Gery Chico as Facets
choro2_gg = choropleth(.poly = chicago, .df = election,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  field = c('emanuel', 'chico'), 
  .title = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', type = 'ggplot',
  colpal = 'Blues', n = 5, style = 'pretty') +
  opts(legend.position = c(0.15, 0.25));

choro2_sp = choropleth(.poly = chicago, .df = election,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  field = c('emanuel', 'chico'), 
  .title = 'Chicago Mayoral Election 2011',
  .legtitle = 'Votes %', type = 'spplot',
  colpal = 'Blues', n = 5, style = 'pretty')

  
# Plot Rahm Emanuel and Gery Chico as Animations

choro5 = function(cand) {plotChoropleth(.poly = chicago, .df   = election,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  field = cand, title = 'Chicago Mayoral Election 2011',
  legtitle = 'Votes %',
  colpal = 'Blues', n = 5, style = 'pretty')+
  opts(legend.position = c(0.15, 0.25))
}  
library(animation)
saveMovie(for (cand in c('emanuel', 'chico')) 
  print(choro5(cand)),
  clean = T)
  
                      
# Plot Vote Pct Secured by Winner
# Source: http://goo.gl/ve65F (Chicago Tribune)

# find winner and  pct votes
candidates = names(df)[4:9];                                
df$winner  = candidates[apply(df[, candidates], 1, which.max)];    
df$winpct  = apply(df[, candidates], 1, max);

# assign palettes to winners and bin pct votes
df$colpal  = sapply(df$winner, switch, 
    'braun'    = 'PuRd', 
    'chico'    = 'Greens',
    'delvalle' = 'Oranges', 
    'emanuel'  = 'Purples')
df$bins = cut(df$winpct, c(0, 50, 75, 100), labels = F, include.lowest = T)

# choose color based on bin and palette
df = adply(df, 1, transform, 
       fillcol = brewer.pal(5, colpal)[bins + 2]);
       
choro3 = plotChoropleth(.poly = chicago, .df   = df,
  id1 = list('WARD', 'PRECINCT'),
  id2 = list('ward', 'precinct'),
  fill = 'fillcol', title = 'Chicago Mayoral Election 2011',
  legtitle = 'Winner Votes %',
  colpal = 'Blues', n = 5, style = 'pretty');                

