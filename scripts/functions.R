convex_hull_resample <- function(x){
  
  #' this function re-samples, data ala jacknife - leave one out, an SF object
  #'  and calculates a convex hull
  
  #' @param x an sf/tibble/dataframe, split into a  list of dataframes for distinct
  #' groups
  #' @param summ  way to summarize the extent, defaults to median
  
  sf::sf_use_s2(FALSE)
    
  if(missing(x)){stop('No data provided to function')}
  
  rown <- nrow(x)
  full_ch <- x %>% 
    st_union() %>% 
    st_convex_hull()
  
  if(rown >= 10){
    
    boundary_pts <- st_touches(full_ch, x, sparse = F)
    pts2rm <- which(boundary_pts == T)
    x[boundary_pts == T, 'boundary'] <- T
    
   boots <- numeric(rown)
    for (record in pts2rm){
        boots[record] <- st_area(
            st_convex_hull(
              st_union(
                x[-record,])
            )
          )
    }
   boots <- boots[boots > 0]
  
    data <- data.frame(
      Species = x$scrubbed_species_binomial[1], 
      Records = rown, 
      Min_CH_Ha = min(boots) * 0.0001,
      Max_CH_Ha = max(boots) * 0.0001,
      Full_CH_Ha = st_area(full_ch) * 0.001,
      Median_CH_Ha = median(boots) * 0.0001
      )
  }
  
  else(
    
    data <- data.frame(
      Species = x$scrubbed_species_binomial[1], 
      Records = rown, 
      Min_CH_Ha = NA,
      Max_CH_Ha = NA,
      Full_CH_Ha = st_area(full_ch) * 0.001,
      Median_CH_Ha = NA
    )
  )
  
#  on.exit(options(sf::sf_use_s2(TRUE)))
  return(data)
  
}

