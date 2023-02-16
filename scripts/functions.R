convex_hull_resample <- function(x){
  
  #' this function re-samples, data ala jacknife - leave one out, an SF object
  #'  and calculates a convex hull
  
  #' @param x an sf/tibble/dataframe, split into a  list of dataframes for distinct
  #' groups
  #' @param summ  way to summarize the extent, defaults to median
  
  if(missing(x)){stop('No data provided to function')}
  
  rown <- nrow(x)
  records <- 1:rown
  boots <- numeric(rown)
  for (record in records){
      boots[record] <- st_area(
        st_convex_hull(
          st_union(
            x[-record,])
          )
        )
  }
  
  data <- data.frame(
    Species = x$scrubbed_species_binomial[1], 
    Records = rown, 
    Min_CH_Ha = min(boots) * 0.0001,
    Max_CH_Ha = max(boots) * 0.0001,
    Median_CH_Ha = median(boots) * 0.0001
    )
  
  return(data)
  
}

