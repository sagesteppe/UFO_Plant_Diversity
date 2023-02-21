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
      Full_CH_Ha = as.numeric(st_area(full_ch)) * 0.001,
      Median_CH_Ha = median(boots) * 0.0001
      )
  }
  
  else(
    
    data <- data.frame(
      Species = x$scrubbed_species_binomial[1], 
      Records = rown, 
      Min_CH_Ha = NA,
      Max_CH_Ha = NA,
      Full_CH_Ha = as.numeric(st_area(full_ch)) * 0.001,
      Median_CH_Ha = NA
    )
  )
  
#  on.exit(options(sf::sf_use_s2(TRUE)))
  return(data)
  
}



# Boxplot theme

#' the boxplot is the data visualization technique which we use most often. 
#' We test it out here with our 4 main stratum being represented as the 
#' three species in the data set

#' @export
#' @rdname UFO_EoS
theme_boxplot <- function(){ 
  font = "sans"   #assign font family up front
  base_size = 10
  legend.position = 'none'
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 12,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,                #left align     
        vjust = 3
      ),
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 11),               #font size
      
      plot.caption = element_text(  #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(  #axis titles
        family = font),   
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 8),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      axis.line = element_line(
        colour = "grey25", linetype=3)
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

boxplot_drawer <- function(df, response, col_pal, group){
  
  term <- as.formula(paste(rlang::enexpr(response), ' ~ ', dplyr::enexpr(group)))
  response <- rlang::enquo(response)
  group <- rlang::enquo(group)
  
  my_means <- ggpubr::compare_means(term,  data = df)
  my_comparisons <- my_means %>% 
    nest(groups = c(group1, group2)) %>% 
    pull(groups) %>% 
    map(., as.character)
  
  min_v <- dplyr::summarise(df, mean_mpg = floor(min(!!response))) |>
    pull() |> min()
  
  sample_sizes <- df %>% 
    dplyr::group_by(!!group) %>%  
    dplyr::tally() %>% 
    dplyr::mutate(n = paste0('n = ', n)) 
  
  ufo_boxplot <- ggplot(df, aes(x = !!group, y = !!response, colour = !!group),  
                        alpha = 0.5) + 
    stat_boxplot(notch = T, notchwidth = 0.75, 
                 varwidth = T, 
                 outlier.shape = 1, outlier.alpha = 0.8, outlier.colour = 'black') +
    
    geom_text(data = sample_sizes,
              aes(!!group, Inf, label = n), color = 'black', 
              vjust = "inward", size = 4, 
              y = min_v * 0.95) +
    ggpubr::stat_compare_means(comparisons = my_comparisons, 
                               aes(label = ..p.signif..),
                               tip.length = 0, vjust = 0.25, size = 4) +
    ggpubr::stat_compare_means(label.y = min_v * 2, size = 4)  +
    
    theme_boxplot() +
    expand_limits(y= min_v) +
    scale_colour_manual(values = col_pal) +
    theme(legend.position = 'none')
  
  return(ufo_boxplot)
}

strata_pal <- setNames(
  
  #' these colours were selected via extraction of colour hex's from images using
  #' the RImagePalette package. Thanks to Megan Bach for taking multiple of the 
  #' images which were used in the process.
  
  c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841',
             '#1357a6', '#1B1212', '#F9E076', '#39993A', '#00688B'),
             c('PJ', 'SS', 'SD', 'MMS', 'AS', 'RI', 'OT', 'GR', 'PP', 'MC')
  
)

