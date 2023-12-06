## R for Data Science ##

# Explore -----------------------------------------------------------------

library (tidyverse)

ggplot(data=mpg) + geom_point(mapping = aes( x = displ, y = hwy))

## displ = engine size hwy = fuel efficiency
## negative relationship with fuel eff and engine size

ggplot(data=mpg) + geom_point(mapping = aes( x = displ, y = hwy, color = class))

## color class of car to examine why outliers exist

## aesthetic mapping

ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

# alpha aesthetic
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shape aesthetic
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy, shape = class))
#> Warning: Using shape only works out to 6 categories, will drop others after that 


# Set aesthetic yourself 

ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

## change stroke for border, fill for donut poiint, shape

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue", 
             shape = 21, fill = "white", size = 1, stroke =5)

## Facets 

ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_wrap(~ class, nrow = 2)

## facet grid 

ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_grid(drv ~ cyl)

##remove facet from rows 
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) + 
        facet_grid(. ~ cyl)

## remove facet from columns
ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) +
        facet_grid(drv ~ .)


## Geometric Objects 

# smooth the points to a trend 
ggplot(data = mpg) + 
        geom_smooth(mapping = aes(x = displ, y = hwy))
## add in different lines based on third variable 
ggplot(data = mpg) + 
        geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

##overlay geoms 

ggplot(data = mpg) + 
        geom_point(mapping = aes(x = displ, y = hwy)) +
        geom_smooth(mapping = aes(x = displ, y = hwy))

## cleaner version 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
        geom_point() + 
        geom_smooth()

##add in color for class of car 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
        geom_point(mapping = aes(color = class)) + 
        geom_smooth()

##edit one geom to only show a subset of values 

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
        geom_point(mapping = aes(color = class)) + 
        geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


##statistical transformations 

ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut))

### allows for groupings to count data in x and map it to a count. This works for bar chart
### histiogram, and frequency polygons using internal function called stat_count()

 ##example of what it is doing above 

ggplot(data = diamonds) + 
        stat_count(mapping = aes(x = cut))

## if you want to map another way, like proportion rather than count, try 

ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

###add color around bars 

ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, colour = cut))

##change bar color 
ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, fill = cut))

##stacking variables by color 

ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, fill = clarity))

## help compare individual values 

ggplot(data = diamonds) + 
        geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

##Coordinate systems 

##flip x and y axis 

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
        geom_boxplot() +
        coord_flip()

### Coxcomb chart 

bar <- ggplot(data = diamonds) + 
        geom_bar(
                mapping = aes(x = cut, fill = cut), 
                show.legend = FALSE,
                width = 1
        ) + 
        theme(aspect.ratio = 1) +
        labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


### save a graph or chart 

ggsave(filename = "mpg-plot.png", plot = my_bar_plot)

#Transform----------------
library(nycflights13)

library(dplyr)
 ## filter function for rows 
### >, <, ==, >=, <=, !=, &, , | for or 

flights %>%
        filter (dest == "IAH") %>%
        group_by(year, month, day) %>% 
        summarize(
                arr_delay = mean(arr_delay, na.rm = TRUE)
        )

##flights in Januar yand February 
flights %>% 
        filter(month == 1 | month == 2)

## more concise is 

flights %>% 
        filter(month %in% c(1,2))

JanFeb <- flights %>%
        filter(month %in% c(1,2))

## arrange columns
 flights %>%
         arrange(year, month, day, dep_time)
 
 ## arrange data frame within a column
 
 flights %>%
         arrange(desc(dep_delay))
 
 ##distinct operates on rows and can remove duplicates 
 
 ##find all unique origin and destination pairs and keep all other columns when filtering for unqiue rows
 
 flights %>% 
         distinct(origin, dest, .keep_all = TRUE)
 
 ##count number of unique occurances 
 
 flights %>% 
         count(origin, dest, sort = TRUE)
