# Analysis on San Francisco Rentals

<table>
<thead>
<tr class="header">
<th style="text-align: center;">Name</th>
<th style="text-align: center;">Matriculation Number</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">Joel Lo Liang Ze</td>
<td style="text-align: center;">A0234274H</td>
</tr>
<tr class="even">
<td style="text-align: center;">Lim Tze Cherng Ethan</td>
<td style="text-align: center;">A0233622N</td>
</tr>
<tr class="odd">
<td style="text-align: center;">Mathumita Raju</td>
<td style="text-align: center;">A0245499L</td>
</tr>
<tr class="even">
<td style="text-align: center;">Tan Jing Kai, Brian</td>
<td style="text-align: center;">A0233495Y</td>
</tr>
<tr class="odd">
<td style="text-align: center;">Zhu Yiran</td>
<td style="text-align: center;">A0258237X</td>
</tr>
</tbody>
</table>

## Introduction

San Francisco Rentals data shows new construction activities, rental
prices (in USD) and building permits of the San Francisco Bay Area from
1990 to 2018. The Bay Area’s nine counties are Alameda, Contra Costa,
Marin, Napa, San Francisco, San Mateo, Santa Clara, Solano, and Sonoma.
The San Francisco Rentals data of `rent`, `permits` and
`new_construction` comes from Kate Pennington through web scraping
craigslist, data.sfgov.org and Vital Signs respectively. In this
project, we are looking to answer the following question regarding
rental prices in San Francisco:

-   **What are some possible determinants that affect rental price in
    San Francisco?**

To investigate this, we will utilize the primary data set: `rent` on San
Francisco Rentals from 2000 to 2018 obtained from
[tidytuesday/data/2022/2022-07-05/](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-07-05).

    library(tidyverse)
    library(maps)

    rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

A brief description of the variables **relevant** to our analysis are
given in the tables below:

### Rent

The key variables we will utilise from the `rent` dataset are `year`,
`county`, `price`, `sqft`, `lat` and `lon`. These variables provide
information on the year the rental unit’s advertisement was placed,
counties the rental units are located in, their rental prices, their
interior areas, and their geographical coordinates.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Variable</th>
<th style="text-align: left;">Class</th>
<th style="text-align: left;">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">year</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">year the rental is posted in</td>
</tr>
<tr class="even">
<td style="text-align: left;">county</td>
<td style="text-align: left;">character</td>
<td style="text-align: left;">county the rental is posted in</td>
</tr>
<tr class="odd">
<td style="text-align: left;">price</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">monthly rental price in USD</td>
</tr>
<tr class="even">
<td style="text-align: left;">sqft</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">interior area of rental apartment in
square feet</td>
</tr>
<tr class="odd">
<td style="text-align: left;">lat</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">latitude (paired with with longitude to
give location)</td>
</tr>
<tr class="even">
<td style="text-align: left;">lon</td>
<td style="text-align: left;">double</td>
<td style="text-align: left;">longitude (paired with with latitude to
give location)</td>
</tr>
</tbody>
</table>

### Grouping of counties

Conventionally, the nine-county Bay Area can be divided further into
sub-regions based on their geographical locations: East Bay, North Bay,
the city of San Francisco, San Francisco Peninsula, and South Bay. The
counties that fall under the five sub-regions are detailed in the
following table. We would be following this grouping of counties for all
of our visualisations.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Sub-Region</th>
<th style="text-align: left;">County/Counties</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">East Bay</td>
<td style="text-align: left;">Contra Costa, Alameda</td>
</tr>
<tr class="even">
<td style="text-align: left;">North Bay</td>
<td style="text-align: left;">Sonoma, Napa, Solano, Marin</td>
</tr>
<tr class="odd">
<td style="text-align: left;">San Francisco City</td>
<td style="text-align: left;">San Francisco</td>
</tr>
<tr class="even">
<td style="text-align: left;">San Francisco Peninsula</td>
<td style="text-align: left;">San Mateo</td>
</tr>
<tr class="odd">
<td style="text-align: left;">South Bay</td>
<td style="text-align: left;">Santa Clara</td>
</tr>
</tbody>
</table>

## Data Cleaning and Summary

The relevant columns were selected from the `rent` dataset and grouped
by Sub-Region to form three new data frames for data visualisation.
Moreover, only rows containing no NA values from year 2000 to year 2018
are considered. `santa cruz` county is excluded as it is not part of the
Bay Area. The Average Rental Prices per Sub-Region per Year, Geospatial
Location of the Rental and Rental Prices per Square Feet per Sub-Region
are the 3 data frames derived from the `rent` dataset to plot how price
varies with time, location and rental size.

### Average Rental Prices per Sub-Region per Year

The columns `year`, `county`, `price` is selected from `rent` to plot
average rental prices of the sub-region from year 2000 to year 2018. The
summary table below shows the ranges of average rental prices and the
minimum starting year per sub-region. It is to be noted that the minimum
year for San Francisco Peninsula is year 2001 and not year 2000 as there
are no rental postings in San Mateo in year 2000.

    average_rental_prices <- rent %>% 
      # Selection of relevant columns from the rent dataset for visualisation 
      select(year, county, price) %>%
      # Excluding county not part of the Bay Area
      filter(county != "santa cruz") %>%
      # Use only data from 2000 to 2018
      filter(2000<=year & year<=2018) %>%
      # Omitting of NA values
      na.omit() %>%
      # Grouping counties into their respective sub-regions under a new "sub_region" column
      mutate(sub_region = case_when(county == "alameda" | county == "contra costa" ~ "East Bay",
                         county == "sonoma" | county == "napa" | county == "marin" | county == "solano" ~ "North Bay",
                         county == "san mateo" ~ "San Francisco Peninsula",
                         county == "san francisco" ~ "San Francisco",
                         county == "santa clara" ~ "South Bay")) %>%
      # Grouping the variables for visualisation later on
      group_by(sub_region, year) %>%
      # Calculate average rental price for each year in each county
      summarise(rental_average = mean(price))

    # Printing a summary of the cleaned rental prices data frame
    average_rental_prices %>% 
      group_by(sub_region) %>% 
      summarise(`min year` = min(year), `max year` = max(year), `min avg price` = min(rental_average), `max avg price`= max(rental_average)) %>% 
      ungroup()

    ## # A tibble: 5 × 5
    ##   sub_region              `min year` `max year` `min avg price` `max avg price`
    ##   <chr>                        <dbl>      <dbl>           <dbl>           <dbl>
    ## 1 East Bay                      2000       2018           1029.           2622.
    ## 2 North Bay                     2000       2018           1230            3029.
    ## 3 San Francisco                 2000       2018           1666.           3948.
    ## 4 San Francisco Peninsula       2001       2018           1464.           3295.
    ## 5 South Bay                     2000       2018           1312.           3003.

### Geospatial Location of Rentals

The columns `county`, `price`, `lat`, `lon` is selected from `rent` to
plot the geospatial locations of rentals from year 2000 to year 2018.
The location data is rebounded to the San Francisco Bay Area of 36.8
&lt; latitude &lt; 38.8 and -123.5 &lt; longitude &lt; -121. The
longitudinal values were found to be shifted by 0.03 and would not fit
into our map plot, hence the correction was added after visually
plotting the rental location.

    # Preparing a vector containing counties of interest for filtering
    county_rentals <- c("alameda","contra costa","marin","napa","san francisco","san mateo","santa clara","solano","sonoma")

    # Extracting coordinates from the maps package for the boundaries of counties of interest
    county_coords <- map_data("county") %>% 
      filter(subregion %in% county_rentals) %>%
      # Grouping counties into their respective regions under a new "region" column
      mutate(sub_region = case_when(subregion == "alameda" | subregion == "contra costa" ~ "East Bay",
                         subregion == "sonoma" | subregion == "napa" | subregion == "marin" | subregion == "solano" ~ "North Bay",
                         subregion == "san mateo" ~ "San Francisco Peninsula",
                         subregion == "san francisco" ~ "San Francisco",
                         subregion == "santa clara" ~ "South Bay"))

    # Creating new data frame for rental prices, longitude and latitude
    rentals_and_coords <- rent %>%
      # Excluding county not part of the Bay Area
      filter(county != "santa cruz") %>%
      # Use only data from 2000 to 2018
      filter(2000<=year & year<=2018) %>%
      # Filtering rental listings within the area of interest - the 5 regions, there were
      # several values far outside of the area of interest
      filter(lat > 36.8, lat < 38.8, lon > -123.5, lon < -121) %>%
      # Selection of relevant columns from the rent dataset for visualization 
      select(price, lat, lon) %>%
      # Filtering out the listings that were outliers
      filter(price < quantile(price, 0.75) + 1.5 * IQR(price)) %>%
      # Adjusting the longitude values slightly as the locations of rental listings
      # were deviating slightly to the left (spatial correction for visualization)
      mutate(lon = lon + 0.03) %>%
      # Omitting NA values
      na.omit()

    # Printing a summary of the data frame containing rental prices and coordinates
    rentals_and_coords %>% 
      summarise(`min price` = min(price), `max price`= max(price),
                `min lat` = min(lat), `max lat` = max(lat),
                `min lon` = min(lon), `max lon` = max(lon)
                )

    ## # A tibble: 1 × 6
    ##   `min price` `max price` `min lat` `max lat` `min lon` `max lon`
    ##         <dbl>       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1         400        5850      37.0      38.8     -123.     -121.

### Rental Prices per Square Feet per Sub-Region

The columns `county`, `price`, `size` is selected from `rent` to plot
the rental prices per square feet per sub-region from year 2000 to year
2018. The smallest rental is 90 square feet in San Francisco which is
roughly a small 1 room apartment while the largest rental is 2246 square
feet in North Bay which is roughly a large 4 room house.

    # Setting seed for reproducibility 
    set.seed(72325)
    rental_prices_size <- rent %>% 
      # Excluding county not part of the Bay Area
      filter(county != "santa cruz") %>%
      # Use only data from 2000 to 2018
      filter(2000<=year & year<=2018) %>%
      # Grouping the counties into their respective regions
      mutate(sub_region = case_when(county == "alameda" | county == "contra costa" ~ "East Bay",
                         county == "sonoma" | county == "napa" | county == "marin" | county == "solano" ~ "North Bay",
                         county == "san mateo" ~ "San Francisco Peninsula",
                         county == "san francisco" ~ "San Francisco",
                         county == "santa clara" ~ "South Bay")) %>%
      # Selection of relevant columns from the rent dataset for visualization 
      select(sub_region, price, sqft) %>%
      # Omitting NA values
      na.omit() %>%
      # Exclusion of outliers as several outliers had extreme values that negatively 
      # impacted quality of visualizations
      filter(sqft < quantile(sqft, 0.75) + 1.5 * IQR(sqft)) %>%
      # Sampling of 1000 records to prevent cluttering of points in scatter plot
      sample_n(1000)

    # Printing a summary of the data frame containing rental prices and sizes 
    # of rental listings in square feet
    rental_prices_size %>% 
      group_by(sub_region) %>% 
      summarise(`min price` = min(price), `max price` = max(price),
                `min size` = min(sqft), `max size`= max(sqft)
                ) %>% 
      ungroup()

    ## # A tibble: 5 × 5
    ##   sub_region              `min price` `max price` `min size` `max size`
    ##   <chr>                         <dbl>       <dbl>      <dbl>      <dbl>
    ## 1 East Bay                        500        4500        150       2200
    ## 2 North Bay                       590        6000        220       2246
    ## 3 San Francisco                   600        9495         90       2000
    ## 4 San Francisco Peninsula         667        4750        425       1950
    ## 5 South Bay                       600        9500        240       2200

## Data Visualizations

### Plotting Average Housing Rental Prices in the Bay Area (2000-2018)

    annual_rent_price_plot = average_rental_prices %>% 
      ggplot(aes(x = year, y = rental_average, colour = sub_region)) +
      geom_line(linewidth = 1) +
      theme_minimal() +
      scale_colour_viridis_d() +
      ylim(0, max(average_rental_prices$rental_average)) +
      scale_x_continuous(breaks = seq(min(average_rental_prices$year), max(average_rental_prices$year), 2)) +
      geom_vline(xintercept = 2007, linetype = 2, color = "red", size = 0.6) +
      geom_vline(xintercept = 2010, linetype = 2, color = "indianred4", size = 0.6) +
      labs(
        color = "Sub-Region",
        x = "Year",
        y = "Average Rental Price (USD)",
        title = "Average Housing Rental Prices in the Bay Area (2000-2018)",
        subtitle = "Red vertical lines mark 2007-2010 Subprime Mortgage Crisis",
        caption = "*San Francisco Peninsula starts from 2001"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

    annual_rent_price_plot

<img src="/figure-markdown_strict/Visualization 1-1.png" style="display: block; margin: auto;" />

This visualization illustrates the changes in **average rental prices**
of rental listings grouped to their respective **sub-regions** against a
**time period** between 2000 to 2018. Since the type of information
represented here is time-series data, a line chart is useful to portray
how a continuous variable (in this case, average rental price) changes
over time in a simple yet informative manner.

Additionally, we are also able to highlight the occurrence(s) of
economic events over time and consequently, their influences on average
rental prices. Further analysis on these impacts on rental prices will
be explored in the ‘Discussion’ section.

### Plotting Prices of Rental Listings and their Locations

    point_colors <- colorRampPalette(c("goldenrod1", "purple4"))(n = 50)

    map_plot <- ggplot(data = county_coords) +
      theme_bw() +
      scale_color_gradient(low = point_colors[1], high = point_colors[50]) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 5))) +
      geom_polygon(aes(x = long, y = lat, group = subregion, fill = sub_region),
                   color = "black", alpha = 0.4) +
      scale_fill_manual(values = c("cornflowerblue","cadetblue1","aquamarine3","darkturquoise","aliceblue")) +
      geom_point(data = rentals_and_coords, aes(x = lon, y = lat, color = price), 
                 shape = 16, alpha = 0.5, size = 0.8) +
      labs(title = "Regional Map of Sub-Regions and their Respective Rental Listings",
           x = "Longtitude", y = "Latitude",
           color = "Rental Price (USD)", fill = "Sub-Region")

    map_plot

<img src="/figure-markdown_strict/Visualization 2-1.png" style="display: block; margin: auto;" />

This geospatial map visualization illustrates the variations in **rental
prices** of individual rental listings based on their exact
**geographical coordinates (latitude and longitude)**. A geospatical
visualization is useful in this case, as it plots data against a
background map of California, which might allow for certain trends to be
identified that may not be apparent in a tabular or non-spatial
presentation.

### Plotting the Relationship between Size of Rental Units and Rental Prices

    sqft_price_plot <- ggplot(rental_prices_size, aes(x = sqft, y = price, color = sub_region)) +
      geom_point(alpha = 0.3) +
      geom_smooth(aes(color = sub_region), method = "lm", se = FALSE, linetype = "solid", linewidth = 1) +
      labs(
        color = "Sub-Region",
        x = "Size of Rental Unit (square feet)",
        y = "Rental Price (USD)",
        title = "Relationship Between Size and Price of Rental Listings") +
      theme_minimal() +
      scale_colour_viridis_d() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

    sqft_price_plot

<img src="/figure-markdown_strict/Visualization 3-1.png" style="display: block; margin: auto;" />

This visualization investigates on the relationships between the
**rental prices** and **interior area in square feet** (an indicator for
size of the unit) of rental listings, grouped according to their
respective **sub-regions**. Since rental prices and interior area are
both continuous variables, a scatter plot is useful for illustrating how
they are related to each other. Additionally, a scatter plot can provide
hints to the linearity of the relationship between variables, through
the use of a regression line.

## Discussion

Through the visualizations, we are able to conclude that there are
strong correlations between the three explored factors (time, location,
and size) and rental prices in San Francisco.

### Average Housing Rental Prices in the Bay Area (2000-2018)

For the first visualization, rent prices have consistently increasing
over time, with a notable dip in 2007 due to the trigger of the subprime
mortgage crisis. Demand for housing has remained suppressed (as
reflected in its price) in the years to come due to the resulting global
recession. Interestingly, rental prices have outpaced inflation rates;
where Californian rent has nearly doubled in contrast to a cumulative
inflation of 45% in the United States between 2000 - 2018 (U.S. Bureau
of Labor Statistics, 2023) Sadly, further research into the causal
determinants for this economic phenomenon is outside the scope of this
dataset and course.

### Prices of Rental Listings and their Locations

Other determinants such as location and size of units were explored as
well. From the second visualization, we see that rent is notably higher
within dense residential areas such as San Francisco, San Francisco
Peninsula and South Bay regions, as compared to the more distant
suburban regions. From this, we can determine that the location, and
moreover proximity to city centers play a crucial role in influencing
rental prices. This is aligned with the literature, where Barton (2011)
found that land was a significant determinant in rent prices in
California housing.

We have also noticed some outliers which have coordinates located in the
sea, suggesting errant listings. These data were not removed as they
possess minimal disruption to the visualization.

### Relationship between Size of Rental Units and Rental Prices

Finally, from the third visualization, we see that rental prices also
increase with the size of rental units for all sub-regions. A noticeable
difference is the rate of increase, where San Francisco has the steepest
gradient compared to other sub-regions. From this, we can infer that
size is a key determinant of rental prices in all sub-regions and has a
larger effect on rental prices in San Francisco specifically.

## References

Barton, S. E. (2011). Land rent and housing policy: A case study of the
San Francisco Bay Area rental housing market. *American Journal of
Economics and Sociology*, 70(4), 845-873.

Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts,
2000-2018. Retrieved from
<https://github.com/katepennington/historic_bay_area_craigslist_housing_posts/blob/master/clean_2000_2018.csv.zip>

United States Bureau of Labor Statistics (2023). *Consumer Price Index*.
Retrieved from: <https://www.bls.gov/cpi/>
