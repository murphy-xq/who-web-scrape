who-web-scrape
================

# Disease incidence and immunization coverage: An exercise in data scraping and visualization 🗺 👩‍⚕ 🔍

## Motivation

WHO makes a plethora of immunization-related datasets available
[online](http://www.who.int/immunization/monitoring_surveillance/data/en/);
however, as typically encountered when working with public datasets from
international organizations, data are presented as tricky html tables or
machine-unfriendly spreadsheets 🤖 💥

Here we will use `rvest`, `readxl`, and `tidyverse` to scrape WHO
measles data, clean and compile a tidy dataset, and create an
interactive map.

### Web scrape

As you see below, WHO presents incidence data by country from 1980 to
2017 for 11 different diseases in html tales or in a spreadsheet.

![](images/who_measles_incidence.png)

By inspecting the webpage in the browser ([see here for a brief
tutorial](https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/)),
we see that the third table element contains the data. `rvest` functions
`html_nodes` and `html_table` allow us to scrape the data from that
table element and pull into a dataframe.

``` r
# parse the html
base_url <- "http://apps.who.int/immunization_monitoring/globalsummary/timeseries/tsincidence"
html <- read_html(str_c(base_url,"measles"))

# grab the third table node and extract the table contents
measles_df <- html %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = TRUE, trim = TRUE, header = TRUE) %>%
  as_data_frame()

# inspect the output
measles_df
#> # A tibble: 195 x 27
#>    `Click on a cou… `2017` `2016` `2015` `2014` `2013` `2012` `2011` `2010` `2009` `2008` `2007`
#>    <chr>            <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#>  1 Afghanistan      1'511  638    1'154  492    430    2'787  3'013  1'989  2'861  1'599  1'141 
#>  2 Albania          12     17     ""     ""     0      9      28     10     0      ""     22    
#>  3 Algeria          112    41     63     0      25     18     112    103    107    217    0     
#>  4 Andorra          0      0      ""     ""     0      0      0      0      0      0      0     
#>  5 Angola           29     53     119    11'699 8'523  4'458  1'449  1'190  2'807  265    1'014 
#>  6 Antigua and Bar… 0      0      0      0      0      0      0      0      0      0      0     
#>  7 Argentina        3      0      0      1      0      2      3      17     3      0      0     
#>  8 Armenia          1      2      33     13     10     0      0      2      0      0      1     
#>  9 Australia        81     99     74     340    158    199    190    70     104    65     11    
#> 10 Austria          95     27     309    117    ""     36     68     52     49     448    20    
#> # ... with 185 more rows, and 15 more variables: `2006` <chr>, `2005` <chr>, `2004` <chr>,
#> #   `2003` <chr>, `2002` <chr>, `2001` <chr>, `2000` <chr>, `1999` <chr>, `1998` <chr>,
#> #   `1997` <chr>, `1996` <chr>, `1995` <chr>, `1990` <chr>, `1985` <chr>, `1980` <chr>
```

#### Clean

Now that we have a dataframe of the reported measles cases, we want to
do some cleaning. First, there are quite a lot of blanks in the table so
we want to ensure those are explicit NA values. Next, we want to remove
the thousands separator which in this particular table is curiously an
apostrophe. Lastly, we want to convert the data type for cases and years
from `<chr>` to `<int>`.

``` r
measles_clean <- measles_df %>% 
  na_if("") %>%                      # convert blanks to explicit NAs
  rename(country = 1) %>%            # rename first column
  gather(year, cases, -country) %>%  # reshape df
  mutate_at(vars(-country), funs(str_replace_all(., "'", ""))) %>%  # remove apostrophe thousands separator
  mutate_at(vars(-country), funs(as.integer))                       # convert cases to integer

# Let's abstract this into a function for use later
clean_web_table <- function(df) {
  df %>%
    as_data_frame() %>% 
    na_if("") %>%            
    rename(country = 1) %>%  
    gather(year, cases, -country) %>%
    mutate_at(vars(-country), funs(str_replace_all(., "'", ""))) %>%
    mutate_at(vars(-country), funs(as.integer))
}
```

#### Scrape, clean, and compile all disease incidence tables

Now that we have succesfully scraped the measles data, let’s adapt our
approach so that we can scrape all available data. To start, we build a
vector containing each of the available disease tables. Using `purrr`,
we parse each page’s html content and table as before and use `map_df`
and `clean_web_table` to pull the output into a tidy
dataframe.

``` r
diseases <- c("diphtheria","japenc","measles","mumps","pertussis","rubella","crs","ntetanus","ttetanus","yfever")  # polio
base_url <- "http://apps.who.int/immunization_monitoring/globalsummary/timeseries/tsincidence"

full_scrapes <- diseases %>% 
  set_names() %>%             # set vector of disease names needed for id variable 
  map_df(
    ~str_c(base_url, .x) %>%  # build urls
    read_html() %>%           # parse html per page
    html_nodes("table") %>%   # specify table elements
    .[[3]] %>%                # grab third table
    html_table(fill = TRUE, trim = TRUE, header = TRUE) %>%  # process third table per page
    clean_web_table(),        # clean up dfs 
  .id = "disease"             # name the id column
  )

full_scrapes
#> # A tibble: 43,875 x 4
#>    disease    country              year cases
#>    <chr>      <chr>               <int> <int>
#>  1 diphtheria Afghanistan          2017     1
#>  2 diphtheria Albania              2017     0
#>  3 diphtheria Algeria              2017     0
#>  4 diphtheria Andorra              2017     0
#>  5 diphtheria Angola               2017    NA
#>  6 diphtheria Antigua and Barbuda  2017     0
#>  7 diphtheria Argentina            2017     0
#>  8 diphtheria Armenia              2017     0
#>  9 diphtheria Australia            2017     8
#> 10 diphtheria Austria              2017     0
#> # ... with 43,865 more rows
```

### Process spreadsheet

Spreadsheets encountered in the wild are often filled with non-tabular
data containing row headers, multi-level headers, formatting as data,
and/or multiple tables represented across the same sheet ([see this
article for spreadsheet best
practices](https://amstat.tandfonline.com/doi/full/10.1080/00031305.2017.1375989#.W3mcApNKjeQ)).
And, dealing with public datasets from large internatoinal organizations
is no exception. However, we have an expcetion here, in that the dataset
made availble by WHO is fairly straightforward to load and clean.

``` r
# read_excel makes processing spreadsheets a breeze
read_excel(here("data","incidence_series.xls"), sheet = "Measles")
#> # A tibble: 194 x 42
#>    WHO_REGION ISO_code Cname Disease `2017` `2016` `2015` `2014` `2013` `2012` `2011` `2010` `2009`
#>    <chr>      <chr>    <chr> <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 EMR        AFG      Afgh… measles   1511    638   1154    492    430   2787   3013   1989   2861
#>  2 EUR        ALB      Alba… measles     12     17     NA     NA      0      9     28     10      0
#>  3 AFR        DZA      Alge… measles    112     41     63      0     25     18    112    103    107
#>  4 EUR        AND      Ando… measles      0      0     NA     NA      0      0      0      0      0
#>  5 AFR        AGO      Ango… measles     29     53    119  11699   8523   4458   1449   1190   2807
#>  6 AMR        ATG      Anti… measles      0      0      0      0      0      0      0      0      0
#>  7 AMR        ARG      Arge… measles      3      0      0      1      0      2      3     17      3
#>  8 EUR        ARM      Arme… measles      1      2     33     13     10      0      0      2      0
#>  9 WPR        AUS      Aust… measles     81     99     74    340    158    199    190     70    104
#> 10 EUR        AUT      Aust… measles     95     27    309    117     NA     36     68     52     49
#> # ... with 184 more rows, and 29 more variables: `2008` <dbl>, `2007` <dbl>, `2006` <dbl>,
#> #   `2005` <dbl>, `2004` <dbl>, `2003` <dbl>, `2002` <dbl>, `2001` <dbl>, `2000` <dbl>,
#> #   `1999` <dbl>, `1998` <dbl>, `1997` <dbl>, `1996` <dbl>, `1995` <dbl>, `1994` <dbl>,
#> #   `1993` <dbl>, `1992` <dbl>, `1991` <dbl>, `1990` <dbl>, `1989` <dbl>, `1988` <dbl>,
#> #   `1987` <dbl>, `1986` <dbl>, `1985` <dbl>, `1984` <dbl>, `1983` <dbl>, `1982` <dbl>,
#> #   `1981` <dbl>, `1980` <dbl>

read_excel(here("data","incidence_series.xls"), sheet = "Measles") %>% 
  gather(year, cases, -c(1:4)) %>%
  na_if("") %>%
  rename(country = Cname) %>%  
  clean_names() %>% 
  mutate(year = as.integer(year)) %>% 
  select(-who_region)
#> # A tibble: 7,372 x 5
#>    iso_code country             disease  year cases
#>    <chr>    <chr>               <chr>   <int> <dbl>
#>  1 AFG      Afghanistan         measles  2017  1511
#>  2 ALB      Albania             measles  2017    12
#>  3 DZA      Algeria             measles  2017   112
#>  4 AND      Andorra             measles  2017     0
#>  5 AGO      Angola              measles  2017    29
#>  6 ATG      Antigua and Barbuda measles  2017     0
#>  7 ARG      Argentina           measles  2017     3
#>  8 ARM      Armenia             measles  2017     1
#>  9 AUS      Australia           measles  2017    81
#> 10 AUT      Austria             measles  2017    95
#> # ... with 7,362 more rows
```

Again, we can iterate through each of the available tabs in the workbook
so that we pull together all disease data, reshape, clean, and compile a
tidy dataset. Here this can be swiftly accomplished using `readxl`,
`purrr`, and functions from `dplyr` and `tidyr`.

``` r
excel_path <- here("data", "incidence_series.xls")

excel_path %>% 
  excel_sheets() %>% 
  .[-c(1,length(.))] %>%  # drop sheets we don't care about ("Readme" and "Reg & Global Incidence")
  set_names() %>% 
  map_df(
    ~read_excel(path = excel_path, sheet = .x, trim_ws = TRUE) %>% 
      gather(year, cases, -c(1:4)) %>%
      na_if("") %>%
      rename(country = Cname) %>%
      clean_names() %>% 
      mutate(year = as.integer(year)) %>% 
      select(-who_region), 
    .id = "disease"
    )
#> # A tibble: 65,572 x 6
#>    disease iso_code country             disease  year cases
#>    <chr>   <chr>    <chr>               <chr>   <int> <dbl>
#>  1 CRS     AFG      Afghanistan         CRS      2017    NA
#>  2 CRS     ALB      Albania             CRS      2017     0
#>  3 CRS     DZA      Algeria             CRS      2017    NA
#>  4 CRS     AND      Andorra             CRS      2017     0
#>  5 CRS     AGO      Angola              CRS      2017    NA
#>  6 CRS     ATG      Antigua and Barbuda CRS      2017     0
#>  7 CRS     ARG      Argentina           CRS      2017     0
#>  8 CRS     ARM      Armenia             CRS      2017     0
#>  9 CRS     AUS      Australia           CRS      2017     0
#> 10 CRS     AUT      Austria             CRS      2017     0
#> # ... with 65,562 more rows
```

#### Clean

### Compile dataframe and visualize
