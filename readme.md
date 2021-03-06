
### Background

`nazihuntR` : Tools to explore ZAŁOGA SS KL AUSCHWITZ data-store which contains information about Nazis who served at [Auschwitz](https://en.wikipedia.org/wiki/Auschwitz_concentration_camp)

Ref: <http://pamiec.pl/>

In January 2017 the [Krakow Rememberance Institute](https://en.wikipedia.org/wiki/Institute_of_National_Remembrance) published documents about the nearly 9,000 Nazi's who worked at Auschwitz concentration camp. This package builds an API wrapper around this data. In addition, the package utizies [Ryan Hafen](http://ryanhafen.com/blog/)'s incredible [trelliscopeJS](https://github.com/hafen/trelliscopejs) package to allow for interactive exploration of the data.

The packages allows you scrape the data real-time using the `get_nazis` function, this can take up to an hour depending on your internet speed. As an alternative the `import_nazis` function provides real-time access to the data as of February 1st, 2017.

The package also provides links to documents, typically in German or Polish, for any Nazi's that have public records.

### Package functions

The following functions are implemented:

-   `get_nazis`: scrapes current data
-   `import_nazis`: imports cached data
-   `visualize_nazis`: visualizes a trelliscope of the daa
-   `tidy_counts`: tidy multiple count columns

### Installation

``` r
devtools::install_github("abresler/nazihuntR") 
devtools::install_github("hafen/trelliscopejs") # required for interactive analysis
```

### Usage

``` r
library(nazihuntR)
library(dplyr)
get_nazis(parse_bios = TRUE, tidy_columns = FALSE, return_message = TRUE) ## scrapes
df_all <- 
nazihuntR::import_nazi_data(table_name = 'all') # returns cached table of all nazi data

df_names  <- 
nazihuntR::import_nazi_data(table_name = 'names') # returns cached table of all nazi names

df_items  <- 
nazihuntR::import_nazi_data(table_name = 'items') # returns cached table of all nazi fields with multiple items

nazihuntR::import_nazis(table_name = 'names', visualize_results = TRUE) # provides a trelliscope view

nazihuntR::import_nazis(table_name = 'items') %>% 
  visualize_nazis(title = "Nazis With Multiple Items")
```

#### Coming Soon

-   Full tidying of the data
-   Transalations
-   OCR of documents
-   Image analysis tools
