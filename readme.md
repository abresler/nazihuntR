
`nazihuntR` : Tools to explore ZAŁOGA SS KL AUSCHWITZ data-store which containing information the 10,000 soliders who worked at Auschwitz.

Ref: <http://pamiec.pl/>

### Installation

``` r
devtools::install_github("abresler/nazihuntR")
```

``` r
options(width=120)
```

### Usage

``` r
library(nazihuntR)
df_all <- 
nazihuntR::import_nazi_data(table_name = 'all') # returns cached table of all nazi data

df_names  <- 
nazihuntR::import_nazi_data(table_name = 'names') # returns cached table of all nazi names

df_items  <- 
nazihuntR::import_nazi_data(table_name = 'items') # returns cached table of all nazi fields with multiple items
```
