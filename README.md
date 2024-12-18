Working repo for companion app to Nature Medicine paper

To run, check out directory then build app datasets with data_raw/make_app_data.R\
App data are (large) excluded from version control via .gitignore\
When inst/extdata has the city\_ country\_ region\_ folders run with:

`golem::document_and_reload(); run_app()`

[**To add/fix/check**]{.underline}

**Possibly not feasible**

-   colour scale and legend, set static for full data range, this would mean we see the change through time well\
-   Input filtering for table taken from mod_map_inputs (fill datatable filtering boxes): Altern. add same granularity of input for table than for map **-\>** first stage done: can load filtering terms on initialisation

**One big issue**

-   table loading time, to load the largest of the tables it takes at least 60 seconds, add loading bar?

**Help with words**

-   Improve readability of "no data" text
-   Improve readability of dynamic scenario label
-   Improve readability of table usage
-   Add content in help page (e.g items and how to use the app practically)
    -   Scenario/Component toggle
    -   Map clicks to see values
-   Add content in research page (what do the terms mean)
    -   glossary
    -   links
    -   ...

[**Fixed/Added/To check**]{.underline}

-   Label and dynamic scenario label(Component: to Including:, both(?))
    -   now "Including" and "Both" labels
-   Shift colour scale towards more intense to prevent cities being invisible with light background
    -   instead added circle outlines
-   Reverse direction of total scale, lighter better darker worse
    -   rev(pal)
-   Cities in local languages vs cities in English
    -   changes some cities to english, non-manual lookup for this difficult to find
-   Attributable number to Excess deaths
    -   done
-   Hide markers when no data
    -   clear colours and legend
-   update export button to download filtered dataset not just first page
    -   done
-   shorter/more even column names (table tab)
    -   on second thought they seem fine, implemented visibility button
-   "In development" warning
    -   added in app banner

