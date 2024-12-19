Working repo for companion app to Nature Medicine paper

To run, check out directory then build app datasets with data_raw/make_app_data.R\
App data are (large) excluded from version control via .gitignore\
When inst/extdata has the city\_ country\_ region\_ folders run with:

`golem::document_and_reload(); run_app()`

[**To add/fix/check**]{.underline}

-   colour scale and legend, set static for full data range, this would mean we see the change through time well

**Help with words and UI**


-   Improve readability of dynamic scenario label text
-   Add content in help page (how to use the app practically)
    -   Scenario/Component toggles
    -   Map clicks to see values
    -   ...
-   Add content in research page (what is the research behind and the meaning of the numbers)
    -   definitions
    -   links
    -   ...
-   Table tab UI, column layout/row layout

[**Fixed/Added/To check**]{.underline}

-   Improve readability of "no data" text
    - done
-   Improve readability of table usage text
    - done
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
-   Input filtering for table taken from mod_map_inputs (fill datatable filtering boxes)
    -   done: can load filtering terms on initialisation
    -   done: intial table filters brought from map inputs
-   table loading time, to load the largest of the tables it takes at least 60 seconds
  - informative loading indicators
  
