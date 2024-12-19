Working repo for companion app to Nature Medicine paper

To run, check out directory then build app datasets with data_raw/make_app_data.R\
App data are (large) excluded from version control via .gitignore\
When inst/extdata has the city\_ country\_ region\_ folders run with:

`golem::document_and_reload(); run_app()`

[**Ideas**]{.underline}

-   fixed colour scale and legend ? (visualise change through time)

**Help with word/UI**

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
