Working repo for companion app to Nature Medicine paper  

To run, check out directory then build app datasets with data_raw/make_app_data.R  
App data are (large) excluded from version control via .gitignore  
When inst/extdata has the city_ country_ region_ folders run with:  

`golem::document_and_reload(); run_app()`

To add ?  

- help page
- research page/glossary

To fix/check

- scenario label (Component: to Including:, both(?))
- shorter/more even column names (table tab)
- Attributable number to Excess deaths
- Shoft colour scale towards more intense to prevent cities being invisible with light background
- No data text, hide markers when no data
- Cities in local languages vs cities in english
- Input filtering for table taken from mod_map_inputs
- "In development" warning
