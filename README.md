Working repo for companion app to Nature Medicine paper  

To run, check out directory then build app datasets with data_raw/make_app_data.R  
App data are (large) excluded from version control via .gitignore  
When inst/extdata has the city_ country_ region_ folders run with:  

`golem::document_and_reload(); run_app()`

To add ?  

- help page
- research page/ glossary


To check 

- scenario label
- shorter/more even column names (table tab)
- colour scale and legend, set static for full data range, this would mean we see the change through time well
- carry over inputs for table filter from map inputs, maybe even fill in the datatable boxes...