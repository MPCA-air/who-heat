#setwd("X:/projects/who_heat/web/who-heat/WHO_HETK")
.rdata<<-list()
#if(input$dataSource == "HETK"){
.rdata[['focus_country']]<<-"Armenia"
.rdata[['focus_indicator']]<<-c("carep")
.rdata[['focus_dimension']]<<-c("Sex")
.rdata[['focus_year']]<<-c(2010)
.rdata[['all_years']]<<-c(2000, 2005, 2010)

.rdata[['maindata']]<<-readRDS("data/maindata.RDS")
.rdata[['inequals']]<<-readRDS("data/inequals.RDS")
.rdata[['nationaldata']]<<-readRDS("data/nationaldata.RDS")
.rdata[['years']]<<-readRDS("data/years.RDS")
.rdata[['strata']]<<-readRDS("data/strata.RDS")
.rdata[['all_table_variables']] <<- c("Country" = 'Country', 
                                      "Year" = "Year", 
                                      "Data source" = "Data source", 
                                      "Health indicator" = "Health indicator", 
                                      "Inequality dimension" = "Inequality dimension",
                                      "Subgroup" = "Subgroup",
                                      "Estimate" = "Estimate",
                                      "Lower 95%CI"  = "Lower 95%CI",
                                      "Upper 95%CI"  = "Upper 95%CI",
                                      "Population share %"   = "Population share %",
                                      "National estimate"    = "National estimate",
                                      "Flag" = "Flag")







.rdata[['focus_data_source']]<<-"All"
.rdata[['mostrecent']]<<-FALSE


.rdata[['focus_inequal_type']]<<-'rd'

.rdata[['benchmark_countries']]<<-c("Afghanistan", "Belize", "Liberia")
.rdata[['countrynames']]<<-readRDS("data/countrynames.RDS")


.rdata[['all_countries']]<<-.rdata[['countrynames']]$country
.rdata[['benchmark_country_list']]<<-.rdata[['all_countries']]
.rdata[['income_groups']] <<- sort(unique(.rdata[['countrynames']]$wbincome2014_4cat))
.rdata[['who_regions']] <<- sort(unique(.rdata[['countrynames']]$whoreg6_name))







.rdata[['focus_table_variables']]<-c("Country", "Year", "Data source", "Health indicator", "Inequality dimension", 
                                     "Subgroup", "Estimate", "Population share %", "Lower 95%CI", "Upper 95%CI")


.rdata[['unrankable_dimensions']] <<- c("Sex", "Geographical region")
.rdata[['rankable_dimensions']]<<- c("Economic status", "Mother's education", "Place of residence")

.rdata[['summary_measures_all']]<<-sort( c("Range difference (RD)" = "rd",    
                                           "Between-Group variance (BGV)" = "bgv", 
                                           "Mean difference from best performing subgroup (MDB)" = "mdb",
                                           "Mean difference from mean (MDM)" = "mdm", 
                                           "Absolute concentration index (ACI)" = "aci",  
                                           "Slope index of inequality (SII)" = "sii",
                                           "Range Ratio (RR)" = "rr", 
                                           "Index of disparity (IDis)" = "idis", 
                                           "Relative concentration index (RCI)" = "rci", 
                                           "Relative index of inequality (RII)" = "rii",
                                           "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm", 
                                           "Theil Index (TI)" = "ti", 
                                           "Population attributable risk (PAR)" = "par",
                                           "Population attributable risk % (PAF)" = "paf", 
                                           "Mean log deviation (MLD)" = "mld"))

.rdata[['summary_measures_rank']]<<- sort(c("Slope index of inequality (SII)" = "sii", 
                                            "Absolute Concentration Index (ACI)" = "aci", 
                                            "Relative concentration index (RCI)" = "rci",
                                            "Relative index of inequality (RII)" = "rii",
                                            "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm"))



.rdata[['summary_measures_unrank']]<<-sort(c("Between-Group Variance (BGV)" = "bgv", 
                                             "Index of disparity (IDis)" = "idis", 
                                             "Mean difference from best performing subgroup (MDB)" = "mdb",
                                             "Mean difference from mean (MDM)" = "mdm", 
                                             "Population attributable risk % (PAF)" = "paf", 
                                             "Population attributable risk (PAR)" = "par", 
                                             "Range difference (RD)" = "rd",
                                             "Relative index of inequality (RII)" = "rii", 
                                             "Range Ratio (RR)" = "rr", 
                                             "Theil Index (TI)" = "ti",
                                             "Mean log deviation (MLD)" = "mld"))




.rdata[['equity_dimensions']] <<- c("Economic status", "Geographic region", "Mother's education",
                                    "Place of residence","Sex")


if(!is.null(.rdata[['strata']])){
  tmpindic <- select(.rdata[['strata']], indic_name, indic) %>% distinct
  #print(head(tmpindic))
  namestmpindic <- tmpindic$indic_name
  tmpindic <- tmpindic$indic
  names(tmpindic)<-namestmpindic
}

.rdata[['full_indicators']] <<- tmpindic








