
#observe({

#if(is.null(input$dataSource)) return()
.rdata<<-list()
#if(input$dataSource == "HETK"){

.rdata[['maindata']]<<-readRDS("data/maindata.RDS")
.rdata[['inequals']]<<-readRDS("data/inequals.RDS")
.rdata[['nationaldata']]<<-readRDS("data/nationaldata.RDS")
.rdata[['countrynames']]<<-readRDS("data/countrynames.RDS")
.rdata[['years']]<<-readRDS("data/years.RDS")
#print(head(.rdata[['countrynames']]))
#  }

.rdata[['all_countries']]<<-.rdata[['countrynames']]$country

.rdata[['focus_data_source']]<<-"All"
.rdata[['mostrecent']]<<-FALSE

.rdata[['focus_country']]<<-"Armenia"
.rdata[['focus_indicator']]<<-c("carep")
.rdata[['focus_dimension']]<<-c("Sex")
.rdata[['focus_year']]<<-c(2010)
.rdata[['all_years']]<<-c(2000, 2005, 2010)

.rdata[['focus_inequal_type']]<<-'rd'

.rdata[['benchmark_countries']]<<-c("Afghanistan", "Belize", "Liberia")


.rdata[['all_table_variables']] <- c("Country" = 'Country', 
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
                                     "Flag" = "flag")

.rdata[['focus_table_variables']]<-c("Country", "Year", "Source", "Health indicator", "Inequality dimension", 
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
.rdata[['core_indicators']]<<-c("Antenatal care coverage - at least one visit (2/3 years) (%)" = "anc1",
                                "Antenatal care coverage - at least one visit (5 years) (%)" = "anc15",
                                "Antenatal care coverage - at least four visits (2/3 years) (%)" = "anc4",
                                "Antenatal care coverage - at least four visits (5 years) (%)" = "anc45",
                                #                     "Antibiotic treatment in children with ARI symptoms (%)" = "antip",
                                #                     "Adolescent fertility rate (per 1000 girls aged 15-19 years)" = "asfr1",
                                "BCG immunization coverage among 1-year-olds (%)" = "bcgv",
                                "Children (<5 years) with ARI symptoms taken to facility (%)" = "carep",
                                #                     "Co-coverage (less than 3 interventions)" = "cc31",
                                #                     "Co-coverage (less than 3 interventions)" = "cc34",
                                #                     "Co-coverage (6 or more interventions)" = "cc61",
                                #                     "Co-coverage (6 or more interventions)" = "cc64",
                                #                     "Composite coverage index" = "cci",
                                #                     "Child mortality rate (per 1000 live births)" = "cmr",
                                "Contraceptive prevalence - modern methods (%)" = "cpmo",
                                "Contraceptive prevalence - modern and traditional methods (%)" = "cpmt",
                                "Births by caesarean section (2/3 years) (%)" = "csection",
                                "Births by caesarean section (5 years) (%)" = "csection5",
                                "DTP3 immunization coverage among 1-year-olds (%)" = "dptv",
                                "Early initiation of breastfeeding (2/3 years) (%)" = "ebreast",
                                "Early initiation of breastfeeding (5 years) (%)" = "ebreast5",
                                "Family planning needs satisfied (%)" = "fps",
                                "Full immunization coverage among 1-year-olds (%)" = "fullv",
                                #                     "Hib immunization coverage among 1-year-olds (%)" = "hib3v",
                                #                     "Infant mortality rate (deaths per 1000 live births)" = "imr",
                                #                     "Intermittent preventive treatment for malaria during pregnancy (%)" = "iptp",
                                "Children (<5 years) sleeping under insecticide-treated nets (%)" = "itnch",
                                "Pregnant women sleeping under insecticide-treated nets (%)" = "itnwm",
                                "Measles immunization coverage among 1-year-olds (%)" = "mslv",
                                #                     "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
                                #                     "Prevalence of obesity in non-pregnant women (15-49 years) (%)" = "obesewm",
                                #                     "Children (<5 years) with diarrhoea receiving ORS (%)" = "ors",
                                "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)" = "ort",
                                #                     "Children (<3 years) overweight (%)" = "overwgt3",
                                #                     "Children (<5 years) overweight (%)" = "overwgt5",
                                #                     "Postnatal care for babies (%)" = "pncall",
                                #                     "Postnatal care for babies born outside a health facility (%)" = "pnchome",
                                #                     "Postnatal care for women (%)" = "pncwm",
                                #                     "Postnatal mortality rate (per 1000 live births)" = "pnmr",
                                "Polio immunization coverage among 1-year-olds (%)" = "poliov",
                                #                     "Population using improved sanitation facilities (%)" = "sanit",
                                "Births attended by skilled health personnel (2/3 years) (%)" = "sba",
                                "Births attended by skilled health personnel (5 years) (%)" = "sba5",
                                #                     "Children (<3 years) stunted (%)" = "stunt3",
                                #                     "Children (<5 years) stunted (%)" = "stunt5",
                                #                     "Total fertility rate (per woman)" = "tfr",
                                #                     "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
                                #                     "Children (<3 years) underweight (%)" = "uweight3",
                                #                     "Children (<5 years) underweight (%)" = "uweight5",
                                "Children (6-59 months) who received vitamin A supplementation (%)" = "vita")
#                     "Children (<3 years) wasted (%)" = "wast3",
#                     "Children (<5 years) wasted (%)" = "wast5",
#                     "Population using improved drinking water sources (%)" = "water")

.rdata[['full_indicators']] <<-c("Antenatal care coverage - at least one visit (2/3 years) (%)" = "anc1",
                                 "Antenatal care coverage - at least one visit (5 years) (%)" = "anc15",
                                 "Antenatal care coverage - at least four visits (2/3 years) (%)" = "anc4",
                                 "Antenatal care coverage - at least four visits (5 years) (%)" = "anc45",
                                 #                     "Antibiotic treatment in children with ARI symptoms (%)" = "antip",
                                 #                     "Adolescent fertility rate (per 1000 girls aged 15-19 years)" = "asfr1",
                                 "BCG immunization coverage among 1-year-olds (%)" = "bcgv",
                                 "Children (<5 years) with ARI symptoms taken to facility (%)" = "carep",
                                 #                     "Co-coverage (less than 3 interventions)" = "cc31",
                                 #                     "Co-coverage (less than 3 interventions)" = "cc34",
                                 #                     "Co-coverage (6 or more interventions)" = "cc61",
                                 #                     "Co-coverage (6 or more interventions)" = "cc64",
                                 #                     "Composite coverage index" = "cci",
                                 #                     "Child mortality rate (per 1000 live births)" = "cmr",
                                 "Contraceptive prevalence - modern methods (%)" = "cpmo",
                                 "Contraceptive prevalence - modern and traditional methods (%)" = "cpmt",
                                 "Births by caesarean section (2/3 years) (%)" = "csection",
                                 "Births by caesarean section (5 years) (%)" = "csection5",
                                 "DTP3 immunization coverage among 1-year-olds (%)" = "dptv",
                                 "Early initiation of breastfeeding (2/3 years) (%)" = "ebreast",
                                 "Early initiation of breastfeeding (5 years) (%)" = "ebreast5",
                                 "Family planning needs satisfied (%)" = "fps",
                                 "Full immunization coverage among 1-year-olds (%)" = "fullv",
                                 #                     "Hib immunization coverage among 1-year-olds (%)" = "hib3v",
                                 #                     "Infant mortality rate (deaths per 1000 live births)" = "imr",
                                 #                     "Intermittent preventive treatment for malaria during pregnancy (%)" = "iptp",
                                 "Children (<5 years) sleeping under insecticide-treated nets (%)" = "itnch",
                                 "Pregnant women sleeping under insecticide-treated nets (%)" = "itnwm",
                                 "Measles immunization coverage among 1-year-olds (%)" = "mslv",
                                 #                     "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
                                 #                     "Prevalence of obesity in non-pregnant women (15-49 years) (%)" = "obesewm",
                                 #                     "Children (<5 years) with diarrhoea receiving ORS (%)" = "ors",
                                 "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)" = "ort",
                                 #                     "Children (<3 years) overweight (%)" = "overwgt3",
                                 #                     "Children (<5 years) overweight (%)" = "overwgt5",
                                 #                     "Postnatal care for babies (%)" = "pncall",
                                 #                     "Postnatal care for babies born outside a health facility (%)" = "pnchome",
                                 #                     "Postnatal care for women (%)" = "pncwm",
                                 #                     "Postnatal mortality rate (per 1000 live births)" = "pnmr",
                                 "Polio immunization coverage among 1-year-olds (%)" = "poliov",
                                 #                     "Population using improved sanitation facilities (%)" = "sanit",
                                 "Births attended by skilled health personnel (2/3 years) (%)" = "sba",
                                 "Births attended by skilled health personnel (5 years) (%)" = "sba5",
                                 #                     "Children (<3 years) stunted (%)" = "stunt3",
                                 #                     "Children (<5 years) stunted (%)" = "stunt5",
                                 #                     "Total fertility rate (per woman)" = "tfr",
                                 #                     "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
                                 #                     "Children (<3 years) underweight (%)" = "uweight3",
                                 #                     "Children (<5 years) underweight (%)" = "uweight5",
                                 "Children (6-59 months) who received vitamin A supplementation (%)" = "vita")
#                     "Children (<3 years) wasted (%)" = "wast3",
#                     "Children (<5 years) wasted (%)" = "wast5",
#                     "Population using improved drinking water sources (%)" = "water")


.rdata[['health_indicator_abbr']] <<- c("anc1" = "Antenatal care coverage - at least one visit (2/3 years) (%)",
                                        "anc15" = "Antenatal care coverage - at least one visit (5 years) (%)",
                                        "anc4" = "Antenatal care coverage - at least four visits (2/3 years) (%)",
                                        "anc45" = "Antenatal care coverage - at least four visits (5 years) (%)",
                                        #                                  "antip" = "Antibiotic treatment in children with ARI symptoms (%)",
                                        #                                  "asfr1" = "Adolescent fertility rate (per 1000 girls aged 15-19 years)",
                                        "bcgv" = "BCG immunization coverage among 1-year-olds (%)",
                                        "carep" = "Children (<5 years) with ARI symptoms taken to facility (%)",
                                        #                                   "cc31" = "Co-coverage (less than 3 interventions)",
                                        #                                   "cc34" = "Co-coverage (less than 3 interventions)",
                                        #                                   "cc61" = "Co-coverage (6 or more interventions)",
                                        #                                   "cc64" = "Co-coverage (6 or more interventions)",
                                        #                                   "cci" = "Composite coverage index",
                                        #                                   "cmr" = "Child mortality rate (per 1000 live births)",
                                        "cpmo" = "Contraceptive prevalence - modern methods (%)",
                                        "cpmt" = "Contraceptive prevalence - modern and traditional methods (%)",
                                        "csection" = "Births by caesarean section (2/3 years) (%)",
                                        "csection5" = "Births by caesarean section (5 years) (%)",
                                        "dptv" = "DTP3 immunization coverage among 1-year-olds (%)",
                                        "ebreast" = "Early initiation of breastfeeding (2/3 years) (%)",
                                        "ebreast5" = "Early initiation of breastfeeding (5 years) (%)",
                                        "fps" = "Family planning needs satisfied (%)",
                                        "fullv" = "Full immunization coverage among 1-year-olds (%)",
                                        #                                   "hib3v" = "Hib immunization coverage among 1-year-olds (%)",
                                        #                                   "imr" = "Infant mortality rate (deaths per 1000 live births)",
                                        #                                   "iptp" = "Intermittent preventive treatment for malaria during pregnancy (%)",
                                        "itnch" = "Children (<5 years) sleeping under insecticide-treated nets (%)",
                                        "itnwm" = "Pregnant women sleeping under insecticide-treated nets (%)",
                                        "mslv" = "Measles immunization coverage among 1-year-olds (%)",
                                        #                                   "nmr" = "Neonatal mortality rate (deaths per 1000 live births)",
                                        #                                   "obesewm" = "Prevalence of obesity in non-pregnant women (15-49 years) (%)",
                                        #                                   "ors" = "Children (<5 years) with diarrhoea receiving ORS (%)",
                                        "ort" = "Children (<5 years) with diarrhoea receiving ORT and continued feeding (%)",
                                        #                                   "overwgt3" = "Children (<3 years) overweight (%)",
                                        #                                   "overwgt5" = "Children (<5 years) overweight (%)",
                                        #                                   "pncall" = "Postnatal care for babies (%)",
                                        #                                   "pnchome" = "Postnatal care for babies born outside a health facility (%)",
                                        #                                   "pncwm" = "Postnatal care for women (%)",
                                        #                                   "pnmr" = "Postnatal mortality rate (per 1000 live births)",
                                        "poliov" = "Polio immunization coverage among 1-year-olds (%)",
                                        #                                   "sanit" = "Population using improved sanitation facilities (%)",
                                        "sba" = "Births attended by skilled health personnel (2/3 years) (%)",
                                        "sba5" = "Births attended by skilled health personnel (5 years) (%)",
                                        #                                   "stunt3" = "Children (<3 years) stunted (%)",
                                        #                                   "stunt5" = "Children (<5 years) stunted (%)",
                                        #                                   "tfr" = "Total fertility rate (per woman)",
                                        #                                   "u5mr" = "Under-five mortality rate (deaths per 1000 live births)",
                                        #                                   "uweight3" = "Children (<3 years) underweight (%)",
                                        #                                   "uweight5" = "Children (<5 years) underweight (%)",
                                        "vita" = "Children (6-59 months) who received vitamin A supplementation (%)")
#                                   "wast3" = "Children (<3 years) wasted (%)",
#                                   "wast5" = "Children (<5 years) wasted (%)",
#                                   "water" = "Population using improved drinking water sources (%)")


#})

focusCountry_selector <- function(id){
  
  #print("in focus country")
  #print(.rdata[['all_countries']])
  
  if(is.null(.rdata[['all_countries']])) return()
  print(.rdata[['focus_country']])
  
  countries <- .rdata[['all_countries']]
  #countries <- getFilteredCountries()
  if(is.null(countries)){ countries <- c()}
  
  
  selectInput(id, 
              h5("Country:"), 
              choices  = countries, 
              multiple = FALSE, 
              selected = .rdata[['focus_country']])
  
  
}








