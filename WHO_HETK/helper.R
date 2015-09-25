# source('utils/GHO.R')
# source('utils/hetkdb.R')
# source('utils/get_filtered.R')  # Retrieve lists filtered by Year, Equity Dimension, or Health Indicator
# #source('utils/list_append.R')  # Append an item to a list
# #source('utils/sql_in.R')  # Take a character or numeric vector and return it in a form suitable for
#                                           # using after the 'IN' subcommand in a SQLite SELECT statement.
# #source('utils/is_rank.R')  # A function to test rank order
# #source('utils/not_in.R')  # A function that returns elements in vector1 that are not in vector2
# source('utils/health_indicator_list.R')  # Returns a list ov available health indicators
# #source('utils/useful.R')
# source('utils/findfixed.R')  # findFixedFactors function
# source('utils/plotter.R')  # The ggplot functions for rendering the graphs
# source('utils/comparisonCountries.R')  # The function to fecth benchmark country data
# 
# #----------Source files for the calculation of the inequality measures-------------#
# source('utils/inequal.R')
# source('utils/inequal_functions/midpointprop.R')
# source('utils/inequal_functions/aci.R')  # Absolute concentration index
# source('utils/inequal_functions/bgv.R')  # Between groups variance
# source('utils/inequal_functions/idis.R')  # Index of disparity
# source('utils/inequal_functions/riikm.R')  # Kunst Mackenbach Index
# source('utils/inequal_functions/mdb.R')  # Mean difference between best subgroup
# source('utils/inequal_functions/mdm.R')  # Meand difference between mean
# source('utils/inequal_functions/mld.R')  # Mean log deviation
# source('utils/inequal_functions/paf.R')  # Population attributable fraction
# source('utils/inequal_functions/par.R')  # population attributable risk (capitalised so as not to confuse par())
# source('utils/inequal_functions/rci.R')  # Relative concentration index
# source('utils/inequal_functions/rd.R')  # Rate (or Range) Difference
# source('utils/inequal_functions/rii.R')  # Relative Index of Inequality
# source('utils/inequal_functions/rr.R')  # Rate (or Range) Ratio
# source('utils/inequal_functions/sii.R')  # Slope index of inequality
# source('utils/inequal_functions/ti.R')  # Theil index


# unrankable <- c("Sex", "Geographical region")
# rankable <- c("Economic status", "Mother's education", "Place of residence")
# allSummaryMeasures <-sort( c("Range difference (RD)" = "rd",    
#                              "Between-Group variance (BGV)" = "bgv", 
#                              "Mean difference from best performing subgroup (MDB)" = "mdb",
#                              "Mean difference from mean (MDM)" = "mdm", 
#                              "Absolute concentration index (ACI)" = "aci",  
#                              "Slope index of inequality (SII)" = "sii",
#                              "Range Ratio (RR)" = "rr", 
#                              "Index of disparity (IDis)" = "idis", 
#                              "Relative concentration index (RCI)" = "rci", 
#                              "Relative index of inequality (RII)" = "rii",
#                              "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm", 
#                              "Theil Index (TI)" = "ti", 
#                              "Population attributable risk (PAR)" = "par",
#                              "Population attributable risk % (PAF)" = "paf", 
#                              "Mean log deviation (MLD)" = "mld"))
# 
# rankSummaryMeasures <- sort(c("Slope index of inequality (SII)" = "sii", 
#                               "Absolute Concentration Index (ACI)" = "aci", 
#                               "Relative concentration index (RCI)" = "rci",
#                               "Relative index of inequality (RII)" = "rii",
#                               "Relative Index of Inequality (Kunst Mackenbach) (RIIKM)" = "riikm"
# ))
# 
# unrankSummaryMeasures <- sort(c("Between-Group Variance (BGV)" = "bgv", 
#                                 "Index of disparity (IDis)" = "idis", 
#                                 "Mean difference from best performing subgroup (MDB)" = "mdb",
#                                 "Mean difference from mean (MDM)" = "mdm", 
#                                 "Population attributable risk % (PAF)" = "paf", 
#                                 "Population attributable risk (PAR)" = "par", 
#                                 "Range difference (RD)" = "rd",
#                                 "Relative index of inequality (RII)" = "rii", 
#                                 "Range Ratio (RR)" = "rr", 
#                                 "Theil Index (TI)" = "ti",
#                                 "Mean log deviation (MLD)" = "mld"))

healthIndicatorAbbreviations <- c("anc1" = "Antenatal care coverage - at least one visit (2/3 years) (%)",
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


# Don't allow the user to upload too big a data file (100K)
options(shiny.maxRequestSize = 0.100*1024^2)


