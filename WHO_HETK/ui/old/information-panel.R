tabPanel(tags$h5("Information"), value='information', 
         tabsetPanel(id = "info_panels",
                     tabPanel(h6("Glossary"), value='gloss_panel', 'TexT'),
                     tabPanel(h6("About"), value='about_panel', source('ui/about-panel.R')$value),
                     tabPanel(h6("Administration"), value='admin_panel',
                              conditionalPanel(condition='input.admin_show==true', source('ui/administration-panel.R')$value),
                              conditionalPanel(condition='input.admin_show==false', includeHTML("./www/adminFail.html"))
                     )
         )
         
) 
