# Zhi Huang 08/23/2018
# library(shinyWidgets)
navbarPage(title=div(a(img(src="images/logo.png",
                           "oncoKB webtool",
                           height = 28,
                           style = "margin:0px 0px; padding-bottom: 5px"), href=""),escape=F),
           tabPanel("Converter",
                    sidebarLayout(
                      position = "left",
                      sidebarPanel(width = 3,
                                   tabsetPanel(
                                     tabPanel("Foundation Medicine",
                                              h4("File Uploader", style="color: STEELBLUE"),
                                              fileInput("xml_file", "XML file",
                                                        multiple = FALSE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv", ".vcf")),
                                              # Horizontal line ----
                                              tags$hr(),
                                              p('If you want a sample .vcf file to upload,',
                                                'you can first download the sample',
                                                a(href = 'data/test.vcf', 'test.vcf'),
                                                'file, and then try uploading them.'),
                                              actionButton("action1", "Confirm when Complete")
                                     ),
                                     tabPanel("NantOmics",
                                              h4("File Uploader", style="color: STEELBLUE"),
                                              fileInput("vcf_file", "VCF file",
                                                        multiple = FALSE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv", ".vcf")),
                                              fileInput("xlsx_file", "XLSX sheet (NANT Results) (optional)",
                                                        multiple = FALSE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv", ".vcf")),
                                              
                                              # Include clarifying text ----
                                              helpText("Note: NANT Results is the XLSX file contains the result from NantOmics."),
                                              # Horizontal line ----
                                              tags$hr(),
                                              p('If you want a sample .vcf file to upload,',
                                                'you can first download the sample',
                                                a(href = 'data/test.vcf', 'test.vcf'),
                                                'file, and then try uploading them.'),
                                              actionButton("action2", "Confirm when Complete")
                                     )
                                   )
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("About",
                                   h2("OncoKB Knowledge Database", style="color: STEELBLUE; font-size: 22px"),
                                   HTML('<p> <br> This application was developed to help clinical physicians to get oncoKB information from VCF files.</p>')
                          ),
                          tabPanel("Results",
                                   uiOutput("resultsUI"),
                                   
                                   tags$head(
                                     tags$script(HTML("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                                                      m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                                                      })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
                                                      
                                                      ga('create', 'UA-113406500-2', 'auto');
                                                      ga('send', 'pageview');"))
                                   ),
                                   tags$head(tags$script('Shiny.addCustomMessageHandler("buttonCallbackHandler",
                                                         function(typeMessage) {console.log(typeMessage)
                                                         if(typeMessage == "tab2"){
                                                         $("a:contains(Results)").click();
                                                         }
                                                         });
                                                         
                                                         '))
                                   ), # end of Results Panel
                          
                          tabPanel("Logs",
                                   h4("Database Logs", style="color: STEELBLUE; font-size: 22px"),
                                   verbatimTextOutput("logs")
                          )
                                 )
                               )# end of main Panel
                             ) # end of sidebarLayout
                         ), # end of tabPanel
           tabPanel("Tutorial"
           ),
           tabPanel("FAQ"
                    
           ),
           tabPanel("About",
                    h3("About Us", style="color: STEELBLUE; padding-bottom: 20px"),
                    tags$div(
                      tags$img(src='images/IUSM2.png',
                               height="100",
                               alt="IUSM", class="center", style="padding: 30px"),
                      tags$img(src='images/regenstrief.png',
                               height="100",
                               alt="regenstrief", class="center", style="padding: 30px"),
                      style="text-align: center; padding: 20px"
                    ),
                    h4("Development Team", style="color: STEELBLUE; padding-bottom: 20px"),
                    h5("Prof. Kun Huang's Laboratory", style="color: STEELBLUE"),
                    h4("Publications", style="color: STEELBLUE; padding-bottom: 20px"),
                    tags$ul(
                      tags$li("-")
                    ),
                    h4("Funding for this software is or has been provided by:", style="color: STEELBLUE; padding-bottom: 20px")
           ),
           tags$head(tags$script(HTML("document.title = 'oncoKB webtool';"))), # rename the title by JS
           tags$div(
             p(a("oncoKB webtool", href=""), "Version v1.0 | ", a("IUSM",href="https://medicine.iu.edu/", target="_blank"), " | ", a("RI",href="http://www.regenstrief.org/", target="_blank"), style="color: grey; font-size: 12px"), 
             p("Questions and feedback:  | ", a("Report Issue", href="", target="_blank"), " | ", a("Github", href="", target="_blank"), style="color: grey; font-size: 12px"),
             style="text-align: center; padding-top: 40px"
           )
)
