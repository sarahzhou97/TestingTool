library(shiny)

source('resources/strings.R')

shinyUI(tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  tags$div(id = "header", img(src='IconsAndLogos/Icons/iconAcom_Green.png'),
    h1("Testing Tool",align="center"),
    img(id = "rightimg",src='IconsAndLogos/Icons/iconAcom_Green_flipped.png')
  ),
  navbarPage(
    theme = "spacelab",
    "",
    tabPanel("About",
             tags$div(id = "about",h2("A/B and Multivariate Testing Tool for Analysts",style="strong"),
             p(subtitle_str),
             img(src='IconsAndLogos/Icons/iconShakyLeaf_Green.png'),h3("Introduction"),
             p(introduction_str),
             img(src='IconsAndLogos/Icons/iconShakyLeaf_Green.png'),
              h3("Sample Size Calculator"),
              p(sampleSizeCalculator1_str),
             p(strong(z.test_str)),
             sampleSizeCalculator_str_z.test,
             p(strong(t.test_str)),
             sampleSizeCalculator_str_t.test,
             p("Notes:"),
             p(sampleSizeCalculator2_str),
              img(src='IconsAndLogos/Icons/iconShakyLeaf_Green.png'),
              h3("Test Analyzer"),
              p(analyzer1_str),
             p(strong(z.test_str),analyzer_str_p.test),
              p(strong(t.test_str)),
              analyzer_str_t.test),
             tags$div(class = "footer","©Sarah Zhou 2017")),
    tabPanel("Sample Size Calculator",
             sidebarPanel(
               tabsetPanel(
                 tabPanel("2-Sample Test",
                    tabsetPanel(
                       tabPanel("z-test",
                                textInput("p1", label = h4('P1 Value(s)'), value = "0.5, 0.9"),
                                textInput("delta_z", label = h4('Expected Lift'), value = "0.01, 0.02, 0.05, 0.1"),
                                textInput("split_z", label = h4('Holdout'), value = "0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5"),
                                sliderInput("sig.level_z", label = h4("Significance Level"), value = 0.05, min = 0.05, max = 0.15,step = 0.025),
                                sliderInput("power_z", label = h4("Power"), value = 0.8, min = 0.7, max = 0.9,step = 0.05),
                                textInput("samples_z", label = h4("Average Samples Per Day (Optional)")),
                                h6("Leave above text field blank if you want Sample Size. Enter value if you want Test Duration."),
                                br(),
                                actionButton("table_z","Generate Tables"),
                                downloadButton('download_samplesize_z', 'Export to Excel'),
                                br()
                       ),
                       tabPanel("t-test",
                                textInput("mu1sd", label = h4('Mean and Standard Deviation Value(s)'), value = "5, 1; 9.8, 3.4"),
                                textInput("delta_t", label = h4('Expected Lift'), value = "0.01, 0.02, 0.05, 0.1"),
                                textInput("split_t", label = h4('Holdout'), value = "0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5"),
                                sliderInput("sig.level_t", label = h4("Significance Level"), value = 0.05, min = 0.05, max = 0.15,step = 0.025),
                                sliderInput("power_t", label = h4("Power"), value = 0.8, min = 0.7, max = 0.9,step = 0.05),
                                textInput("samples_t", label = h4("Average Samples Per Day (Optional)")),
                                h6("Leave above text field blank if you want Sample Size. Enter value if you want Test Duration."),
                                br(),
                                actionButton("table_t","Generate Tables"),
                                downloadButton("download_samplesize_t","Export to Excel")
                       )
                      )
                  ),
                 tabPanel("Multivariate Test",
                          tabsetPanel(
                            tabPanel("ANOVA 1-way",
                                     br(),
                                     h6("Note: this test assumes the same 'expected lift' for all Test Groups."),
                                     textInput("mu1sd_anova", label = h4('Mean and Standard Deviation Value(s)'),value = "5, 1, 4; 9.8, 3.4, 4"),
                                     textInput("delta_anova", label = h4('Expected Lift'), value = "0.1, 0.2, 0.3, 0.4"),
                                     sliderInput("sig.level_anova", label = h4("Significance Level"), value = 0.05, min = 0.05, max = 0.15,step = 0.025),
                                     sliderInput("power_anova", label = h4("Power"), value = 0.8, min = 0.7, max = 0.9,step = 0.05),
                                     textInput("samples_anova", label = h4("Average Samples Per Day (Optional)")),
                                     h6("Leave above text field blank if you want Sample Size. Enter value if you want Test Duration."),
                                     br(),
                                     actionButton("table_anova","Generate Tables"),
                                     downloadButton("download_samplesize_anova","Export to Excel")
                            ),
                            tabPanel("Chi-Square Test",
                                     br(),
                                     h6("Note: this test assumes the same 'expected lift' for all Test Groups."),
                                     textInput("p1factor1factor2_chisq", label = h4('P1, Number of Levels for Factor 1, and Number of Levels for Factor 2'), value = "0.5, 2, 2; 0.8, 3, 4"),
                                     textInput("delta_chisq", label = h4('Expected Lift'), value = "0.1, 0.2, 0.3, 0.4"),
                                     sliderInput("sig.level_chisq", label = h4("Significance Level"), value = 0.05, min = 0.05, max = 0.15,step = 0.025),
                                     sliderInput("power_chisq", label = h4("Power"), value = 0.8, min = 0.7, max = 0.9,step = 0.05),
                                     textInput("samples_chisq", label = h4("Average Samples Per Day (Optional)")),
                                     h6("Leave above text field blank if you want Sample Size. Enter value if you want Test Duration."),
                                     br(),
                                     actionButton("table_chisq","Generate Tables"),
                                     downloadButton("download_samplesize_chisq","Export to Excel")
                            )
                            
                          )
                 )
               )
             ),
             mainPanel(
               tags$div(id="calculator_mainpanel",uiOutput("tables_samplesize"))
             )
    ),
    tabPanel("Analyzer",
             sidebarPanel(
               tabsetPanel(
                 tabPanel("2-Sample Test",
                    tabsetPanel(
                      tabPanel("z-test",
                              h3("Test"),
                              textInput("x1", label = h4('Number of successes')),
                              textInput("n1", label = h4('Number of samples')),
                              h3("Control"),
                              textInput("x2", label = h4('Number of successes')),
                              textInput("n2", label = h4('Number of samples')),
                              br(),
                              actionButton("analyzer_z","Analyze"),
                              downloadButton('download_analysis_z', 'Export to Excel')
                      ),
                       tabPanel("t-test",
                                tabsetPanel(
                                  tabPanel("From CSV File",
                                           fileInput("file",label = h4('File')),
                                           textInput("control_name", label = h4('Control column name')),
                                           textInput("test_name", label = h4('Test column name')),
                                           br(),
                                           actionButton("analyzer_t_csv","Analyze"),
                                           downloadButton('download_analysis_t_csv', 'Export to Excel')
                                  ),
                                  tabPanel("From Parameters",
                                           h3("Test"),
                                           textInput("m1", label = h4('Mean')),
                                           textInput("sd1", label = h4('Standard Deviation')),
                                           textInput("num1", label = h4('Number of Samples')),
                                           h3("Control"),
                                           textInput("m2", label = h4('Mean')),
                                           textInput("sd2", label = h4('Standard Deviation')),
                                           textInput("num2", label = h4('Number of Samples')),
                                           br(),
                                           actionButton("analyzer_t_values","Analyze"),
                                           downloadButton('download_analysis_t_param', 'Export to Excel')
                                  )
                                  
                                )
                       )
                    )
               ),
               tabPanel("Multivariate Test",
                        tabsetPanel(
                          tabPanel("ANOVA 1-way"
                                   
                          ),
                          tabPanel("Chi-Square Test",
                                   tabsetPanel(
                                     tabPanel("From CSV File"
                                              
                                     ),
                                     tabPanel("From Parameters"
                                     )
                                     
                                   )
                          )
                        )
               )
             )
             ),
             mainPanel(
               tags$div(id = "analyzer_mainpanel",uiOutput("tables_analyzer"))
             )
    )
  
)
)
)


