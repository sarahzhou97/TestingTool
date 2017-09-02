subtitle_str <- "This tool calculates sample size and test duration for any number of metrics, and analyzes test results. All calculations are viewable in-app, and exportable to Excel for further analysis."

introduction_str <- "Testing is a Data Analyst’s bread and butter—-more specifically, A/B testing, and occasionally Multivariate testing.The Sample Size Calculator feature consists of standard Pre-Analysis procedures—calculating sample size, and estimating test duration for different combinations of expected percent lifts, and percent splits between test and control cells.  This Shiny application automates this process, turning a normally time-consuming and soul-sucking process into something that is as easy as plugging in a couple of parameters. The Test Analyzer feature helps users analyze test results after their data pull that consolidates and builds on top of already-existent R statistical libraries. Both features have robust error-checking, and have two sample t-test and z-test options."

t.test_str <- "For sample t-test:"

z.test_str <- "For proportion z-test:"
  
sampleSizeCalculator1_str <- "After entering the necessary input values (all are required unless indicated 'Optional'), the resulting matrices can be either displayed in-app ('Generate Tables' button) or exported to Excel ('Download')"

sampleSizeCalculator_str_z.test <-
  tags$ul(
    tags$li("- Input formats:","p1 values must be separated by a comma AND a space (eg. “0.5, 0.9”)"),
    tags$li("- Error checking on:","p1 values must be < 1, input format, missing values")
)

sampleSizeCalculator_str_t.test <- 
  tags$ul(
  tags$li("- Input formats: Test values (mean, standard deviation) must be separated by a semi-colon AND a space, and mean and standard deviations must be separated by a comma AND a space (eg. “mean1, sd1; mean2, sd2” is “11, 4.3; 9, 2”)"),
  tags$li("- Error checking on:","input format, missing values")
)

sampleSizeCalculator2_str <- 
  tags$ul(
    tags$li("- Default values are set for the Sample Size Calculator to provide a formatted example of the input values."), 
    tags$li("- 'Average Samples Per Day' input is only for Test Duration matrices. For Sample Size matrices, leave it blank."),
    tags$li("- Errors are displayed on the main panel in red when inputs are submitted")
  )

analyzer1_str <- "After entering the necessary input values (all are required), the resulting data frame can be either displayed in-app (“Generate Tables” button) or exported to Excel (“Download”)"
analyzer_str_p.test <- paste0("input values are number of success samples and total samples for both control and test cells.")

analyzer_str_t.test <-  
  tags$ul(
  tags$li("- CSV method: input values are a csv file, and the control column name, and test column name"), 
  tags$li("- Parameters method: input values are mean, standard deviation, and number of samples for both control and test cells.")
)
