library(shiny)
library(xtable)
library(htmltools)
require(xlsx)

source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')
  
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)
  
  envir <- globalenv()
  source(tempR, local = envir, ...)
}

source_rmd('calculations/TestAnalyzer.Rmd')
source_rmd('calculations/SampleSizeCalculator.Rmd')
source_rmd('util/UtilityFunctions.Rmd')
source_rmd('calculations/SampleSizeCalculator_Multivariate.Rmd')

shinyServer(function(input, output) {
  
  getTestVals <- function(test) {
    if (test=='z') {
      return(process_p1(input$p1))
    } else if (test=='t') {
      return(process_mu1_sd(input$mu1sd))
    } else if (test=='anova') {
      return(process_anova(input$mu1sd_anova))
    } else if (test=='chisq'){
      return(process_anova(input$p1factor1factor2_chisq))
    }
  }
  
  #Sample Size Calculator
  
  getDFs <- function(test) {
    
    #converting raw input
    if(test=='z') { #z-test
      req(input$p1,input$delta_z,input$split_z,input$sig.level_z,input$power_z)
      test_vals <- getTestVals('z')
      delta <- getNumericVector(input$delta_z)
      split <- getNumericVector(input$split_z)
      sig.level <- as.numeric(input$sig.level_z)
      power <- as.numeric(input$power_z)
      if(input$samples_z=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_z)
      }
      dfs <- getSampleSizeMatrices(test,test_vals,delta_arr = delta,samplePerc_arr = split,power = power,sig.level = sig.level,avgSamplesPerDay = samples)

    } else if(test=='t') { #t-test
      req(input$mu1sd,input$delta_t,input$split_t,input$sig.level_t,input$power_t)
      test_vals <- getTestVals('t')
      delta <- getNumericVector(input$delta_t)
      split <- getNumericVector(input$split_t)
      sig.level <- as.numeric(input$sig.level_t)
      power <- as.numeric(input$power_t)
      if(input$samples_t=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_t)
      }
      dfs <- getSampleSizeMatrices(test,test_vals,delta_arr = delta,samplePerc_arr = split,power = power,sig.level = sig.level,avgSamplesPerDay = samples)

    } else if(test=='anova') { #anova
      
      req(input$mu1sd_anova,input$delta_anova,input$sig.level_anova,input$power_anova)

      test_vals <- getTestVals('anova')
      print(test_vals)
      delta <- getNumericVector(input$delta_anova)
      sig.level <- as.numeric(input$sig.level_anova)
      power <- as.numeric(input$power_anova)
      if(input$samples_anova=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_anova)
      }
      
      dfs <- getSampleSizeMatrices_multivariate(test,test_vals,power = power,sig.level = sig.level,avgSamplesPerDay = samples)
      print(dfs)
      
    } else if(test=='chisq') { #chisq
      
      req(input$p1factor1factor2_chisq,input$delta_chisq,input$sig.level_chisq,input$power_chisq)

      test_vals <- getTestVals('chisq')
      delta <- getNumericVector(input$delta_chisq)
      sig.level <- as.numeric(input$sig.level_chisq)
      power <- as.numeric(input$power_chisq)
      if(input$samples_chisq=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_chisq)
      }
      dfs <- getSampleSizeMatrices_multivariate(test,test_vals,power = power,sig.level = sig.level,avgSamplesPerDay = samples)
    }
    
  }
  
  
  
  tabelizeMatrices <- function(test) {
    #rendering data tables
    tables <- list() # create a list to hold all tables
    
    dfs <- getDFs(test)
    test_vals<- getTestVals(test)
    
    for (i in 1:length(dfs)) {
      if (test=='z') {
        tables[[i]] <- getHTMLTable(dfs[[i]],caption=paste("p1:", test_vals[[i]]))
      } else if (test=='t'){
        tables[[i]] <- getHTMLTable(dfs[[i]],caption=paste("mean 1:", test_vals[[i]][1],"standard deviation:",test_vals[[i]][2]))
      } else if (test=='anova') {
        tables[[i]] <- getHTMLTable(dfs[[i]],caption=paste("control mean:", test_vals[[i]][1],"standard deviation:",test_vals[[i]][2],"number of groups:",test_vals[[i]][3]),include.rownames = FALSE)
      } else if (test=='chisq') {
        tables[[i]] <- getHTMLTable(dfs[[i]],caption=paste("p1:", test_vals[[i]][1],"number of levels in factor 1:",test_vals[[i]][2],"number of levels in factor 2:",test_vals[[i]][3]),include.rownames = FALSE)
      }
    }

    return(lapply(as.vector(as.character(unlist(tables))), paste)) # return HTML tables pasted together
  }
  
  output$download_samplesize_z <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      req(input$p1,input$delta_z,input$split_z,input$sig.level_z,input$power_z)
      
      p1 <- process_p1(input$p1)
      delta <- getNumericVector(input$delta_z)
      split <- getNumericVector(input$split_z)
      sig.level <- as.numeric(input$sig.level_z)
      power <- as.numeric(input$power_z)
      
      if(input$samples_z=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_z)
      }
      
      list_dfs <- getSampleSizeMatrices('z',p1,delta_arr = delta,samplePerc_arr = split,power=power,sig.level=sig.level)
      exportToExcel(list_dfs,file=file)
      
    }
    
  )
  
  output$download_samplesize_t <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      req(input$mu1sd,input$delta_t,input$split_t,input$sig.level_t,input$power_t)
      
      mu1sd <- process_mu1_sd(input$mu1sd)
      delta <- getNumericVector(input$delta_t)
      split <- getNumericVector(input$split_t)
      sig.level <- as.numeric(input$sig.level_t)
      power <- as.numeric(input$power_t)
      
      if(input$samples_t=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_t)
      }
      
      list_dfs <- getSampleSizeMatrices('t',mu1sd,delta_arr = delta,samplePerc_arr = split,power=power,sig.level=sig.level)
      exportToExcel(list_dfs,file=file)
    }
  )
  
  output$download_analysis_t_csv <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      csvfile <- input$file
      if(is.null(csvfile)) {
        return(NULL)
      }
      rawdf <- read.csv2(csvfile$datapath)
      control_vector <- rawdf[[input$control_name]]
      test_vector <- rawdf[[input$test_name]]
      
      df <- data.frame(t.test.fromdata(control_vector,test_vector))
      exportToExcel(list(df),file=file)
    }
  )
  
  output$download_analysis_t_param <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      df <- as.data.frame(t.test.fromvalues(as.numeric(input$m1),as.numeric(input$m2),as.numeric(input$sd1),as.numeric(input$sd2),as.numeric(input$num1),as.numeric(input$num2)))
      exportToExcel(list(df),file=file)
    }
  )
  
  output$download_analysis_z <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      df <- as.data.frame(z.test.fromdata(as.numeric(input$x1),as.numeric(input$x2),as.numeric(input$n1),as.numeric(input$n2)))
      exportToExcel(list(df),file=file)
    }
  )
 
  observeEvent(input$table_z, {
    output$tables_samplesize <- renderUI({
      HTML(unlist(tabelizeMatrices('z')))
    })
  })
  
  observeEvent(input$table_t, {
    output$tables_samplesize <- renderUI({
      HTML(unlist(tabelizeMatrices('t')))
    })
  })
  
  #Analyzer
  
  observeEvent(input$analyzer_z, {
    output$tables_analyzer <- renderUI({
      df <- as.data.frame(z.test.fromdata(as.numeric(input$x1),as.numeric(input$x2),as.numeric(input$n1),as.numeric(input$n2)))
      HTML(getHTMLTable(df,include.rownames = FALSE))
    })
  })
  
  observeEvent(input$analyzer_t_csv, {
    file <- input$file
    if(is.null(file)) {
      return(NULL)
    }
    df_csv <- read.csv(file$datapath)
    
    control_vector <- as.numeric(na.omit(df_csv[[input$control_name]]))
    test_vector <- as.numeric(na.omit(df_csv[[input$test_name]]))
    
    output$tables_analyzer <- renderUI({
      df <- as.data.frame(t.test.fromdata(control_vector,test_vector))
      HTML(getHTMLTable(df,include.rownames = FALSE))
    })
    
  })
  
  observeEvent(input$analyzer_t_values, {
    output$tables_analyzer <- renderUI({
      
    df <- as.data.frame(t.test.fromvalues(as.numeric(input$m1),as.numeric(input$m2),as.numeric(input$sd1),as.numeric(input$sd2),as.numeric(input$num1),as.numeric(input$num2)))
    HTML(getHTMLTable(df,include.rownames = FALSE))
    
    })
  })
  
  observeEvent(input$table_anova, {
    output$tables_samplesize <- renderUI({
      HTML(unlist(tabelizeMatrices('anova')))
    })
  })
  
  observeEvent(input$table_chisq, {
    output$tables_samplesize <- renderUI({
      HTML(unlist(tabelizeMatrices('chisq')))
    })
  })
  
  output$download_samplesize_anova <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      req(input$mu1sd_anova,input$delta_anova,input$sig.level_anova,input$power_anova)
      
      test_vals <- getTestVals('anova')
      print(test_vals)
      delta <- getNumericVector(input$delta_anova)
      sig.level <- as.numeric(input$sig.level_anova)
      power <- as.numeric(input$power_anova)
      if(input$samples_anova=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_anova)
      }
      
      list_dfs <- getSampleSizeMatrices_multivariate(test,test_vals,power = power,sig.level = sig.level,avgSamplesPerDay = samples)
      
      exportToExcel(list_dfs,file=file)
      
    }
    
  )
  
  output$download_samplesize_anova <- downloadHandler(
    filename = function() {paste('out','.xlsx',sep='')},
    content = function(file) {
      req(input$p1factor1factor2_chisq,input$delta_chisq,input$sig.level_chisq,input$power_chisq)
      test_vals <- getTestVals('chisq')
      delta <- getNumericVector(input$delta_chisq)
      sig.level <- as.numeric(input$sig.level_chisq)
      power <- as.numeric(input$power_chisq)
      if(input$samples_chisq=="") {
        samples <- NULL
      } else {
        samples <- as.numeric(input$samples_chisq)
      }
      
      list_dfs <- getSampleSizeMatrices_multivariate(test,test_vals,power = power,sig.level = sig.level,avgSamplesPerDay = samples)
      
      exportToExcel(list_dfs,file=file)
      
    }
    
  )
  
    

  })
  
  

