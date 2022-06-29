
library(shiny)
library(unmarked)
library(ggplot2)
library(reshape2)

function(input, output,session) {

  
  #Upload Fish data
  df_fish_upload <- reactive({
    inFile <- input$fish_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$fish_separator)
    return(df)
  })
  
  #upload covariate file
  df_covariates_upload <- reactive({
    inFile <- input$covariate_upload
    if (is.null(inFile))
      return(NULL)
    covar_df <- read.csv(inFile$datapath, header = TRUE,sep = input$covar_separator)
    return(covar_df)
  })
  
  #return fish data for display
  output$sample_table<- DT::renderDataTable({
    df <- df_fish_upload()
    DT::datatable(df)
  })
  
  #return covariate table for display
  output$covar_table<- DT::renderDataTable({
    covar_df <- df_covariates_upload()
    DT::datatable(covar_df)
  })
  
  output$covarsummar <- renderPrint({
    validate(need(is.null(df_covariates_upload()) == FALSE,"Please enter covariate data." ))
    summary(df_covariates_upload())
  })
    
  output$fishplot <- renderPlot({
    validate(need(is.null(df_fish_upload()) == FALSE,"Please enter fish data." ))
    long_fish = melt(df_fish_upload(),id.vars=c(names(df_fish_upload()[1])))

    ggplot(long_fish,aes(x=variable,y=value,group=as.factor(long_fish[,1]))) +
      geom_point(size=5)+
      geom_line(size=2,aes(colour=as.factor(long_fish[,1])))+
      ylab("Number of fish")+
      xlab("Pass")+
      labs(color = "Site")+
      theme(axis.text.x=element_text(size=20),
            axis.text.y=element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20,angle=90),
            legend.title=element_text(size=20), 
            legend.text=element_text(size=16)
      )
  })
  
  #Fish pcount model
  fitmod <- reactive({
    if(is.null(df_covariates_upload())){
      df2 <- df_fish_upload()
      obs <- as.matrix(df2[,2:ncol(df2)])
      umf <- unmarkedFramePCount(y=obs)
      fm1 <- pcount(~1 ~1 , umf, K=max(obs)*50,mixture=c("P"))
      return(fm1)
    }
    
    else{
      if(is.null(abund_covaarform())) {
        abundfomr="~1 "
      }
      else{
        abundfomr=abund_covaarform()
      }
      
      if(is.null(detect_covaarform())) {
        detectfomr="~1 "
      }
      else{
        detectfomr=detect_covaarform()
      }
      
      #build formula
      formulaall=paste(detectfomr,abundfomr,sep=" ")
      #return(formulaall)
      df2 <- df_fish_upload()
      obs <- as.matrix(df2[,2:ncol(df2)])
      df3 <- df_covariates_upload()
      covars <- as.matrix(df3[,2:ncol(df3)])

      umf <- unmarkedFramePCount(y=obs,siteCovs = data.frame(covars))
      fm1 <- pcount(as.formula(formulaall), umf, K=max(obs)*50,mixture=c("P"))
      return(fm1)
    }
  })
  
  #Print input data summary
  output$InputSummary <- renderPrint({
    validate(need(is.null(df_fish_upload()) == FALSE,"Please enter data." ))
    
    if(is.null(df_covariates_upload())){
      df2 <- df_fish_upload()
      obs <- as.matrix(df2[,2:ncol(df2)])
      umf <- unmarkedFramePCount(y=obs)
      summary(umf)
    }
    
    else{
      df2 <- df_fish_upload()
      obs <- as.matrix(df2[,2:ncol(df2)])
      df3 <- df_covariates_upload()
      covars <- as.matrix(df3[,2:ncol(df3)])
      umf <- unmarkedFramePCount(y=obs,siteCovs = data.frame(covars))
      summary(umf)
    }
  })

  #print model results summary
  output$results <- renderPrint({
    #validate results first
    validate(need(is.null(df_fish_upload()) == FALSE,"Please enter data." ))
    print(fitmod())
    #summary(fitmod())
    })
  
  #Generate and print abundance predictions by site
  output$abundancepredictions <- renderPrint({
    validate(need(is.null(df_fish_upload()) == FALSE,"Please enter data." ))
    if(is.null(fitmod())) return(NULL)
    if(is.null(df_covariates_upload())){
      #Abundance
      btlc <- backTransform(fitmod(),type="state")
      btconfint <- confint(btlc, level = 0.95)
      
      
      df1=data.frame(Type = c("Abundance"),
                     Estimate=c(btlc@estimate),
                     L95 = c(btconfint[1,1]),
                     U95 = c(btconfint[1,2]) 
                     )
      return(df1)}
    else{
      print("Site level abundance estimates.")
      predict(fitmod(),type="state")
    }
    })
  
  #Generate and print detection predictions by site and pass
  output$Detectionpredictions <- renderPrint({
    validate(need(is.null(df_fish_upload()) == FALSE,"Please enter data." ))
    if(is.null(fitmod())) return(NULL)
    if(is.null(df_covariates_upload())){
      #Detection
      btdetect <- backTransform(fitmod(),type="det")
      btconfintdetect <- confint(btdetect, level = 0.95)
      
      df1=data.frame(Type = c("Detection"),
                     Estimate=c(btdetect@estimate),
                     L95 = c(btconfintdetect[1,1]),
                     U95 = c(btconfintdetect[1,2]) 
      )
      return(df1)}
    else{
      print("Site and pass level detection estimates.")
      predict(fitmod(),type="det")      
    }
  })


  #Update selectInput box for abundance and detection covariates and predictions box based on user selections
  observe({
    df2 <-df_covariates_upload()
    if(is.null(df2))  return(NULL)
    
    items2=names(df2)
    names(items2)=items2
    updateSelectInput(session,"abu_covars", label = "Select abundance predictor(s)",
                      choices=list(`Covariate` = items2[2:length(items2)]))
    
    updateSelectInput(session,"detect_covars", label = "Select detection predictor(s)",
                      choices=list(`Covariate` = items2[2:length(items2)]))
  })
  
  #Population predictions combo box based on selected covariates above
  observe({
    updateSelectInput(session,"abu_covars_pred", label = "Select abundance predictor",
                      choices=input$abu_covars)
  })
  
  
  #Generate abundance model - might be able to integrate into model function above
  abund_covaarform <- reactive({
     if(is.null(input$abu_covars)) return(NULL)
     #paste(as.formula(paste(c("~1",input$abu_covars),collapse="+")))
     paste(c("~1",input$abu_covars),collapse="+")
    # paste(c("~1",names(covars)),collapse="+"))
     
   })
  
  #Generate detection model - might be able to integrate into model function above
  detect_covaarform <- reactive({
     if(is.null(input$detect_covars)) return(NULL)
     #paste(as.formula(paste(c("~1",input$detect_covars),collapse="+")))
     paste(c("~1",input$detect_covars),collapse="+")
   })
   
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     #params <- list(n = input$slider)
  #     
  #     if(is.null(df_covariates_upload())){
  #       #Abundance
  #       btlc <- backTransform(fitmod(),type="state")
  #       btconfint <- confint(btlc, level = 0.95)
  #       
  #       
  #       df1=data.frame(Type = c("Abundance"),
  #                      Estimate=c(btlc@estimate),
  #                      L95 = c(btconfint[1,1]),
  #                      U95 = c(btconfint[1,2]) 
  #       )
  #       params <- list(n=df1)
  #       }
  #     else{
  #       #print("Site level abundance estimates.")
  #       params <- list(n=predict(fitmod(),type="state"))
  #     }
  #     
  #     
  #     #params <- list(n=output$abundancepredictions)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
}


