# Raise the default file size limit to 2000MG
options(shiny.maxRequestSize= -1)

function(input, output, session){

  
# read sr dataset -------------------------------------------------------------
    sr = reactive({
    if (!is.null(input$sr_file))
    {
      inFile <- input$sr_file
      fn = inFile$datapath
      df = data.frame(read.delim(fn, nrows = -1, sep = input$sep))
      df$X = NULL
      return(df)
    }
  })
  

# read sc dataset ----  
  #read the sc dataset
  sc = reactive({
  if (!is.null(input$sc_file))
  {
    inFile <- input$sc_file
    fn = inFile$datapath
    df = data.frame(read.delim(fn, nrows = -1, sep = input$sep))
    #df = read.csv.sql(fn, sql = "select * from file where COURSE_CODE != 'NA'")
    df$X = NULL
    
    return(df)
  }
})

# chosenvar functions ----
chosenvar1 <- reactive({return (input$variables1)})
chosenvar2 <- reactive({return (input$variables2)})

# create pdfs ----
output$downloadPlot_gp <- downloadHandler(
  filename = function() {"Grade Penalty.pdf" },
  content = function(file) 
  {
    output$textbox = renderText("RUNNING ANALYSIS")
    validate(need(!is.null(input$sc_file), "Please load student course file"))
    pdf(file ,width=7,height=11)
    grade_penalty(sc(), "Maryland")
    dev.off()
    
  }
)

output$downloadPlot_dp <- downloadHandler(
  filename = function() {"Degree Persistence.pdf" },
  content = function(file) 
  {
    validate(need(!is.null(input$sr_file), "Please load student record file"))
      pdf(file ,width=7,height=11)
      basic_degree_persistence(sr())
      dev.off()
      
  }
)

output$downloadPlot_cp <- downloadHandler(
  filename = function() {"Course Persistence.pdf" },
  content = function(file) 
  {
    validate(need(!is.null(input$sc_file), "Please load student course file"))
    pdf(file ,width=7,height=11)
    if (!is.null(input$sc_file))
    basic.persistence(sc())
    dev.off()
  }
)

# observe for file uploads----
observe({
  
  if (!is.null(input$sr_file))
  {
    updateCheckboxInput(session, "sr_checkbox", value = TRUE)
    output$textbox = renderText(check_missing_sr(sr()))
  }
  
  if (!is.null(input$sc_file))
  {updateCheckboxInput(session, "sc_checkbox", value = TRUE)
   output$textbox = renderText(check_missing_sc(sc()))
  }
  
})

output$mytable1 <- renderTable(
  {tt = NULL
  if (input$sr_checkbox == TRUE) {tt=sr()}
  if (input$sc_checkbox == TRUE) {tt=sc()}
  head(tt,n = 100)}
)

output$variables1 = renderUI({
  tt  = NULL
  if (input$sr_checkbox == TRUE) {tt=c("SELECT", names(sr()))}
  if (input$sc_checkbox == TRUE) {tt=c("SELECT", names(sc()))}
  if (!is.null(tt))
    selectInput('variables1', label = 'Choose dependent variable)', choices = tt, selected = "SELECT")
})

output$variables2 = renderUI({
  tt = NULL
  if (input$sr_checkbox == TRUE) {tt=c("SELECT", names(sr()))}
  if (input$sc_checkbox == TRUE) {tt=c("SELECT", names(sc()))}
  if (!is.null(tt))
    selectInput('variables2', label = 'Choose independent variable (optional)', choices = tt, selected = "SELECT")
})

output$sr_checkbox = renderUI({
  checkboxInput("sr_checkbox", label = "Select", value = FALSE)
})

output$sc_checkbox = renderUI({
  checkboxInput("sc_checkbox", label = "Select", value = FALSE)
})

# data tab ----
output$mytable2 <- renderTable({  
  validate(need(input$variables1 != "SELECT", "Please select a variable for analysis"))

  if (input$sr_checkbox == TRUE) 
     {table(sr()[,input$variables1])}
  
  }) #output$mytable2

output$mytable2 <- renderTable({  
  validate(need(input$variables1 != "SELECT", "Please select a variable for analysis"))
  
  if (input$sc_checkbox == TRUE) 
  {table(sc()[,input$variables1])}
  
}) #output$mytable2

# plot tab ----
output$myPlot <- renderPlot({  
  validate(need(input$variables1 != "SELECT", "Please select a variable for analysis"))
  
  if (input$sr_checkbox == TRUE)
    {
    if (input$variables2 == "SELECT")
      { p = ggplot(data=sr(), aes_string(x=chosenvar1())) +  geom_histogram()
       print (p)
      }
    if (input$variables2 != "SELECT")
    {
      f = as.formula(concat(c(chosenvar1(),  " ~ " , chosenvar2())))
      out = aggregate(f, data = sr(), mean)
      names(out) = c("group", "mean")
       p = ggplot(data=out, aes(x=group, y=mean, group=group, colour=group, fill= group)) + geom_bar(stat="identity")
      print (p)
    }
  }
  
    if  (input$sc_checkbox == TRUE) 
    { p = ggplot(data=sc(), aes_string(x=chosenvar1())) +  geom_histogram()
    print (p)
    }
  })


} #server
