# Define UI for application that plots random distributions

#get About panel
tabPanelAbout <- source("about.R")$value


shinyUI(pageWithSidebar(

  # Application title
  headerPanel("NADA and Substitution Methods - Simulator"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(

    wellPanel(
      h2("Distribution Type and Parameters"),
    selectInput("distribution", "Distribution Type:", c("Log-Normal", "Gamma", "Normal")),
    # This UI limits the reactivity to only this button and changing the distribution

    # IF LN this is the input values
    conditionalPanel(

      condition="input.distribution == 'Log-Normal'",
      numericInput("sdlog",
                   "SD of Logs:",
                   min=0.1,
                   max=2.5,
                   value=1,
                   step=0.5
      ),
      h4("Geometric Standard Deviation"),
      textOutput("GSD"),

      numericInput("meanlog",
                   "Mean of Logs:",
                   min=0.0,
                   max=10,
                   value=0.5,
                   step=0.5
      ),
      h4("Geometric Mean"),
      textOutput("GM")


      ),

    # IF GAMMA this is the input values
    conditionalPanel(
      condition="input.distribution == 'Gamma'",
      numericInput("shape",
                   "Shape:",
                   min=0.1,
                   max=4,
                   value=1,
                   step=0.5
      ),
      numericInput("scale",
                   "Scale:",
                   min=1,
                   max=200,
                   value=100,
                   step=10
      )),
    # IF NORMAL this is the input values
    conditionalPanel(
      condition="input.distribution == 'Normal'",
      numericInput("sd",
                   "Standard Deviation:",
                   min=0.1,
                   max=20,
                   value=1,
                   step=0.5
      ),

      numericInput("mu",
                   "Mean:",
                   min=-10,
                   max=10,
                   value=10,
                   step=1
      )

    ),
      h4("PDF of Distribtion"),
      plotOutput("distGraph")
),

#---- Number of Samples and Censoring Values
    wellPanel(
      h2("Number of Samples and Censoring Information"),
    # This ends the conditional input, we now input our censoring information


      #----
      wellPanel(h4("Sample Size (total)"),
                textOutput("totalObs"),
                h4("Expected Censoring Rate"),
                textOutput("exCen")
                ),

      #---- group 1

      wellPanel(h5("Sample Group 1"),
#                 div(class="row",

                div(class='row',
                    div(class="span2 offset1", numericInput("obs1", "Sample Size:", 50,min=1,max=150,step=1)),
                    div(class="span2", numericInput("censQ1", "Censoring Quantile:", .5 ,min=0,max=1, step=0.01)),
                    div(class="span2", h5("Detection Limit"), textOutput("DL1"))

                ),
                tags$style(type="text/css", '#obs1 {width: 50px;}'),
                tags$style(type="text/css", '#censQ1 {width: 50px;}'),
                tags$style(type="text/css", '#DL1 {width: 50px;}')

                ),

      wellPanel(h5("Sample Group 2"),
                div(class='row',
                    div(class="span2 offset1", numericInput("obs2", "Sample Size:", 50,min=0,max=150,step=1)),
                    div(class="span2", numericInput("censQ2", "Censoring Quantile:", .25 ,min=0,max=1, step=0.01)),
                    div(class="span2", h5("Detection Limit"), textOutput("DL2"))

                ),
                tags$style(type="text/css", '#obs2 {width: 50px;}'),
                tags$style(type="text/css", '#censQ2 {width: 50px;}'),
                tags$style(type="text/css", '#DL2 {width: 50px;}')

      ),

      wellPanel(h5("Sample Group 3"),
                div(class='row',
                    div(class="span2 offset1", numericInput("obs3", "Sample Size:", 0 ,min=0,max=150,step=1)),
                    div(class="span2", numericInput("censQ3", "Censoring Quantile:", .15 ,min=0,max=1, step=0.01)),
                    div(class="span2", h5("Detection Limit"), textOutput("DL3"))

                ),
                tags$style(type="text/css", '#obs3 {width: 50px;}'),
                tags$style(type="text/css", '#censQ3 {width: 50px;}'),
                tags$style(type="text/css", '#DL3 {width: 50px;}')

     )
    ),

    numericInput("simNum",
                 "Number of Simulated Samples:",
                 min=1,
                 max=1000,
                 value=100,
                 step=100
    ),

    # this plots reactively the desnity plot for the proposed distribution and will be reactive to changes in theta values
      actionButton("myValue1", "Select Distribution and RUN Sim"),
      downloadButton('downloadData', 'Download Simulated Data'),
      downloadButton('downloadResults', 'Download Means and S.D. Table')

  ),

  # This is output of the simulation
  mainPanel(

    # we create tabs - plots, summary, RMSE, Bias, and Censored Statistical Summary
    tabsetPanel(
      tabPanel("Plots",
               plotOutput("Meangraph"),
               plotOutput("SDgraph")
      ),

      tabPanel("Summary",
               verbatimTextOutput("summary"),
               h3("Mean Values"),
               tableOutput("Meanview"),
               h3("Standard Deviation Values"),
               tableOutput("SDview")
      ),

      tabPanel("RMSE Plot",
               plotOutput("RMSEplot.mean"),
               plotOutput("RMSEplot.sd")
      ),

      tabPanel("Bias Plot",
               plotOutput("Bplot.mean"),
               plotOutput("Bplot.sd")
      ),

      tabPanel("Censored Statistics Summary",
               h2("Summary Statistics"),
               tableOutput("censum.all"),
               h2("Censoring Limits"),
               tableOutput("limits.all"),
               h2("Summary Statistics by Sample Group"),
               tableOutput("censum.everything"),
               h2("Summary Statistics by Sample Group"),
               tableOutput("limits.everything")
               ),
      tabPanelAbout(), id = "allPanels")

    )


  )
)
