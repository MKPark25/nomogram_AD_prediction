#==================================================
# Nomogram for Probability of Conversion
#==================================================

library(shiny)
library(rms)
library(Hmisc)
library(readr)

# (1) Data import
data <- read_csv("test_data_for_nomogram.csv")

# (2) factor 
data$Sex  <- factor(data$Sex, levels = c(0,1))
data$ApoE4 <- factor(data$ApoE4, levels = c(0,1))

# (3) datadist 
dd <- datadist(data)
options(datadist="dd")

# (4) aregImpute
imputeFormula <- ~ Ab42 + GFAP + NFL + pTau181 + pTau217 +
  Age + Sex + Edu_Years + ApoE4 +
  ADNI_MEM + ADNI_EF + ADNI_LAN + MMSE + Conversion

f <- aregImpute(imputeFormula, data=data, n.impute=5)

#############################################################################
### ADNI_MEM Model
#############################################################################
formula_mem <- Conversion ~ Ab42 + GFAP + NFL + pTau181 + pTau217 + Age + Sex + Edu_Years + ApoE4 + ADNI_MEM
fit_mem <- fit.mult.impute(formula = formula_mem, fitter = lrm, xtrans = f, data = data)
nom_mem <- nomogram(fit_mem, fun = plogis, fun.at = seq(0.1, 0.9, by=0.1), funlabel = "Probability of CONV")

#############################################################################
### ADNI_EF Model
#############################################################################
formula_ef <- Conversion ~ Ab42 + GFAP + NFL + pTau181 + pTau217 + Age + Sex + Edu_Years + ApoE4 + ADNI_EF
fit_ef <- fit.mult.impute(formula = formula_ef, fitter = lrm, xtrans = f, data = data)
nom_ef <- nomogram(fit_ef, fun = plogis, fun.at = seq(0.1, 0.9, by=0.1), funlabel = "Probability of CONV")

#############################################################################
### ADNI_LAN Model
#############################################################################
formula_lan <- Conversion ~ Ab42 + GFAP + NFL + pTau181 + pTau217 + Age + Sex + Edu_Years + ApoE4 + ADNI_LAN
fit_lan <- fit.mult.impute(formula = formula_lan, fitter = lrm, xtrans = f, data = data)
nom_lan <- nomogram(fit_lan, fun = plogis, fun.at = seq(0.1, 0.9, by=0.1), funlabel = "Probability of CONV")

#############################################################################
### MMSE Model
#############################################################################
formula_mmse <- Conversion ~ Ab42 + GFAP + NFL + pTau181 + pTau217 + Age + Sex + Edu_Years + ApoE4 + MMSE
fit_mmse <- fit.mult.impute(formula = formula_mmse, fitter = lrm, xtrans = f, data = data)
nom_mmse <- nomogram(fit_mmse, fun = plogis, fun.at = seq(0.1, 0.9, by=0.1), funlabel = "Probability of CONV")

#==================================================
# Shiny UI
#==================================================
input_module <- function(var){
  wellPanel(
    numericInput(var, var, value = 0, min = -3, max = 30, step = 0.01),
    numericInput("Ab42", "Ab42:", value = 3, min = 0, max = 15, step = 0.01),
    numericInput("GFAP", "GFAP:", value = 50, min = 0, max = 600),
    numericInput("NFL", "NFL:", value = 30, min = 0, max = 150, step = 0.01),
    numericInput("pTau181", "pTau181:", value = 5, min = 0, max = 100, step = 0.1),
    numericInput("pTau217", "pTau217:", value = 50, min = 0, max = 300),
    numericInput("Age", "Age:", value = 70, min = 40, max = 100),
    selectInput("Sex", "Sex:", choices = c("Male"="0", "Female"="1"), selected="0"),
    numericInput("Edu_Years", "Education Years:", value = 12, min = 0, max = 25),
    selectInput("ApoE4", "ApoE4:", choices = c("Non-carrier"="0", "Carrier"="1"), selected="0")
  )
}

output_module <- function(var){
  wellPanel(
    hr(),
    actionButton(inputId = paste0("pred_with_", var), paste0("Update Probability")),
    hr(),
    h4(textOutput(paste0("result_with_", var)))
  )
}

ui <- fluidPage(
  navbarPage("", id = "navbar",
             tabPanel("MMSE", value = "MMSE", 
                      sidebarLayout(
                        sidebarPanel(width = 3, height = 1,
                                     input_module("MMSE")
                        ),
                        mainPanel(
                          plotOutput("nom_MMSE_plot"),
                          output_module("MMSE")
                        )
                      )
             ),
             tabPanel("ADNI_MEM", value = "ADNI_MEM", 
                      sidebarLayout(
                        sidebarPanel(width = 3, height = 1,
                                     input_module("ADNI_MEM")
                        ),
                        mainPanel(
                          plotOutput("nom_ADNI_MEM_plot"),
                          output_module("ADNI_MEM")
                        )
                      )
             ),
             tabPanel("ADNI_EF", value = "ADNI_EF", 
                      sidebarLayout(
                        sidebarPanel(width = 3, height = 1,
                                     input_module("ADNI_EF")
                        ),
                        mainPanel(
                          plotOutput("nom_ADNI_EF_plot"),
                          output_module("ADNI_EF")
                        )
                      )
             ),
             tabPanel("ADNI_LAN", value = "ADNI_LAN", 
                      sidebarLayout(
                        sidebarPanel(width = 3, height = 1,
                                     input_module("ADNI_LAN")
                        ),
                        mainPanel(
                          plotOutput("nom_ADNI_LAN_plot"),
                          output_module("ADNI_LAN")
                        )
                      )
             )
  ),
  mainPanel()
)

#==================================================
# Shiny Server
#==================================================
server <- function(input, output, session) {
  
  # Nomogram plots
  output$nom_MMSE_plot <- renderPlot({
    plot(nom_mmse)
  })
  output$nom_ADNI_MEM_plot <- renderPlot({
    plot(nom_mem)
  })
  output$nom_ADNI_EF_plot <- renderPlot({
    plot(nom_ef)
  })
  output$nom_ADNI_LAN_plot <- renderPlot({
    plot(nom_lan)
  })
  
  #----------------------------------------------------------------------------------------
  # ADNI_MEM
  #----------------------------------------------------------------------------------------
  observeEvent(input$pred_with_ADNI_MEM, {
    
    probMat <- Predict(
      fit_mem,
      Ab42 = input$Ab42,
      GFAP = input$GFAP,
      NFL = input$NFL,
      pTau181 = input$pTau181,
      pTau217 = input$pTau217,
      Age = input$Age,
      Sex = as.numeric(input$Sex),
      Edu_Years = input$Edu_Years,
      ApoE4 = as.numeric(input$ApoE4),
      ADNI_MEM = input$ADNI_MEM,
      fun = plogis
    )
    
    pred_vals <- probMat[["yhat"]]
    
    mean_prob <- mean(pred_vals)
    
    output$result_with_ADNI_MEM <- renderText({
      paste0("Predicted Probability of CONV = ", round(mean_prob, 4))
    })
  })
  
  #----------------------------------------------------------------------------------------
  # ADNI_EF
  #----------------------------------------------------------------------------------------
  observeEvent(input$pred_with_ADNI_EF, {
    probMat <- Predict(
      fit_ef,
      Ab42 = input$Ab42,
      GFAP = input$GFAP,
      NFL = input$NFL,
      pTau181 = input$pTau181,
      pTau217 = input$pTau217,
      Age = input$Age,
      Sex = as.numeric(input$Sex),
      Edu_Years = input$Edu_Years,
      ApoE4 = as.numeric(input$ApoE4),
      ADNI_EF = input$ADNI_EF,
      fun = plogis
    )
    pred_vals <- probMat[["yhat"]]
    mean_prob <- mean(pred_vals)
    
    output$result_with_ADNI_EF <- renderText({
      paste0("Predicted Probability of CONV = ", round(mean_prob, 4))
    })
  })
  
  #----------------------------------------------------------------------------------------
  # ADNI_LAN
  #----------------------------------------------------------------------------------------
  observeEvent(input$pred_with_ADNI_LAN, {
    probMat <- Predict(
      fit_lan,
      Ab42 = input$Ab42,
      GFAP = input$GFAP,
      NFL = input$NFL,
      pTau181 = input$pTau181,
      pTau217 = input$pTau217,
      Age = input$Age,
      Sex = as.numeric(input$Sex),
      Edu_Years = input$Edu_Years,
      ApoE4 = as.numeric(input$ApoE4),
      ADNI_LAN = input$ADNI_LAN,
      fun = plogis
    )
    pred_vals <- probMat[["yhat"]]
    mean_prob <- mean(pred_vals)
    
    output$result_with_ADNI_LAN <- renderText({
      paste0("Predicted Probability of CONV = ", round(mean_prob, 4))
    })
  })
  
  #----------------------------------------------------------------------------------------
  # MMSE
  #----------------------------------------------------------------------------------------
  observeEvent(input$pred_with_MMSE, {
    probMat <- Predict(
      fit_mmse,
      Ab42 = input$Ab42,
      GFAP = input$GFAP,
      NFL = input$NFL,
      pTau181 = input$pTau181,
      pTau217 = input$pTau217,
      Age = input$Age,
      Sex = as.numeric(input$Sex),
      Edu_Years = input$Edu_Years,
      ApoE4 = as.numeric(input$ApoE4),
      MMSE = input$MMSE,
      fun = plogis
    )
    pred_vals <- probMat[["yhat"]]
    mean_prob <- mean(pred_vals)
    
    output$result_with_MMSE <- renderText({
      paste0("Predicted Probability of CONV = ", round(mean_prob, 4))
    })
  })
  
}

#==================================================
# Run the Shiny App
#==================================================
if (interactive()) {
  shinyApp(ui, server)
}
