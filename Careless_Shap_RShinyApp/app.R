library(shiny)
library(tidyverse)
library(careless)
library(caret)
library(gbm)
library(ROCR)
library(shinydashboard)
library(DALEX)

df_question_description <-
  read.csv2("other/question_description.csv")
df_method_description <- read.csv2("other/method_description.csv")

# calculate performance (function)
perfResults <-
  function(true = NULL,
           predicted = NULL,
           predicted_proba = NULL) {
    auc = ModelMetrics::auc(actual = true, predicted = predicted)
    
    f1 = ModelMetrics::f1Score(actual = true, predicted = predicted)
    confMatrix = ModelMetrics::confusionMatrix(actual = true, predicted = predicted)
    acc = (confMatrix[1, 1] + confMatrix[2, 2]) / sum(confMatrix)
    
    sens = ModelMetrics::sensitivity(actual = true, predicted = predicted)
    spec = ModelMetrics::specificity(actual = true, predicted = predicted)
    return(list(
      AUC = auc,
      SENS = sens,
      SPEC = spec,
      F1 = f1,
      ACC = acc
    ))
  }

# load model
gbm_class <- readRDS("model/model_gbm_seed_1.rds")

# load train and test dataset
train_set <- readRDS("datasets/train_set_seed_1.rds")
test_set <- readRDS("datasets/test_set_seed_1.rds")

# FIRST TAB: COMPARISON
# Get GBM performance
predicted_proba = predict(gbm_class, test_set, type = "prob")[, 2]
predicted = predict(gbm_class, test_set)
predicted = ifelse(predicted == "ones", 1, 0)
res_GBM = perfResults(test_set$Careless, predicted, predicted_proba)

# Outcome class
class_careless = select(test_set, Careless)

# test dataset without outcome (for psychant, psychsyn and other Careless metrics calculation)
test_subset = select(test_set,-Careless)
test_subset = test_subset[-which(
  names(test_subset) %in% c(
    "time_p1",
    "time_p2",
    "time_p3",
    "time_p4",
    "time_p5",
    "time_p6",
    "longstr",
    "avgstr" ,
    "longstr1",
    "longstr6",
    "longstrDiff",
    "irvTotal",
    "irv1" ,
    "irv6",
    "irvDiff" ,
    "res_mahad" ,
    "res_psycsyn" ,
    "res_evenodd" ,
    "res_u3poly"
  )
)]


#### psychsyn calculation ####
psychsyn_cv = psychsyn_critval(x = test_subset, anto = FALSE)
thres_SYN <- 1
num_itemPairs <- sum(psychsyn_cv$Freq > thres_SYN, na.rm = TRUE)
while (num_itemPairs < 30 &&
       thres_SYN > 0) {
  # at least 30 such pairs
  thres_SYN <- thres_SYN - 0.01
  if (thres_SYN <= 0) {
    print("Threshold cannot be set. Incresing a number of included items might solve this issue.")
    return(NULL)
  }
  num_itemPairs <- sum(psychsyn_cv$Freq > thres_SYN, na.rm = TRUE)
}
subset_psych_SYN <- psychsyn(test_subset, critval = thres_SYN)
psychsyn_par_min = round(min(subset_psych_SYN), 2)
psychsyn_par_max = round(max(subset_psych_SYN))

#### psychant calculation ####
psychant_cv = psychsyn_critval(x = test_subset, anto = TRUE)
thres_ANT <- -1
num_itemPairs <- sum(psychant_cv$Freq < thres_ANT, na.rm = TRUE)
while (num_itemPairs < 30 &&
       thres_ANT < 0) {
  # at least 30 such pairs
  thres_ANT <- thres_ANT + 0.01
  if (thres_ANT >= 0) {
    print("Threshold cannot be set. Incresing a number of included items might solve this issue.")
    return(NULL)
  }
  num_itemPairs <- sum(psychant_cv$Freq < thres_ANT, na.rm = TRUE)
}
subset_psych_ANT <- psychsyn(test_subset, critval = thres_ANT)
psychant_par_min = round(min(subset_psych_ANT), 2)
psychant_par_max = round(max(subset_psych_ANT), 2)

#### longstring_par calculation ####
subset_longstring = longstring(x = test_subset, avg = F)
longstring_par_min = 1
longstring_par_max = 10

subset_longstring_AVG = longstring(x = test_subset, avg = T)$avgstr
longstringAVG_par_min = round(min(subset_longstring_AVG), 2)
longstringAVG_par_max = round(max(subset_longstring_AVG), 2)

# SECOND TAB: LOCAL INTERPRETABILITY (SHAP)
# Reading Dalex explainer
explain_gbm <-
  readRDS("Dalex explainers/explain_gbm_careless_seed_1.rds")
# Calculated in a similar way as demonstrated below:
#
# explain_gbm <- DALEX::explain(model = gbm_fit,
#                              data = train_set,
#                              y = train_set$Careless == "ones",
#                              label = "GBM")


# RShiny App
ui <- navbarPage(
  "CARELESS - Local interpretability",
  tabPanel(
    "GBM and Careless performance",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "Psychsyn_id",
          label = "Psychsyn:",
          min = psychsyn_par_min,
          max = psychsyn_par_max,
          value = psychsyn_par_min,
          step = round((psychsyn_par_max -
                          psychsyn_par_min) / 20, 2)
        ),
        sliderInput(
          inputId = "Psychant_id",
          label = "Psychant:",
          min = psychant_par_min,
          max = psychant_par_max,
          value = psychant_par_min,
          step = round((psychant_par_max -
                          psychant_par_min) / 20, 2)
        ),
        sliderInput(
          inputId = "Longstring_id",
          label = "Longstring:",
          min = longstring_par_min,
          max = longstring_par_max,
          value = longstring_par_min,
          step = round((longstring_par_max -
                          longstring_par_min) / 20, 2)
        ),
        sliderInput(
          inputId = "LongstringAVG_id",
          label = "Longstring (Average):",
          min = longstringAVG_par_min,
          max = round(mean(subset_longstring_AVG) +
                        3 * sd(subset_longstring_AVG), 2),
          value = longstringAVG_par_min,
          step = round((
            round(mean(subset_longstring_AVG) + 3 * sd(subset_longstring_AVG), 2) -
              longstringAVG_par_min
          ) / 20, 2)
        ),
        tableOutput("table_description")
      ),
      mainPanel(fluidRow(dataTableOutput(
        'table_performance'
      )))
    )
  ),
  tabPanel(
    "Local interpretability (SHAP)",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "class_rb",
          "Step 1: Display only responders that are:",
          c("Careless" = "car",
            "Regular" = "reg")
        ),
        radioButtons(
          "misclas_rb",
          "Step 2: Display only responders that were classified by GBM:",
          c("Incorrectly" = "yes",
            "Correctly" = "no")
        ),
        htmlOutput("testHTML"),
        textInput(
          inputId = "search",
          label = "Optional: Type in the code of a question to get its description:",
          value = ""
        ),
        tableOutput("table_question_description")
      ),
      mainPanel(
        box(
          status = "primary",
          height = "auto",
          width = "auto",
          solidHeader = T,
          column(
            width = 12,
            DT::dataTableOutput("table_t"),
            style = "height:auto; overflow-x: scroll;"
          )
        ),
        # DT::dataTableOutput("table_t"),
        plotOutput("plot_shap")
      )
    )
  )
)

server <- function(input, output) {
  reactive_subset <- reactiveVal(0)
  
  # FIRST TAB
  observe({
    predicted = ifelse(subset_psych_SYN <= input$Psychsyn_id, 1, 0)
    
    true = class_careless %>% unlist(use.names = FALSE)
    res_syn = perfResults(true = true, predicted = predicted)
    
    predicted = ifelse(subset_psych_ANT >= input$Psychant_id, 1, 0)
    true = class_careless %>% unlist(use.names = FALSE)
    res_ant = perfResults(true = true, predicted = predicted)
    
    predicted = ifelse(subset_longstring >= input$Longstring_id, 1, 0)
    true = class_careless %>% unlist(use.names = FALSE)
    res_longstring = perfResults(true = true, predicted = predicted)
    
    predicted = ifelse(subset_longstring_AVG >= input$LongstringAVG_id, 1, 0)
    true = class_careless %>% unlist(use.names = FALSE)
    res_longstringavg = perfResults(true = true, predicted = predicted)
    
    AUC = c(
      res_GBM$AUC,
      res_syn$AUC,
      res_ant$AUC,
      res_longstring$AUC,
      res_longstringavg$AUC
    )
    F1 = c(res_GBM$F1,
           res_syn$F1,
           res_ant$F1,
           res_longstring$F1,
           res_longstringavg$F1)
    SENS = c(
      res_GBM$SENS,
      res_syn$SENS,
      res_ant$SENS,
      res_longstring$SENS,
      res_longstringavg$SENS
    )
    SPEC = c(
      res_GBM$SPEC,
      res_syn$SPEC,
      res_ant$SPEC,
      res_longstring$SPEC,
      res_longstringavg$SPEC
    )
    
    output$table_performance <-
      renderDataTable(
        data.frame(
          Method = c(
            "GBM",
            "Psychsyn",
            "Psychant",
            "Longstring",
            "Longstring (Average)"
          ),
          AUC =  AUC,
          F1 = F1,
          SENS = SENS,
          SPEC = SPEC
        ),
        options = list(
          dom = 't',
          searching = FALSE,
          autoWidth = FALSE
        )
      )
  })
  
  output$testHTML <- renderText({
    paste("<b>Step 3: Choose a questionnaire you want to inspect (Click on a desired row).</b>")
  })
  
  # SECOND TAB
  observe({
    val <- input$search
    if (nchar(val) != 0) {
      wrds = str_detect(df_question_description$Question,
                        regex(input$search, ignore_case = T))
      output$table_question_description <-
        renderTable(df_question_description[wrds, ])
    } else{
      output$table_question_description <-
        renderTable(df_question_description[FALSE, ])
    }
  })
  # SECOND TAB
  output$table_description <- renderTable(df_method_description)
  
  output$table_t <- DT::renderDataTable({
    if (input$class_rb == "reg") {
      if (input$misclas_rb == "no") {
        reactive_subset(filter(test_set[which(predicted == test_set[, "Careless"]), ], Careless == 0))
        filter(test_set[which(predicted == test_set[, "Careless"]), ], Careless == 0)
      } else{
        reactive_subset(filter(test_set[which(predicted != test_set[, "Careless"]), ], Careless == 0))
        filter(test_set[which(predicted != test_set[, "Careless"]), ], Careless == 0)
      }
    } else{
      if (input$misclas_rb == "no") {
        reactive_subset(filter(test_set[which(predicted == test_set[, "Careless"]), ], Careless == 1))
        filter(test_set[which(predicted == test_set[, "Careless"]), ], Careless == 1)
      } else{
        reactive_subset(filter(test_set[which(predicted != test_set[, "Careless"]), ], Careless == 1))
        filter(test_set[which(predicted != test_set[, "Careless"]), ], Careless == 1)
      }
    }
    
  }, options = list(
    dom = 'p',
    searching = FALSE,
    autoWidth = FALSE
  ), server = TRUE, selection = "single")
  
  output$plot_shap <- renderPlot({
    tryCatch({
      shap_questionnaire <- NULL
      if (input$table_t_rows_selected) {
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...',
                     value = 0,
                     {
                       shap_questionnaire <- predict_parts(
                         explainer = explain_gbm,
                         new_observation =  reactive_subset()[input$table_t_rows_selected, ],
                         type = "shap",
                         B = 25
                       )
                     })
        plot(shap_questionnaire)
      } else{
        plot()
      }
    },
    error = function(e) {
      ""
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
