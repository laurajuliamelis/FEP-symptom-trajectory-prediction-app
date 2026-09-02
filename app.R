# SHINY APP: FarmaPRED-PEP Symptom Trajectory Predictor

## Load libraries
library(shiny)
library(ggplot2)
library(rms)
library(bslib)
library(thematic)
library(shinydashboard)
library(bsicons)
library(rsconnect)

## Setup theming
bs_global_set(
  bs_theme(
    bootswatch = "minty",
    base_font = font_google("Roboto"),
    heading_font = font_google("Poppins")
  )
)
thematic::thematic_shiny()

## Load model objects
load("data/model_objects.RData")

# ============================================================
# UI
# ============================================================

ui <- navbarPage(
  title = "FarmaPRED-PEP",
  collapsible = TRUE,
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Roboto"),
    heading_font = font_google("Poppins")
  ),
  
  tags$p(
    "DISCLAIMER: This tool is a research prototype and is not validated for clinical use.",
    style = "color: #c0392b; font-weight: bold; text-align:center; margin-top: 10px;"
  ),
  
  # ==========================================================
  # 1. Positive symptom trajectories
  # ==========================================================
  
  tabPanel(
    "Positive Symptom Trajectories",
    
    
    fluidPage(
      
      sidebarLayout(
        
        sidebarPanel(
          
          tags$h3(
            "Patient Inputs",
            style = "font-weight: bold; color: #287d6b;"
          ),
          
          numericInput(
            "PSFS_DUP",
            "Duration of untreated psychosis (DUP, days)",
            value = 0,
            min = 0,
            max = 1000,
            step = 1
          ),
          
          sliderInput(
            "PSFS_Insight",
            "Insight (PANSS G12; 1 = good, 7 = poor)",
            min = 1,
            max = 7,
            value = 1,
            step = 1
          ),
          
          radioButtons(
            "PSFS_FamilyHistory",
            "Family psychiatric history",
            choices = c(
              "No" = "No",
              "Yes" = "Sí"
            ),
            selected = "No"
          ),
          
          sliderInput(
            "PSFS_Functioning",
            "Global Assessment of Functioning (GAF)",
            min = 0,
            max = 100,
            value = 50,
            step = 1
          ),
          
          numericInput(
            "PSFS_TMT",
            "Processing speed (TMT-A, raw score)",
            value = 30,
            min = 0,
            step = 1
          ),
          
          tags$hr(),
          
          tags$h4(
            "Cognitive reserve",
            style = "font-weight: bold; color: #287d6b;"
          ),
          
          tags$p(
            "Cognitive reserve is calculated internally from premorbid IQ, education, and premorbid adjustment scores (PAS). The values are standardized using the PEPS cohort parameters and projected onto the PCA-derived cognitive reserve dimension."
          ),
          
          numericInput(
            "PSFS_PAS_childhood",
            "PAS score: childhood",
            value = 0,
            min = 0,
            step = 1
          ),
          
          numericInput(
            "PSFS_PAS_adolescence",
            "PAS score: early adolescence",
            value = 0,
            min = 0,
            step = 1
          ),
          
          numericInput(
            "PSFS_Education",
            "Education (years)",
            value = 0,
            min = 0,
            step = 1
          ),
          
          numericInput(
            "PSFS_IQ",
            "Estimated premorbid IQ",
            value = 100,
            min = 0,
            step = 1
          ),
          
          actionButton(
            "predict_PSFS",
            "Predict",
            class = "btn btn-primary"
          )
        ),
        
        mainPanel(
          
          tags$h2(
            "Positive Symptom Trajectory Predictor",
            style = "color: #287d6b; font-weight: bold;"
          ),
          
          tags$p(
            "This application estimates the probability that a patient belongs to the lower-score trajectory (LST) subgroup of longitudinal positive symptoms over one year of follow-up."
          ),
          
          tags$p(
            strong(
              "Please use the left panel to enter the patient's baseline characteristics and obtain the predicted trajectory subgroup."
            )
          ),
          
          tags$h2(
            "Results",
            style = "color: #287d6b; font-weight: bold;"
          ),
          
          accordion(
            
            accordion_panel(
              title = "Prediction",
              icon = bsicons::bs_icon("1-circle-fill"),
              
              fluidRow(
                column(
                  7,
                  uiOutput("PSFS_class_result")
                ),
                column(
                  5,
                  uiOutput("PSFS_prob_result")
                )
              )
            ),
            
            accordion_panel(
              title = "Performance of predicted LST probability in the reference dataset",
              icon = bsicons::bs_icon("2-circle-fill"),
              
              tags$p(
                "This density plot shows the distribution of predicted LST probabilities in the reference dataset according to the observed trajectory subgroup. The vertical line indicates the predicted probability for the current patient."
              ),
              
              plotOutput("PSFS_density_plot")
            ),
            
            id = "PSFS_acc",
            open = c(
              "Prediction",
              "Performance of predicted LST probability in the reference dataset"
            )
          )
        )
      )
    )
    
  ),
  
  # ==========================================================
  # 2. Negative symptom trajectories
  # ==========================================================
  
  tabPanel(
    "Negative Symptom Trajectories",
    
    fluidPage(
      
      sidebarLayout(
        
        sidebarPanel(
          
          tags$h3(
            "Patient Inputs",
            style = "font-weight: bold; color: #287d6b;"
          ),
          
          numericInput(
            "NSFS_DUP",
            "Duration of untreated psychosis (DUP, days)",
            value = 0,
            min = 0,
            max = 1000,
            step = 1
          ),
          
          sliderInput(
            "NSFS_Insight",
            "Insight (PANSS G12; 1 = good, 7 = poor)",
            min = 1,
            max = 7,
            value = 1,
            step = 1
          ),
          
          numericInput(
            "NSFS_VerbalFluency",
            "Verbal fluency (Animal Naming Test, raw score)",
            value = 20,
            min = 0,
            step = 1
          ),
          
          numericInput(
            "NSFS_MarderPositive",
            "Marder Positive Factor score",
            value = 20,
            min = 8,
            max = 56,
            step = 1
          ),
          
          actionButton(
            "predict_NSFS",
            "Predict",
            class = "btn btn-primary"
          )
        ),
        
        mainPanel(
          
          tags$h2(
            "Negative Symptom Trajectory Predictor",
            style = "color: #287d6b; font-weight: bold;"
          ),
          
          tags$p(
            "This application estimates the probability that a patient belongs to the lower-score trajectory (LST) subgroup of longitudinal negative symptoms over one year of follow-up."
          ),
          
          tags$p(
            strong(
              "Please use the left panel to enter the patient's baseline characteristics and obtain the predicted trajectory subgroup."
            )
          ),
          
          tags$h2(
            "Results",
            style = "color: #287d6b; font-weight: bold;"
          ),
          
          accordion(
            
            accordion_panel(
              title = "Prediction",
              icon = bsicons::bs_icon("1-circle-fill"),
              
              fluidRow(
                column(
                  7,
                  uiOutput("NSFS_class_result")
                ),
                column(
                  5,
                  uiOutput("NSFS_prob_result")
                )
              )
            ),
            
            accordion_panel(
              title = "Performance of predicted LST probability in the reference dataset",
              icon = bsicons::bs_icon("2-circle-fill"),
              
              tags$p(
                "This density plot shows the distribution of predicted LST probabilities in the reference dataset according to the observed trajectory subgroup. The vertical line indicates the predicted probability for the current patient."
              ),
              
              plotOutput("NSFS_density_plot")
            ),
            
            id = "NSFS_acc",
            open = c(
              "Prediction",
              "Performance of predicted LST probability in the reference dataset"
            )
          )
        )
      )
    )
    
  ),
  
  # ==========================================================
  # 3. Details
  # ==========================================================
  
  tabPanel(
    "Details",
    
    fluidPage(
      
      tags$h3(
        "About the FarmaPRED-PEP Symptom Trajectory Predictors",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "These prediction tools were developed within the FarmaPRED-PEP project to facilitate the application of statistical models designed to identify distinct longitudinal symptom trajectories in patients experiencing a first episode of psychosis (FEP)."
      ),
      
      tags$p(
        "Two clinical logistic regression models are implemented in the application: one for longitudinal positive symptom trajectories and one for longitudinal negative symptom trajectories. Both models predict membership in one of two trajectory subgroups over one year of follow-up."
      ),
      
      tags$h3(
        "Trajectory subgroups",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "For both symptom domains, patients were classified into two longitudinal trajectory subgroups according to their symptom scores over one year of follow-up:"
      ),
      
      tags$ul(
        tags$li(
          strong("Lower-score trajectory (LST): "),
          "patients with lower symptom scores over follow-up."
        ),
        tags$li(
          strong("Higher-score trajectory (HST): "),
          "patients with higher symptom scores over follow-up."
        )
      ),
      
      tags$p(
        "The LST subgroup represents the more favorable symptom trajectory, whereas the HST subgroup represents the less favorable trajectory."
      ),
      
      tags$h3(
        "Positive symptom trajectory model",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "The positive symptom trajectory model was developed using logistic regression and includes the following baseline predictors:"
      ),
      
      tags$ul(
        tags$li("Duration of untreated psychosis (DUP), in days."),
        tags$li("Insight, assessed using PANSS item G12."),
        tags$li("Family psychiatric history."),
        tags$li("Global Assessment of Functioning (GAF) score."),
        tags$li("Processing speed, assessed using the Trail Making Test Part A (TMT-A)."),
        tags$li("Cognitive reserve, derived from premorbid IQ, education, and premorbid adjustment scores.")
      ),
      
      tags$p(
        "Processing speed is standardized using the mean and standard deviation obtained from the PEPS control population. Cognitive reserve is calculated by standardizing the corresponding variables using the PEPS cohort parameters and projecting them onto the principal component obtained from the PCA."
      ),
      
      tags$h3(
        "Negative symptom trajectory model",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "The negative symptom trajectory model was developed using logistic regression and includes the following baseline predictors:"
      ),
      
      tags$ul(
        tags$li("Duration of untreated psychosis (DUP), in days."),
        tags$li("Insight, assessed using PANSS item G12."),
        tags$li("Verbal fluency, assessed using the Animal Naming Test."),
        tags$li("Marder Positive Factor score.")
      ),
      
      tags$p(
        "Verbal fluency is standardized using the mean and standard deviation obtained from the PEPS control population before being entered into the prediction model."
      ),
      
      tags$h3(
        "Model prediction",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "For each patient, the application calculates the predicted probability of belonging to the LST subgroup. A binary classification is then obtained using the prespecified model-specific probability threshold."
      ),
      
      tags$ul(
        tags$li(
          strong("LST: "),
          "predicted probability ≥ model-specific cutoff."
        ),
        tags$li(
          strong("HST: "),
          "predicted probability < model-specific cutoff."
        )
      ),
      
      tags$p(
        "The probability threshold was defined during model development and is specific to each prediction model."
      ),
      
      tags$h3(
        "Research article",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "A detailed description of the development and external validation of these prediction models will be provided in the corresponding research article."
      ),
      
      tags$p(
        strong("[Research article reference to be added upon publication]")
      ),
      
      
      
      tags$p(
        strong(
          "This tool is intended for research purposes only and must not be used for clinical diagnosis or treatment decisions."
        )
      )
    )
    
    
  ),
  
  # ==========================================================
  # 4. Contact, Credits and Funding
  # ==========================================================
  
  tabPanel(
    "Contact, Credits and Funding",
    
    
    fluidPage(
      
      br(),
      
      tags$h3(
        "About the FarmaPRED-PEP Project",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "This application was developed within the FarmaPRED-PEP project. The FarmaPRED-PEP project aims to develop predictive models for clinical outcomes and symptom trajectories in patients experiencing a first episode of psychosis (FEP). This work integrates clinical, neurocognitive, and other relevant patient characteristics to provide accessible prediction tools for research purposes."
      ),
      
      tags$p(
        "The project was supported by the Instituto de Salud Carlos III (ISCIII), co-funded by the European Union, and carried out by a multidisciplinary team from the University of Barcelona, IDIBAPS, and CIBERSAM."
      ),
      
      tags$h4(
        "Contact us",
        style = "color: #287d6b; font-weight: bold;"
      ),
      
      tags$p(
        "For questions, comments, or feedback regarding this tool or the research, please contact us at ",
        tags$a(
          href = "mailto:sergimash@ub.edu?subject=FarmaPRED-PEP%20Symptom%20Trajectory%20Tool",
          target = "_blank",
          "sergimash@ub.edu"
        ),
        " and ",
        tags$a(
          href = "mailto:laurajulia@ub.edu?subject=FarmaPRED-PEP%20Symptom%20Trajectory%20Tool",
          target = "_blank",
          "laurajulia@ub.edu"
        ),
        ". If you are interested in collaborating with us, do not hesitate to reach out!"
      )
    )
    
    
  ),
  
  hr(),
  
  # ==========================================================
  # Footer
  # ==========================================================
  
  tags$footer(
    
    
    tags$div(
      br(),
      
      tags$em(
        "This study (PMP21/00085) was funded by Instituto de Salud Carlos III (ISCIII) 
    and funded by the European Union (NextGenerationEU - Recovery and Resilience Facility)"
      ),
      
      br(),
      
      img(
        src = "logotip_farmapred.png",
        height = "40px",
        style = "float: center; margin-top: 5px; margin-bottom: 5px;"
      ),
      
      style = "width: 100%; color: black; text-align: center; font-weight: italic; color: #3b5773;"
    ),
    
    tags$div(
      HTML(
        "- Code available at <a href='https://github.com/laurajuliamelis/FEP-symptom-trajectory-prediction-app' target='_blank'><i class='bi bi-github'></i> GitHub</a> -"
      ),
      
      style = "width: 100%; color: black; text-align: center;"
    )
    
    
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # POSITIVE SYMPTOM TRAJECTORY MODEL
  # ==========================================================
  
  observeEvent(input$predict_PSFS, {
    
    
    # --------------------------------------------------------
    # 1. Standardize processing speed
    # --------------------------------------------------------
    
    processing_speed_z <-
      (input$PSFS_TMT - processing_speed_peps$Mean) /
      processing_speed_peps$sd
    
    
    # --------------------------------------------------------
    # 2. Calculate cognitive reserve
    # --------------------------------------------------------
    
    cognitive_data <- data.frame(
      EstimacionCI2meses = input$PSFS_IQ,
      añoseducación = input$PSFS_Education,
      PAS_infancia_adolescencia =
        input$PSFS_PAS_childhood +
        input$PSFS_PAS_adolescence
    )
    
    cognitive_vars <- c(
      "EstimacionCI2meses",
      "añoseducación",
      "PAS_infancia_adolescencia"
    )
    
    for (i in cognitive_vars) {
      
      cognitive_data[[i]] <-
        (cognitive_data[[i]] -
           scale_params[i, "Media"]) /
        scale_params[i, "SD"]
    }
    
    cognitive_reserve <-
      predict(res.pca, newdata = cognitive_data)[, 1]
    
    
    # --------------------------------------------------------
    # 3. Create new patient dataset
    # --------------------------------------------------------
    
    family_history <-
      factor(
        input$PSFS_FamilyHistory,
        levels = c("No", "Sí")
      )
    
    new_patient_PSFS <- data.frame(
      Antecedentes_psiquiátricos = family_history,
      DUP = input$PSFS_DUP,
      EEAG_Total_VB = input$PSFS_Functioning,
      Insight = input$PSFS_Insight,
      Reserva_Cognitiva = cognitive_reserve,
      tmt_A_seg2meses = processing_speed_z
    )
    
    
    # --------------------------------------------------------
    # 4. Predict probability of LST
    # --------------------------------------------------------
    
    prob_LST <-
      as.numeric(
        predict(
          model_clinica_LR_PSFS,
          newdata = new_patient_PSFS,
          type = "fitted"
        )
      )
    
    
    cutoff <- cutoff_clinical_LR_PSFS
    
    
    # --------------------------------------------------------
    # 5. Classification
    # --------------------------------------------------------
    
    predicted_class <-
      if (prob_LST >= cutoff) "LST" else "HST"
    
    
    # --------------------------------------------------------
    # 6. Prediction result
    # --------------------------------------------------------
    
    output$PSFS_class_result <- renderUI({
      
      value_box(
        title = "Predicted trajectory subgroup",
        value = predicted_class,
        
        theme = value_box_theme(
          bg = if (predicted_class == "LST") {
            "#00a86b"
          } else {
            "#D22B2B"
          },
          fg = "#FFFFFF"
        ),
        
        showcase =
          if (predicted_class == "LST") {
            bsicons::bs_icon("graph-up-arrow")
          } else {
            bsicons::bs_icon("graph-down-arrow")
          },
        
        showcase_layout = "left center",
        full_screen = FALSE,
        fill = TRUE
      )
    })
    
    
    # --------------------------------------------------------
    # 7. Probability result
    # --------------------------------------------------------
    
    output$PSFS_prob_result <- renderUI({
      
      value_box(
        title = "Probability of LST",
        value = paste0(
          round(prob_LST * 100, 2),
          "%"
        ),
        
        theme = value_box_theme(
          bg = "#f4f4f4",
          fg = "#555555"
        ),
        
        showcase = bsicons::bs_icon("calculator"),
        showcase_layout = "top right",
        full_screen = FALSE,
        fill = TRUE
      )
    })
    
    
    # --------------------------------------------------------
    # 8. Density plot
    # --------------------------------------------------------
    
    output$PSFS_density_plot <- renderPlot({
      
      plot_data <- data_clinical_imp_PSFS
      
      plot_data$Trajectory <-
        ifelse(
          plot_data$Cluster == "Responder",
          "LST",
          "HST"
        )
      
      plot_data$Trajectory <-
        factor(
          plot_data$Trajectory,
          levels = c("LST", "HST")
        )
      
      ggplot(
        plot_data,
        aes(
          x = Prediction,
          fill = Trajectory,
          color = Trajectory
        )
      ) +
        
        geom_density(
          alpha = 0.20,
          linewidth = 1
        ) +
        
        geom_vline(
          xintercept = prob_LST,
          linetype = "solid",
          linewidth = 1
        ) +
        
        annotate(
          "text",
          x = prob_LST,
          y = Inf,
          label = paste0(
            "Patient: p = ",
            round(prob_LST, 3)
          ),
          vjust = 1.5,
          hjust = 0.5,
          size = 4
        ) +
        
        labs(
          x = "Predicted probability of LST",
          y = "Density",
          fill = "Trajectory",
          color = "Trajectory"
        ) +
        
        scale_fill_manual(
          values = c(
            "LST" = "#00a86b",
            "HST" = "#D22B2B"
          )
        ) +
        
        scale_color_manual(
          values = c(
            "LST" = "#00a86b",
            "HST" = "#D22B2B"
          )
        ) +
        
        theme_classic() +
        
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(
            size = 14,
            face = "bold"
          ),
          legend.text = element_text(size = 12),
          legend.title = element_text(
            size = 14,
            face = "bold"
          )
        )
    })
    
    
  })
  
  # ==========================================================
  # NEGATIVE SYMPTOM TRAJECTORY MODEL
  # ==========================================================
  
  observeEvent(input$predict_NSFS, {
    
    
    # --------------------------------------------------------
    # 1. Standardize verbal fluency
    # --------------------------------------------------------
    
    verbal_fluency_z <-
      (input$NSFS_VerbalFluency -
         verbal_fluency_peps$Mean) /
      verbal_fluency_peps$sd
    
    
    # --------------------------------------------------------
    # 2. Create new patient dataset
    # --------------------------------------------------------
    
    new_patient_NSFS <- data.frame(
      DUP = input$NSFS_DUP,
      PSFS_VB = input$NSFS_MarderPositive,
      Insight = input$NSFS_Insight,
      animalesPDtotal2meses = verbal_fluency_z
    )
    
    
    # --------------------------------------------------------
    # 3. Predict probability of LST
    # --------------------------------------------------------
    
    prob_LST <-
      as.numeric(
        predict(
          model_clinica_LR_NSFS,
          newdata = new_patient_NSFS,
          type = "fitted"
        )
      )
    
    
    cutoff <- cutoff_clinical_LR_NSFS
    
    
    # --------------------------------------------------------
    # 4. Classification
    # --------------------------------------------------------
    
    predicted_class <-
      if (prob_LST >= cutoff) "LST" else "HST"
    
    
    # --------------------------------------------------------
    # 5. Prediction result
    # --------------------------------------------------------
    
    output$NSFS_class_result <- renderUI({
      
      value_box(
        title = "Predicted trajectory subgroup",
        value = predicted_class,
        
        theme = value_box_theme(
          bg = if (predicted_class == "LST") {
            "#00a86b"
          } else {
            "#D22B2B"
          },
          fg = "#FFFFFF"
        ),
        
        showcase =
          if (predicted_class == "LST") {
            bsicons::bs_icon("graph-up-arrow")
          } else {
            bsicons::bs_icon("graph-down-arrow")
          },
        
        showcase_layout = "left center",
        full_screen = FALSE,
        fill = TRUE
      )
    })
    
    
    # --------------------------------------------------------
    # 6. Probability result
    # --------------------------------------------------------
    
    output$NSFS_prob_result <- renderUI({
      
      value_box(
        title = "Probability of LST",
        value = paste0(
          round(prob_LST * 100, 2),
          "%"
        ),
        
        theme = value_box_theme(
          bg = "#f4f4f4",
          fg = "#555555"
        ),
        
        showcase = bsicons::bs_icon("calculator"),
        showcase_layout = "top right",
        full_screen = FALSE,
        fill = TRUE
      )
    })
    
    
    # --------------------------------------------------------
    # 7. Density plot
    # --------------------------------------------------------
    
    output$NSFS_density_plot <- renderPlot({
      
      plot_data <- data_clinical_imp_NSFS
      
      plot_data$Trajectory <-
        ifelse(
          plot_data$Cluster == "Responder",
          "LST",
          "HST"
        )
      
      plot_data$Trajectory <-
        factor(
          plot_data$Trajectory,
          levels = c("LST", "HST")
        )
      
      ggplot(
        plot_data,
        aes(
          x = Prediction,
          fill = Trajectory,
          color = Trajectory
        )
      ) +
        
        geom_density(
          alpha = 0.20,
          linewidth = 1
        ) +
        
        geom_vline(
          xintercept = prob_LST,
          linetype = "solid",
          linewidth = 1
        ) +
        
        annotate(
          "text",
          x = prob_LST,
          y = Inf,
          label = paste0(
            "Patient: p = ",
            round(prob_LST, 3)
          ),
          vjust = 1.5,
          hjust = 0.5,
          size = 4
        ) +
        
        labs(
          x = "Predicted probability of LST",
          y = "Density",
          fill = "Trajectory",
          color = "Trajectory"
        ) +
        
        scale_fill_manual(
          values = c(
            "LST" = "#00a86b",
            "HST" = "#D22B2B"
          )
        ) +
        
        scale_color_manual(
          values = c(
            "LST" = "#00a86b",
            "HST" = "#D22B2B"
          )
        ) +
        
        theme_classic() +
        
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(
            size = 14,
            face = "bold"
          ),
          legend.text = element_text(size = 12),
          legend.title = element_text(
            size = 14,
            face = "bold"
          )
        )
    })
    
    
  })
}

# ============================================================
# Run the app
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
