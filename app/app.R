library(shiny)
source("model_parameters.R", local = TRUE)
source("predict.R", local = TRUE)

ratio_fields <- c(nlr_8 = "NLR", lmr_8 = "LMR", plr_8 = "PLR")
count_fields <- c(neutrophile_8 = "Neutrophils", lymphocyte_8 = "Lymphocytes",
                  monocyte_8 = "Monocytes", platelet_8 = "Platelets")
number_input <- function(id, label, model_id = "model1", step = .01) {
  limits <- model_parameters$models[[model_id]]$limits[[id]]
  numericInput(id, label, value = round(limits["default"], 3),
               min = limits["min"], max = limits["max"], step = step)
}
colors <- c("#B3D9DE", "#8ABFC9", "#579BAA", "#307A8D", "#195467")

ui <- fluidPage(
  title = "Week 12 CDAI Explorer",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(class = "page-wrap",
    tags$header(class = "app-header",
      div(div(class = "eyebrow", "CROHN DISEASE / RESEARCH"),
          h1("Week 12 CDAI Explorer"),
          p("Explore predicted probabilities of disease activity after starting biologic therapy.")),
      div(class = "version-badge", "V2", tags$span("16 October 2026"))),
    div(class = "app-layout",
      tags$aside(class = "input-panel",
        h2("Model and inputs"),
        selectInput("model", "Prediction model", selectize = FALSE,
          choices = c("Model 1: blood count ratios" = "model1", "Model 2: absolute cell counts" = "model2")),
        h3("Baseline characteristics"),
        div(class = "input-pair", number_input("age_years", "Age (years)", step = 1),
            selectInput("gender", "Sex", choices = c("Male", "Female"), selectize = FALSE)),
        number_input("cdai_score_no", "Baseline CDAI", step = 1),
        selectInput("previous_ibd_related_surge", "Prior IBD-related surgery", choices = c("No", "Yes"), selectize = FALSE),
        h3("Week 8 biomarkers"),
        conditionalPanel("input.model === 'model1'",
          p(class = "field-note", "Enter ratios measured at week 8."),
          div(class = "input-pair", number_input("nlr_8", "NLR"), number_input("lmr_8", "LMR")),
          number_input("plr_8", "PLR", step = .1)),
        conditionalPanel("input.model === 'model2'",
          p(class = "field-note", "All cell counts are in 10", tags$sup("9"), "/L."),
          div(class = "input-pair", number_input("neutrophile_8", "Neutrophils", "model2"),
              number_input("lymphocyte_8", "Lymphocytes", "model2")),
          div(class = "input-pair", number_input("monocyte_8", "Monocytes", "model2"),
              number_input("platelet_8", "Platelets", "model2", step = 1))),
        actionButton("reset", "Reset inputs", class = "reset-button"),
        p(class = "field-note range-note", "Inputs are restricted to the observed development ranges.")),
      tags$main(class = "results-panel",
        div(class = "results-card",
          div(class = "section-top", div(h2("Predicted week 12 CDAI"), textOutput("model_name", inline = TRUE)),
              span(class = "live-badge", "Updates with your inputs")),
          uiOutput("probability_rows"),
          div(class = "result-summary", textOutput("interpretation")),
          downloadButton("download_results", "Download probabilities", class = "download-button")),
        div(class = "context-card",
          h2("Read the results"),
          p("Each row is the probability of one of five ordered CDAI groups. The probabilities sum to 100%."),
          p("These groups describe disease activity. They do not predict treatment failure or indicate when to switch treatment."),
          tags$details(tags$summary("Model details and interpretation"),
            p("Both models adjust for age, sex, baseline CDAI, and prior IBD surgery. Model 1 uses natural-log blood count ratios; Model 2 uses absolute counts."),
            p("The models were developed in 94 complete cases. Bootstrap-corrected C-statistics were 0.649 and 0.651. Proportional odds diagnostics suggested assumption departures."),
            p("This is an exploratory research prototype. External validation and assessment of clinical utility are required."),
            p("NLR: neutrophil-to-lymphocyte ratio. LMR: lymphocyte-to-monocyte ratio. PLR: platelet-to-lymphocyte ratio."))))),
    tags$footer(class = "app-footer", "Fixed V2 model parameters. No model refitting or patient dataset is used by this app.")
  )
)

server <- function(input, output, session) {
  values <- reactive({
    list(age_years = input$age_years, gender = input$gender, cdai_score_no = input$cdai_score_no,
         previous_ibd_related_surge = input$previous_ibd_related_surge,
         nlr_8 = input$nlr_8, lmr_8 = input$lmr_8, plr_8 = input$plr_8,
         neutrophile_8 = input$neutrophile_8, lymphocyte_8 = input$lymphocyte_8,
         monocyte_8 = input$monocyte_8, platelet_8 = input$platelet_8)
  })
  probabilities <- reactive({
    req(input$model)
    tryCatch(predict_cdai(input$model, values()),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$model_name <- renderText({
    if (input$model == "model1") "Model 1: blood count ratios" else "Model 2: absolute cell counts"
  })
  output$probability_rows <- renderUI({
    p <- probabilities()
    tagList(lapply(seq_along(p), function(i) {
      div(class = "probability-row",
        div(class = "probability-label", span(class = "group-number", paste("Group", i)),
            span(model_parameters$group_labels[i]),
            strong(sprintf("%.1f%%", p[i] * 100))),
        div(class = "bar-track", div(class = "bar-fill",
          style = sprintf("width: %.6f%%; background-color: %s;", p[i] * 100, colors[i]))))
    }))
  })
  output$interpretation <- renderText({
    p <- probabilities(); i <- which.max(p)
    sprintf("Highest probability: group %d (%s), %.1f%%.", i, model_parameters$group_labels[i], p[i] * 100)
  })
  observeEvent(input$reset, {
    updateSelectInput(session, "gender", selected = "Male")
    updateSelectInput(session, "previous_ibd_related_surge", selected = "No")
    for (model_id in names(model_parameters$models)) {
      for (v in names(model_parameters$models[[model_id]]$limits)) {
        updateNumericInput(session, v, value = round(model_parameters$models[[model_id]]$limits[[v]]["default"], 3))
      }
    }
  })
  output$download_results <- downloadHandler(
    filename = function() paste0("cdai-probabilities-", input$model, ".csv"),
    content = function(file) write.csv(data.frame(
      Model = input$model, Version = model_parameters$version,
      Group = 1:5, CDAI = model_parameters$group_labels, Probability = probabilities()), file, row.names = FALSE)
  )
}
shinyApp(ui, server)
