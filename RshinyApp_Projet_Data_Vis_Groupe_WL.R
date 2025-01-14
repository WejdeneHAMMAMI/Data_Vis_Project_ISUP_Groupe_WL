# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(scales)
library(shiny)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(caret)  # Pour la modélisation prédictive
library(DT)  # Pour les tables interactives
library(maps)
library(data.table)
library(formattable)
library(ROSE)
library(pROC)
library(GGally)
library(treemap)
library(randomForest)
library(DALEX)



loan_data <- read.csv("C:/Users/wejde/OneDrive/Bureau/ISUP/Data Viz/loan.csv", sep = ",")

# Fonction pour remplacer les NA dans un dataframe
replace_na <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
    } else {
      mode_value <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_value
    }
  }
  return(df)
}

loan_data <- replace_na(loan_data)
loan_data <- loan_data[sample(nrow(loan_data), size = floor(0.3 * nrow(loan_data))), ]

loan_data<- loan_data[,-c(1,2)]

# Calcul des quartiles de annual_inc
quartiles <- quantile(loan_data$annual_inc, probs = c(0.25, 0.75))
# Création des nouvelles colonnes
loan_data <- loan_data %>%
  mutate(
    loan_condition = case_when(
      loan_status %in% c("Fully Paid", "Current", "Issued", "Does not meet the credit policy. Status:Fully Paid") ~ "Good Loan",
      TRUE ~ "Bad Loan"
    ),
    income_category = case_when(
      annual_inc > quartiles[2] ~ "High",
      annual_inc < quartiles[1] ~ "Low",
      TRUE ~ "Medium"
    )
  )

#unbalanced dataset
#ggplot(data.frame(loan_data), aes(x=loan_data$loan_condition)) + geom_bar()
#dealing with unbalanced dataset

#over <- ovun.sample(loan_condition~., data = loan_data, method = "over")$data

loan_data <- loan_data %>%
  filter(!is.na(loan_status)) %>%
  mutate(
    default = ifelse(loan_status  %in% c("Fully Paid", "Current", "Issued", "Does not meet the credit policy. Status:Fully Paid" ), 0, 1),
    grade = factor(grade),
    emp_length = factor(emp_length, levels = unique(emp_length))
  )

# Préparation des données pour la carte
a <- data.table(table(loan_data$addr_state))
setnames(a, c("region", "count"))
a$region <- sapply(state.name[match(a$region, state.abb)], tolower)
all_states <- map_data("state")
Total <- merge(all_states, a, by = "region")



# Moyenne des taux d'intérêt par income_category et purpose
group_income_purpose <- loan_data %>%
  group_by(income_category, purpose) %>%
  summarise(mean_interest_rate = mean(int_rate, na.rm = TRUE), .groups = "drop")

# Moyenne du montant des prêts par income_category et purpose
group_dti_purpose <- loan_data %>%
  group_by(income_category, purpose) %>%
  summarise(mean_loan_amount = mean(loan_amnt, na.rm = TRUE), .groups = "drop")

# Extraction des valeurs de montant des prêts
loan_a <- group_dti_purpose$mean_loan_amount

# Ajout des montants de prêt totaux au premier tableau
new_groupby <- group_income_purpose %>%
  mutate(total_loan_amount = loan_a)

# Tri des données par income_category
sort_group_income_purpose <- new_groupby %>%
  arrange(income_category)
# Comptage des prêts par income_category, purpose et loan_condition
loan_count <- loan_data %>%
  group_by(income_category, purpose, loan_condition) %>%
  summarise(loan_c = n(), .groups = "drop")

# Séparation des "Good Loans" et "Bad Loans"
good_loans <- loan_count %>%
  filter(loan_condition == "Good Loan") %>%
  arrange(income_category)

bad_loans <- loan_count %>%
  filter(loan_condition == "Bad Loan") %>%
  arrange(income_category)
# Ajout des colonnes pour les nombres de prêts bons/mauvais, total et ratio
final_df <- sort_group_income_purpose %>%
  mutate(
    good_loans_count = good_loans$loan_c,
    bad_loans_count = bad_loans$loan_c,
    total_loans_issued = good_loans$loan_c + bad_loans$loan_c,
    bad_good_ratio_percent = round(bad_loans$loan_c / (good_loans$loan_c + bad_loans$loan_c) * 100, 2)
  ) %>%
  arrange(income_category)


data_na <- (colSums(is.na(loan_data)) / nrow(loan_data)) * 100
data_na <- data_na[data_na > 0]  
data_na <- data.frame(Variable=names(data_na), Missing_Ratio = data.frame(data_na)[,1])


# Fonction pour éliminer les outliers selon l'IQR
remove_outliers_iqr <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data <- data %>%
    filter(data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound)
  
  
}

# Appliquer la fonction pour int_rate, dti et annual_inc
loan_data <- loan_data %>%
  remove_outliers_iqr("int_rate") %>%
  remove_outliers_iqr("dti") %>%
  remove_outliers_iqr("annual_inc")


# Plot bar chart
ggplot(data_na, aes(x = Variable, y = Missing_Ratio)) +
  geom_bar(stat = "identity", fill = "grey", color = "black", alpha = 0.7) +
  labs(title = "Pourcentage de valeurs manquantes par variable", x = "Variable", y = "Missing Ratio (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pas de donnees manquantes

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Prédiction de défaut de paiement"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Présentation des données", tabName = "data_overview", icon = icon("info-circle")),
      menuItem("Analyse des données", tabName = "data_analysis", icon = icon("chart-bar")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      # Présentation des données
      tabItem(tabName = "data_overview",
              h2("Présentation du projet et des données"),
              fluidRow(
                box(
                  title = "Contexte du projet",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  "Ce projet utilise la base de données de prêts de LendingClub pour analyser et prédire la probabilité de défaut de paiement des emprunteurs.
                  Les données contiennent des informations détaillées sur les caractéristiques des prêts, les taux d'intérêt, les revenus des emprunteurs, et les antécédents professionnels, permettant une analyse approfondie des facteurs de risque."
                ),
                box(
                  title = "Aperçu de la base de données",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::dataTableOutput("data_preview")
                )
                
              )
      ),
      
      # Analyse des données
      tabItem(tabName = "data_analysis",
              fluidRow(
                box(
                  title = "Répartition des défauts",
                  width = 12,
                  plotlyOutput("default_distribution")
                ),
                box(
                  title = " Relation entre Loan Amount, Principal Received et Loan Condition",
                  width = 12,
                  plotlyOutput("loan_dist")
                ),
                box(
                  title = "Montants de pret par statut de propriétaire",
                  width = 12,
                  plotlyOutput("home")
                ),
                box(
                  title = "Distribution des montants de prêts par grade",
                  width = 12,
                  plotlyOutput("loan_amount_by_grade")
                )
              ),
              fluidRow(
                box(
                  title = "Taux d'intérêt par grade",
                  width = 12,
                  plotlyOutput("interest_rate_by_grade")
                ),
                box(
                  title = "Boxplot du Taux d'intérêt par grade et Term",
                  width = 12,
                  plotlyOutput("int_grade")
                ),
                
              ),
              fluidRow(
                box(
                  title = "Courbe de survie des défauts par grade",
                  width = 12,
                  plotOutput("survival_curve")
                )
              ),
              fluidRow(
                
                box(
                  title = "Relation entre les revenus annuels et le montant du prêt",
                  width = 12,
                  plotlyOutput("income_vs_loan")
                ),
                box(
                  title = "Loan counts in respective states",
                  width = 12,
                  plotOutput("loan_counts_map")  # Ajout du graphique de carte ici
                )
              ), fluidRow(
                box(
                  title = "Relation entre le revenu annuel moyen et le montant de prêt moyen par grade",
                  width = 12,
                  plotlyOutput("avg_income_vs_loan_by_grade")
                ))
              , fluidRow(
                box(
                  title = "Heatmap des corrélations",
                  width = 12,
                  plotlyOutput("heatmap_corr")
                ), 
                box(
                  title = "Taux d'intérêt moyens par statut de prêt",
                  width = 12,
                  plotlyOutput("intRate_status")
                ), 
                
              )
              
      ),
      
      # Prédiction
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "Prédire la probabilité de défaut",
                  width = 12,
                  selectInput("grade", "Grade", choices = unique(loan_data$grade)),
                  selectInput("emp_length", "Ancienneté professionnelle", choices = unique(loan_data$emp_length)),
                  selectInput("purpose", "Objectif", choices = unique(loan_data$purpose)),
                  selectInput("term", "Durée du prêt (en mois)", choices = unique(loan_data$term)),
                  numericInput("loan_amnt", "Montant du prêt", value = 10000),
                  numericInput("annual_inc", "Revenu annuel", value = 50000),
                  sliderInput("int_rate", "Taux d'intérêt", min = min(loan_data$int_rate), max = max(loan_data$int_rate), value = 10),
                  actionButton("predict_btn", "Prédire"),
                  verbatimTextOutput("prediction_result")
                )
              )
              
              ,
              fluidRow(
                box(
                  title = "Accuracy du modèle GLM",
                  width = 12,
                  verbatimTextOutput("glm_accuracy_result")
                ),
                box(
                  title = "Matrice de confusion GLM",
                  width = 12,
                  plotOutput("confusion_plot_glm")
                ),
                
                box(
                  title = "Accuracy du modèle RF",
                  width = 12,
                  verbatimTextOutput("rf_accuracy_result")
                ),
                box(
                  title = "Matrice de confusion RF",
                  width = 12,
                  plotOutput("confusion_plot_rf")
                )
              ),
              
              fluidRow(
                
                box(
                  title = "Importance des Variables",
                  width = 12,
                  plotOutput("vi_rf_plot")
                ), 
                box(
                  title = "Dépendances Partielles",
                  width = 12,
                  plotOutput("pdp_rf_plot")
                )
                
              )
              
              
              
              
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Aperçu des données
  output$data_preview <- DT::renderDataTable({
    DT::datatable(head(loan_data, 100), options = list(pageLength = 10))
  })
  
  # Analyse des données
  output$default_distribution <- renderPlotly({
    plot_data <- loan_data %>%
      group_by(default) %>%
      summarise(count = n())
    
    plot_ly(plot_data, x = ~default, y = ~count, type = 'bar', color = ~factor(default),
            colors = c("#FF4136", "#0074D9")) %>%
      layout(title = "Répartition des prêts par état", xaxis = list(title = "Défaut"), yaxis = list(title = "Nombre de prêts"))
  })

  output$loan_dist <- renderPlotly({
    sample_data <- loan_data[sample(nrow(loan_data), 1000), c("total_rec_prncp", "loan_condition", "loan_amnt")]
    ggpairs(sample_data, mapping = aes(color = loan_condition))
  })
  
  output$home <- renderPlotly({
    mort_df <- loan_data %>% select(home_ownership, grade, sub_grade)
    mort_df <- mort_df[mort_df$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), 
    ]
    
    g_mort <- ggplot(mort_df, aes(grade))
    g_mort + geom_bar(aes(fill = grade)) + facet_wrap(~home_ownership) + labs(x = "Grade", 
                                                                              y = "Number of Loans", title = "Issued Loans of Different Home Ownership") + 
      theme_bw()
  })
  
  
  output$loan_amount_by_grade <- renderPlotly({
    plot_ly(loan_data, x = ~grade, y = ~loan_amnt, type = "box", color = ~grade) %>%
      layout(title = "Montants des prêts par grade", xaxis = list(title = "Grade"), yaxis = list(title = "Montant du prêt"))
  })
  
  output$interest_rate_by_grade <- renderPlotly({
    plot_ly(loan_data, x = ~grade, y = ~int_rate, type = "box", color = ~grade) %>%
      layout(title = "Taux d'intérêt par grade", xaxis = list(title = "Grade"), yaxis = list(title = "Taux d'intérêt"))
  })
  
  output$int_grade <- renderPlotly({
    give_count <- 
      stat_summary(fun.data = function(x) return(c(y = median(x)*1.06,
                                                   label = length(x))),
                   geom = "text")
    
    give_mean <- 
      stat_summary(fun.y = mean, colour = "darkgreen", geom = "point", 
                   shape = 18, size = 3, show.legend = FALSE)
    
    loan_data %>%
      ggplot(aes(grade, int_rate)) +
      geom_boxplot(fill = "white", colour = "darkblue", 
                   outlier.colour = "red", outlier.shape = 1) +
      give_count +
      give_mean +
      scale_y_continuous(labels = comma) +
      labs(title="Interest Rate by Grade", x = "Grade", y = "Interest Rate \n") +
      facet_wrap(~ term)
  })
  
  
  output$correlation_matrix <- renderPlotly({
    numeric_cols <- loan_data %>% select_if(is.numeric)  %>% na.omit()   # Sélectionner uniquement les colonnes numériques
    
    corr_matrix <- round(cor(numeric_cols, use = "complete.obs"), 2)
    ggcorrplot(corr_matrix, method = "circle", hc.order = TRUE, type = "lower", lab = TRUE) %>%
      ggplotly()
  })
  
  output$survival_curve <- renderPlot({
    surv_data <- loan_data %>% mutate(time = as.numeric(loan_amnt)) # Utilisation du montant du prêt comme approximation du temps
    fit <- survfit(Surv(time, default) ~ grade, data = surv_data)
    ggsurvplot(fit, data = surv_data, pval = TRUE, conf.int = TRUE,
               title = "Courbe de survie par grade",
               xlab = "Durée (jours)", ylab = "Probabilité de survie")
  })
  
  output$emp_length_density <- renderPlotly({
    density_plot <- ggplot(loan_data, aes(x = emp_length, fill = factor(default))) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
      labs(title = "Densité de l'ancienneté professionnelle selon le statut de défaut", x = "Ancienneté", y = "Densité")
    ggplotly(density_plot)
  })
  
  output$income_vs_loan <- renderPlotly({
    scatter_plot <- ggplot(loan_data, aes(x = annual_inc, y = loan_amnt, color = factor(default))) +
      geom_point(alpha = 0.4) +
      scale_x_log10() +
      scale_y_log10() +
      labs(title = "Relation entre le revenu annuel et le montant du prêt", x = "Revenu annuel", y = "Montant du prêt")
    ggplotly(scatter_plot)
  })
  # Graphique de carte : Loan counts in respective states
  output$loan_counts_map <- renderPlot({
    ggplot(Total, aes(x = long, y = lat, map_id = region)) + 
      geom_map(aes(fill = count), map = all_states) +
      labs(title = "Loan counts in respective states", x = "", y = "") +
      scale_fill_gradientn("", colours = terrain.colors(10), guide = "legend") +
      theme_bw()
  })
  
  output$avg_income_vs_loan_by_grade <- renderPlotly({
    # Agrégation des données pour calculer les moyennes
    data_summary <- loan_data %>%
      group_by(grade) %>%
      summarise(
        avg_annual_inc = mean(annual_inc, na.rm = TRUE),
        avg_loan_amnt = mean(loan_amnt, na.rm = TRUE)
      )
    
    # Création du graphique interactif
    plot_ly(data_summary, 
            x = ~avg_annual_inc, 
            y = ~avg_loan_amnt, 
            type = 'scatter', 
            mode = 'markers',
            color = ~grade, 
            size = ~avg_loan_amnt, 
            marker = list(opacity = 0.7, sizemode = 'diameter')) %>%
      layout(
        title = "Relation entre le revenu annuel moyen et le montant de prêt moyen par grade",
        xaxis = list(title = "Average Annual Income", tickformat = "$,"),
        yaxis = list(title = "Average Loan Amount", tickformat = "$,"),
        showlegend = TRUE
      )
  })
  
  # Heatmap des corrélations
  output$heatmap_corr <- renderPlotly({
    numeric_vars <- loan_data %>%
      select_if(is.numeric) %>%
      na.omit()
    corr_matrix <- cor(numeric_vars, use = "complete.obs")
    
    heatmap_plot <- plot_ly(
      z = corr_matrix,
      x = colnames(corr_matrix),
      y = colnames(corr_matrix),
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(title = "Heatmap des corrélations")
    
    heatmap_plot
  })
  
  output$intRate_status<- renderPlotly({
    # Taux d'intérêt pour les "Good Loans"
    avg_fully_paid <- loan_data %>% 
      filter(loan_status == "Fully Paid") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_current <- loan_data %>% 
      filter(loan_status == "Current") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_issued <- loan_data %>% 
      filter(loan_status == "Issued") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_long_fully_paid <- loan_data %>% 
      filter(loan_status == "Does not meet the credit policy. Status:Fully Paid") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    # Taux d'intérêt pour les "Bad Loans"
    avg_default_rates <- loan_data %>% 
      filter(loan_status == "Default") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_charged_off <- loan_data %>% 
      filter(loan_status == "Charged Off") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_long_charged_off <- loan_data %>% 
      filter(loan_status == "Does not meet the credit policy. Status:Charged Off") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_grace_period <- loan_data %>% 
      filter(loan_status == "In Grace Period") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_short_late <- loan_data %>% 
      filter(loan_status == "Late (16-30 days)") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    avg_long_late <- loan_data %>% 
      filter(loan_status == "Late (31-120 days)") %>% 
      summarise(avg_int_rate = round(mean(int_rate, na.rm = TRUE), 2)) %>% 
      pull(avg_int_rate)
    
    # Préparation des données pour les graphiques polaires
    data_good_loans <- data.frame(
      r = c(avg_fully_paid, avg_current, avg_issued, avg_long_fully_paid),
      theta = c("Fully Paid", "Current", "Issued", "No C.P. Fully Paid")
    )
    
    data_bad_loans <- data.frame(
      r = c(avg_default_rates, avg_charged_off, avg_long_charged_off, avg_grace_period, avg_short_late, avg_long_late),
      theta = c("Default", "Charged Off", "C.P. Charged Off", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)")
    )
    
    # Création des graphiques polaires avec Plotly
    fig <- subplot(
      plot_ly(
        data_good_loans, 
        r = ~r, 
        theta = ~theta, 
        type = "scatterpolar", 
        mode = "lines+markers", 
        fill = "toself", 
        name = "Good Loans",
        line = list(color = "#63AF63"),
        marker = list(color = "#B3FFB3", size = 8, symbol = "square")
      ),
      plot_ly(
        data_bad_loans, 
        r = ~r, 
        theta = ~theta, 
        type = "scatterpolar", 
        mode = "lines+markers", 
        fill = "toself", 
        name = "Bad Loans",
        line = list(color = "#C31414"),
        marker = list(color = "#FF5050", size = 8, symbol = "square")
      ),
      nrows = 1,
      shareY = TRUE
    ) %>%
      layout(
        title = "Average Interest Rates <br> Loan Status Distribution",
        showlegend = TRUE,
        polar = list(
          radialaxis = list(tickfont = list(size = 8)),
          angularaxis = list(tickfont = list(size = 8), rotation = 90, direction = "counterclockwise")
        ),
        polar2 = list(
          radialaxis = list(tickfont = list(size = 8)),
          angularaxis = list(tickfont = list(size = 8), rotation = 90, direction = "clockwise")
        )
      )
    
    fig
  })
  
  
  
  
  
  #})
  
  # Modèle de prédiction

  
  observeEvent(input$predict_btn, {
    
    # Préparation des données

    model_data <- loan_data %>%
      select(default, int_rate, grade, dti, emp_length, term, purpose, loan_amnt, annual_inc) %>%
      mutate(
        default = as.factor(default),
        grade = as.factor(grade),
        emp_length = as.factor(emp_length),
        purpose = as.factor(purpose),
        term = as.factor(term)
      ) %>%
      na.omit()
 
    
    # Séparation en train et test (80% train, 20% test)
    set.seed(123)
    train_index <- createDataPartition(model_data$default, p = 0.8, list = FALSE)
    train <- model_data[train_index, ]
    test <- model_data[-train_index, ]
    
    #perform one-hot encoding on train and test
    dummyTrain <- dummyVars(" ~ .", data=train[,-c(1)])
    train_data <- data.frame(predict(dummyTrain, newdata=train))
    train_data <-cbind(train[,c(1)], train_data)
    colnames(train_data)[1] = "default"
    
    dummyTest <- dummyVars(" ~ .", data=test[,-c(1)])
    test_data <- data.frame(predict(dummyTest, newdata=test))
    test_data <-cbind(test[,c(1)], test_data)
    colnames(test_data)[1] = "default"
    
    
    # Équilibrage des classes
    train_data_balanced <- ROSE(default ~ ., data = train_data, seed = 123)$data
    train_data_balanced$default <- as.factor(train_data_balanced$default)
    test_data$default <- as.factor(test_data$default)
    # Modèle GLM
    logit_model <- glm(default~.
      #default ~ int_rate + grade + dti + emp_length + term + purpose + loan_amnt + annual_inc
      , data = train_data_balanced, family = binomial)
    
    # Prédictions et évaluation GLM
    test_predictions_glm <- predict(logit_model, test_data, type = "response")
    test_pred_class_glm <- ifelse(test_predictions_glm > 0.5, 1, 0)
    confusion_glm <- confusionMatrix(as.factor(test_pred_class_glm), test_data$default)
    
    output$glm_accuracy_result <- renderText({
      paste0("Accuracy du modèle GLM : ", round(confusion_glm$overall['Accuracy'] * 100, 2), "%")
    })
    
    # Modèle Random Forest
    rf_model <- randomForest(
      default ~ .
      , data = train_data_balanced, ntree = 100, importance = TRUE)
    
    # Prédictions et évaluation Random Forest
    test_pred_class_rf <- predict(rf_model, test_data, type = "response")
   
    confusion_rf <- confusionMatrix(test_pred_class_rf, test_data$default)
    
    output$rf_accuracy_result <- renderText({
      paste0("Accuracy du modèle Random Forest : ", round(confusion_rf$overall['Accuracy'] * 100, 2), "%")
    })
    
   
    output$confusion_plot_glm <- renderPlot({
      confusion_df_glm <- as.data.frame(confusion_glm$table)
      ggplot(confusion_df_glm, aes(x = Prediction, y = Reference, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white", size = 6) +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Matrice de Confusion GLM", x = "Prédictions", y = "Références") +
        theme_minimal()
    })
    
    output$confusion_plot_rf <- renderPlot({
      confusion_df_rf <- as.data.frame(confusion_rf$table)
      ggplot(confusion_df_rf, aes(x = Prediction, y = Reference, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white", size = 6) +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Matrice de Confusion Random Forest", x = "Prédictions", y = "Références") +
        theme_minimal()
    })
   
    #Explicabilité du modele RF 
    
    # One-hot encoding des variables catégorielles
    dummy_vars <- dummyVars(" ~ .", data = train_data_balanced[, -1])
    train_data_numeric <- data.frame(predict(dummy_vars, newdata = train_data_balanced[, -1]))
    
    # Créer l'explicateur DALEX
    explainer_rf <- explain(
      model = rf_model,
      data = train_data_numeric,
      y = as.numeric(as.character(train_data_balanced$default)),
      label = "Random Forest"
    )
    
    # Importance des variables
    output$vi_rf_plot <- renderPlot({
      vi_rf <- model_parts(explainer_rf)
      plot(vi_rf)
    })
    
    # Profils de dépendance partielle
    output$pdp_rf_plot <- renderPlot({
      pdp_rf <- model_profile(explainer_rf, variables = c("int_rate", "loan_amnt", "annual_inc"))
      plot(pdp_rf)
    })
  })
  
  
  
}
shinyApp(ui = ui, server = server)

###### fin Rshiny App













##################################################################
#figures que j'ai pas réussi à les mettre dans l'appli
##################################################################
# Filtrer les données pour chaque catégorie de revenu
high_income <- final_df %>%
  filter(income_category == "High") %>%
  select(purpose, mean_interest_rate)

medium_income <- final_df %>%
  filter(income_category == "Medium") %>%
  select(purpose, mean_interest_rate)

low_income <- final_df %>%
  filter(income_category == "Low") %>%
  select(purpose, mean_interest_rate)

# Créer le graphique avec plusieurs traces
fig2 <- plot_ly()

fig2 <- fig2 %>%
  add_trace(
    data = high_income,
    x = ~mean_interest_rate,
    y = ~purpose,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#0040FF", size = 12),
    name = "High Income"
  ) %>%
  add_trace(
    data = medium_income,
    x = ~mean_interest_rate,
    y = ~purpose,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#FE9A2E", size = 12),
    name = "Medium Income"
  ) %>%
  add_trace(
    data = low_income,
    x = ~mean_interest_rate,
    y = ~purpose,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#FE2E2E", size = 12),
    name = "Low Income"
  )

# Personnaliser le layout
fig2 <- layout(
  fig2,
  title = list(text = "Average Purpose Interest Rate <br><i>by Income Category</i>"),
  xaxis = list(title = "Average Interest Rate"),
  yaxis = list(title = ""),
  legend = list(orientation = "h", x = 0.3, y = -0.1)
)

# Retourner le graphique
fig2

##################################################################
# Create the treemap
library(treemap)
prp_df <- loan_data %>%
  select(purpose, loan_amnt) %>%
  na.omit() %>%
  group_by(purpose) %>%
  summarise(
    volume = n(),
    average_amnt = sum(as.numeric(loan_amnt), na.rm = TRUE) / n()
  )


prp_df <- prp_df[!prp_df$purpose == "", ]

treemap(
  prp_df,
  
  index = "purpose",  # Column defining categories (purpose)
  vSize = "volume",     # Column defining size of rectangles (volume)
  vColor = "average_amnt", # Column defining color of rectangles (average_amnt)
  range = c(6000, 20000),  # Color range for average amount
  type = "manual",       # Specify manual color definition
  palette = c("yellow", "green", "orange", "orange2", "firebrick"), # Color palette
  algorithm = "pivotSize", # Treemap layout algorithm
  sortID = "-size",       # Sort categories by size (descending)
  title = "Purposes of Loans",
  title.legend = "Avg_Amnt",   # Legend title
  fontfamily.labels = "serif",  # Font family for labels
  fontsize.labels = 10,      # Font size for labels
  fontsize.legend = 8,     # Font size for legend
  fontface.labels = 1,       # Font weight for labels (bold)
  position.legend = "bottom",  # Legend position
  force.print.labels = TRUE,   # Force printing of labels
  border.col = "white"       # Border color of rectangles
)

library(DescTools)
Desc(loan_data$loan_amnt, main = "Loan amount distribution", plotit = TRUE)


##################################################################
### dans server ##
output$incPurp <-  DT::renderDataTable({
  
  # Création du tableau DT avec formatage conditionnel
  DT::datatable(final_df, options = list(pageLength = 10)) %>%
    DT::formatStyle(
      'bad_good_ratio_percent',
      backgroundColor = DT::styleInterval(
        c(15, 25),          # Seuils pour les intervalles de couleurs
        c('yellow', 'orange', 'red')  # Couleurs correspondantes
      ),
      color = 'black'       # Couleur du texte pour assurer la lisibilité
    )
  
  
})



