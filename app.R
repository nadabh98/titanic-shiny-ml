# ============================================================
# TITANIC - APPLICATION SHINY
# Comparaison Régression Logistique / Random Forest
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(titanic)
library(randomForest)


# ============================================================
# 2. DONNEES TITANIC
# ============================================================

titanic <- titanic_train


# ============================================================
# 3. PREPARATION DES DONNEES
# ============================================================

titanic <- titanic %>%
  mutate(
    Age = ifelse(
      is.na(Age),
      median(Age, na.rm = TRUE),
      Age
    ),
    Sex = as.factor(Sex),
    Pclass = as.factor(Pclass),
    Embarked = ifelse(
      is.na(Embarked) | Embarked == "",
      "S",
      Embarked
    ),
    Embarked = as.factor(Embarked)
  )


# ============================================================
# 4. VARIABLES UTILISEES POUR LES MODELES
# ============================================================

titanic_model <- titanic %>%
  select(
    Survived,
    Pclass,
    Sex,
    Age,
    SibSp,
    Parch
  ) %>%
  mutate(
    Survived = factor(
      Survived,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    )
  )


# ============================================================
# 5. SEPARATION TRAIN / TEST
# ============================================================

set.seed(123)

n <- nrow(titanic_model)

train_index <- sample(
  1:n,
  size = 0.8 * n
)

train <- titanic_model[train_index, ]

test <- titanic_model[-train_index, ]


# ============================================================
# 6. REGRESSION LOGISTIQUE
# ============================================================

modele <- glm(
  Survived ~ Pclass + Sex + Age + SibSp + Parch,
  data = train,
  family = binomial
)


# ============================================================
# 7. RANDOM FOREST
# ============================================================

set.seed(123)

modele_rf <- randomForest(
  Survived ~ Pclass + Sex + Age + SibSp + Parch,
  data = train,
  ntree = 500
)


# ============================================================
# 8. PREDICTIONS SUR LE JEU DE TEST
# ============================================================

probabilites <- predict(
  modele,
  newdata = test,
  type = "response"
)

predictions <- ifelse(
  probabilites >= 0.5,
  "Oui",
  "Non"
)

predictions <- factor(
  predictions,
  levels = c("Non", "Oui")
)


predictions_rf <- predict(
  modele_rf,
  newdata = test
)

predictions_rf <- factor(
  predictions_rf,
  levels = c("Non", "Oui")
)


# ============================================================
# 9. ACCURACY
# ============================================================

accuracy_logistique <- mean(
  predictions == test$Survived
)

accuracy_rf <- mean(
  predictions_rf == test$Survived
)


# ============================================================
# 10. ANALYSE DES ERREURS
# ============================================================

test_resultats <- test %>%
  mutate(
    prediction_logistique = predictions,
    prediction_rf = predictions_rf
  )


# ------------------------------------------------------------
# Erreurs - Régression logistique
# ------------------------------------------------------------

resultats_erreurs_logistique <- test_resultats %>%
  group_by(Pclass) %>%
  summarise(
    faux_positifs = sum(
      Survived == "Non" &
        prediction_logistique == "Oui"
    ),
    
    vrais_negatifs = sum(
      Survived == "Non"
    ),
    
    faux_negatifs = sum(
      Survived == "Oui" &
        prediction_logistique == "Non"
    ),
    
    vrais_positifs = sum(
      Survived == "Oui"
    ),
    
    taux_faux_positifs =
      faux_positifs / vrais_negatifs,
    
    taux_faux_negatifs =
      faux_negatifs / vrais_positifs
  )


# ------------------------------------------------------------
# Erreurs - Random Forest
# ------------------------------------------------------------

resultats_erreurs_rf <- test_resultats %>%
  group_by(Pclass) %>%
  summarise(
    faux_positifs = sum(
      Survived == "Non" &
        prediction_rf == "Oui"
    ),
    
    vrais_negatifs = sum(
      Survived == "Non"
    ),
    
    faux_negatifs = sum(
      Survived == "Oui" &
        prediction_rf == "Non"
    ),
    
    vrais_positifs = sum(
      Survived == "Oui"
    ),
    
    taux_faux_positifs =
      faux_positifs / vrais_negatifs,
    
    taux_faux_negatifs =
      faux_negatifs / vrais_positifs
  )


# ============================================================
# 11. INTERFACE SHINY
# ============================================================

ui <- fluidPage(
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  
  # ----------------------------------------------------------
  # TITRE
  # ----------------------------------------------------------
  
  div(
    style = "
      text-align:center;
      padding:30px 20px;
      margin-bottom:25px;
    ",
    
    h1(
      "🚢 Titanic — Prédiction de survie",
      style = "
        font-weight:700;
        margin-bottom:10px;
      "
    ),
    
    p(
      "Analyse comparative entre Régression logistique et Random Forest",
      style = "
        font-size:18px;
        color:#666;
      "
    )
  ),
  
  
  # ----------------------------------------------------------
  # PROFIL DU PASSAGER
  # ----------------------------------------------------------
  
  card(
    
    style = "margin-bottom:25px;",
    
    card_header("👤 Profil du passager"),
    
    div(
      
      style = "padding:25px;",
      
      fluidRow(
        
        column(
          6,
          
          selectInput(
            "pclass",
            "🎫 Classe du passager",
            
            choices = c(
              "🥇 1ère classe" = 1,
              "🥈 2ème classe" = 2,
              "🥉 3ème classe" = 3
            ),
            
            selected = 3
          )
        ),
        
        
        column(
          6,
          
          radioButtons(
            "sex",
            "👤 Sexe",
            
            choices = c(
              "👩 Femme" = "female",
              "👨 Homme" = "male"
            ),
            
            selected = "female",
            inline = TRUE
          )
        )
      ),
      
      
      fluidRow(
        
        column(
          4,
          
          sliderInput(
            "age",
            "🎂 Âge",
            
            min = 0,
            max = 80,
            value = 30,
            step = 1
          )
        ),
        
        
        column(
          4,
          
          numericInput(
            "sibsp",
            "👨‍👩‍👧 Frères / sœurs / conjoint",
            
            value = 0,
            min = 0,
            max = 8
          )
        ),
        
        
        column(
          4,
          
          numericInput(
            "parch",
            "👨‍👩‍👧 Parents / enfants",
            
            value = 0,
            min = 0,
            max = 6
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # ONGLETS
  # ==========================================================
  
  tabsetPanel(
    
    id = "onglets",
    
    
    # ========================================================
    # ONGLET 1 - PREDICTION
    # ========================================================
    
    tabPanel(
      
      "🎯 Prédiction",
      
      br(),
      
      h3(
        "Prédiction pour le passager sélectionné"
      ),
      
      p(
        "Les deux modèles analysent exactement le même profil."
      ),
      
      br(),
      
      
      fluidRow(
        
        # ----------------------------------------------------
        # LOGISTIQUE
        # ----------------------------------------------------
        
        column(
          6,
          
          card(
            
            card_header(
              "📊 Régression logistique"
            ),
            
            div(
              
              style = "
                text-align:center;
                padding:30px;
              ",
              
              h4("Prédiction"),
              
              h2(
                textOutput(
                  "prediction_logistique"
                ),
                
                style = "
                  font-size:30px;
                  font-weight:bold;
                  margin:20px;
                "
              ),
              
              hr(),
              
              h4(
                "Probabilité de survie"
              ),
              
              h2(
                textOutput(
                  "probabilite_logistique"
                ),
                
                style = "
                  font-size:32px;
                  margin-top:20px;
                "
              )
            )
          )
        ),
        
        
        # ----------------------------------------------------
        # RANDOM FOREST
        # ----------------------------------------------------
        
        column(
          6,
          
          card(
            
            card_header(
              "🌲 Random Forest"
            ),
            
            div(
              
              style = "
                text-align:center;
                padding:30px;
              ",
              
              h4("Prédiction"),
              
              h2(
                textOutput(
                  "prediction_rf"
                ),
                
                style = "
                  font-size:30px;
                  font-weight:bold;
                  margin:20px;
                "
              ),
              
              hr(),
              
              h4(
                "Probabilité de survie"
              ),
              
              h2(
                textOutput(
                  "probabilite_rf"
                ),
                
                style = "
                  font-size:32px;
                  margin-top:20px;
                "
              )
            )
          )
        )
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "💡 Interprétation du profil"
        ),
        
        div(
          
          style = "
            padding:25px;
            font-size:17px;
            line-height:1.7;
          ",
          
          textOutput(
            "interpretation_profil"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ONGLET 2 - EFFET DE LA CLASSE
    # ========================================================
    
    tabPanel(
      
      "⚖️ Effet de la classe",
      
      br(),
      
      h3(
        "Que se passe-t-il si seule la classe change ?"
      ),
      
      p(
        "Le sexe, l'âge et la composition familiale restent identiques."
      ),
      
      plotOutput(
        "comparaison_classes",
        height = "500px"
      ),
      
      br(),
      
      card(
        
        card_header(
          "📌 Interprétation"
        ),
        
        div(
          
          style = "
            padding:25px;
            font-size:17px;
          ",
          
          textOutput(
            "interpretation_classe"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ONGLET 3 - COMPARAISON DES MODELES
    # ========================================================
    
    tabPanel(
      
      "🌲 Comparaison des modèles",
      
      br(),
      
      h3(
        "Les deux modèles donnent-ils la même réponse ?"
      ),
      
      plotOutput(
        "comparaison_modeles",
        height = "500px"
      ),
      
      br(),
      
      textOutput(
        "interpretation_modeles"
      )
    ),
    
    
    # ========================================================
    # ONGLET 4 - PERFORMANCE
    # ========================================================
    
    tabPanel(
      
      "📊 Performance",
      
      br(),
      
      h3(
        "Performance globale sur le jeu de test"
      ),
      
      p(
        "Ces résultats évaluent les modèles sur l'ensemble du jeu de test."
      ),
      
      plotOutput(
        "graph_accuracy",
        height = "500px"
      ),
      
      br(),
      
      tableOutput(
        "table_performance"
      )
    ),
    
    
    # ========================================================
    # ONGLET 5 - BIAIS ET ERREURS
    # ========================================================
    
    tabPanel(
      
      "⚠️ Biais et erreurs",
      
      br(),
      
      h3(
        "Erreurs selon la classe du passager"
      ),
      
      p(
        "Cette analyse permet d'identifier les groupes pour lesquels les modèles font le plus d'erreurs."
      ),
      
      plotOutput(
        "graph_erreurs",
        height = "650px"
      ),
      
      br(),
      
      card(
        
        card_header(
          "🔎 Interprétation"
        ),
        
        div(
          
          style = "
            padding:25px;
            font-size:17px;
            line-height:1.7;
          ",
          
          textOutput(
            "interpretation_biais"
          )
        )
      )
    )
  )
)


# ============================================================
# 12. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # PROFIL DU PASSAGER
  # ==========================================================
  
  passager <- reactive({
    
    data.frame(
      
      Pclass = factor(
        input$pclass,
        levels = levels(train$Pclass)
      ),
      
      Sex = factor(
        input$sex,
        levels = levels(train$Sex)
      ),
      
      Age = input$age,
      SibSp = input$sibsp,
      Parch = input$parch
    )
  })
  
  
  # ==========================================================
  # PROBABILITE - LOGISTIQUE
  # ==========================================================
  
  probabilite_logistique <- reactive({
    
    predict(
      modele,
      newdata = passager(),
      type = "response"
    )
  })
  
  
  # ==========================================================
  # PROBABILITE - RANDOM FOREST
  # ==========================================================
  
  probabilite_rf <- reactive({
    
    predict(
      modele_rf,
      newdata = passager(),
      type = "prob"
    )[, "Oui"]
  })
  
  
  # ==========================================================
  # PREDICTION LOGISTIQUE
  # ==========================================================
  
  output$prediction_logistique <- renderText({
    
    p <- probabilite_logistique()
    
    if (p >= 0.5) {
      
      "🟢 SURVIE : OUI"
      
    } else {
      
      "🔴 SURVIE : NON"
    }
  })
  
  
  # ==========================================================
  # PROBABILITE LOGISTIQUE
  # ==========================================================
  
  output$probabilite_logistique <- renderText({
    
    paste0(
      round(
        probabilite_logistique() * 100,
        1
      ),
      " %"
    )
  })
  
  
  # ==========================================================
  # PREDICTION RANDOM FOREST
  # ==========================================================
  
  output$prediction_rf <- renderText({
    
    p <- probabilite_rf()
    
    if (p >= 0.5) {
      
      "🟢 SURVIE : OUI"
      
    } else {
      
      "🔴 SURVIE : NON"
    }
  })
  
  
  # ==========================================================
  # PROBABILITE RANDOM FOREST
  # ==========================================================
  
  output$probabilite_rf <- renderText({
    
    paste0(
      round(
        probabilite_rf() * 100,
        1
      ),
      " %"
    )
  })
  
  
  # ==========================================================
  # INTERPRETATION DU PROFIL
  # ==========================================================
  
  output$interpretation_profil <- renderText({
    
    p_log <- probabilite_logistique()
    p_rf <- probabilite_rf()
    
    difference <- abs(
      p_log - p_rf
    ) * 100
    
    
    if (difference < 5) {
      
      paste0(
        "Les deux modèles donnent des estimations très proches ",
        "(écart de ",
        round(difference, 1),
        " points)."
      )
      
    } else {
      
      modele_plus_eleve <- ifelse(
        p_rf > p_log,
        "Random Forest",
        "régression logistique"
      )
      
      paste0(
        "Les deux modèles donnent des estimations différentes. ",
        modele_plus_eleve,
        " estime une probabilité de survie plus élevée, ",
        "avec un écart de ",
        round(difference, 1),
        " points."
      )
    }
  })
  
  
  # ==========================================================
  # PROBABILITES POUR LES 3 CLASSES
  # ==========================================================
  
  probas_par_classe <- reactive({
    
    classes <- c(1, 2, 3)
    
    
    sapply(
      
      classes,
      
      function(classe) {
        
        nouveau_passager <- data.frame(
          
          Pclass = factor(
            classe,
            levels = levels(train$Pclass)
          ),
          
          Sex = factor(
            input$sex,
            levels = levels(train$Sex)
          ),
          
          Age = input$age,
          SibSp = input$sibsp,
          Parch = input$parch
        )
        
        
        predict(
          modele_rf,
          newdata = nouveau_passager,
          type = "prob"
        )[, "Oui"]
      }
    )
  })
  
  
  # ==========================================================
  # GRAPHIQUE - EFFET DE LA CLASSE
  # ==========================================================
  
  output$comparaison_classes <- renderPlot({
    
    df <- data.frame(
      
      Classe = factor(
        c(1, 2, 3)
      ),
      
      Probabilite = probas_par_classe()
    )
    
    
    ggplot(
      df,
      aes(
        x = Classe,
        y = Probabilite,
        fill = Classe
      )
    ) +
      
      geom_col(
        width = 0.6
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(
              Probabilite * 100,
              1
            ),
            " %"
          )
        ),
        
        vjust = -0.4,
        size = 5
      ) +
      
      scale_y_continuous(
        
        limits = c(0, 1),
        
        labels = function(x) {
          paste0(
            x * 100,
            " %"
          )
        }
      ) +
      
      labs(
        title = "Même profil, classe différente",
        x = "Classe",
        y = "Probabilité de survie"
      ) +
      
      theme_minimal(
        base_size = 16
      ) +
      
      theme(
        legend.position = "none"
      )
  })
  
  
  # ==========================================================
  # INTERPRETATION - EFFET DE LA CLASSE
  # ==========================================================
  
  output$interpretation_classe <- renderText({
    
    probas <- probas_par_classe()
    
    ecart <- (
      max(probas) -
        min(probas)
    ) * 100
    
    
    classe_max <- which.max(
      probas
    )
    
    classe_min <- which.min(
      probas
    )
    
    
    paste0(
      
      "Pour ce profil, la probabilité de survie varie de ",
      
      round(
        min(probas) * 100,
        1
      ),
      
      " % en classe ",
      
      classe_min,
      
      " à ",
      
      round(
        max(probas) * 100,
        1
      ),
      
      " % en classe ",
      
      classe_max,
      
      ". L'écart est de ",
      
      round(
        ecart,
        1
      ),
      
      " points de pourcentage."
    )
  })
  
  
  # ==========================================================
  # COMPARAISON DES MODELES
  # ==========================================================
  
  output$comparaison_modeles <- renderPlot({
    
    df <- data.frame(
      
      Modele = c(
        "Régression logistique",
        "Random Forest"
      ),
      
      Probabilite = c(
        probabilite_logistique(),
        probabilite_rf()
      )
    )
    
    
    ggplot(
      df,
      aes(
        x = Modele,
        y = Probabilite,
        fill = Modele
      )
    ) +
      
      geom_col(
        width = 0.55
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(
              Probabilite * 100,
              1
            ),
            " %"
          )
        ),
        
        vjust = -0.5,
        size = 6
      ) +
      
      scale_y_continuous(
        
        limits = c(0, 1),
        
        labels = function(x) {
          paste0(
            x * 100,
            " %"
          )
        }
      ) +
      
      labs(
        x = "Modèle",
        y = "Probabilité de survie"
      ) +
      
      theme_minimal(
        base_size = 16
      ) +
      
      theme(
        legend.position = "none"
      )
  })
  
  
  # ==========================================================
  # INTERPRETATION - COMPARAISON DES MODELES
  # ==========================================================
  
  output$interpretation_modeles <- renderText({
    
    p1 <- probabilite_logistique()
    p2 <- probabilite_rf()
    
    difference <- abs(
      p1 - p2
    ) * 100
    
    
    if (difference < 5) {
      
      paste0(
        "Les deux modèles sont très proches pour ce passager : ",
        "écart de ",
        round(
          difference,
          1
        ),
        " points."
      )
      
    } else {
      
      paste0(
        "Les modèles présentent un écart de ",
        round(
          difference,
          1
        ),
        " points de probabilité de survie."
      )
    }
  })
  
  
  # ==========================================================
  # PERFORMANCE GLOBALE
  # ==========================================================
  
  output$graph_accuracy <- renderPlot({
    
    comparaison <- data.frame(
      
      Modele = c(
        "Régression logistique",
        "Random Forest"
      ),
      
      Accuracy = c(
        accuracy_logistique,
        accuracy_rf
      )
    )
    
    
    ggplot(
      comparaison,
      aes(
        x = Modele,
        y = Accuracy,
        fill = Modele
      )
    ) +
      
      geom_col(
        width = 0.55
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(
              Accuracy * 100,
              2
            ),
            " %"
          )
        ),
        
        vjust = -0.5,
        size = 6
      ) +
      
      scale_y_continuous(
        
        limits = c(0, 1),
        
        labels = function(x) {
          paste0(
            x * 100,
            " %"
          )
        }
      ) +
      
      labs(
        x = "Modèle",
        y = "Accuracy"
      ) +
      
      theme_minimal(
        base_size = 16
      ) +
      
      theme(
        legend.position = "none"
      )
  })
  
  
  # ==========================================================
  # TABLEAU PERFORMANCE
  # ==========================================================
  
  output$table_performance <- renderTable({
    
    data.frame(
      
      Modèle = c(
        "Régression logistique",
        "Random Forest"
      ),
      
      Accuracy = paste0(
        
        round(
          c(
            accuracy_logistique,
            accuracy_rf
          ) * 100,
          2
        ),
        
        " %"
      )
    )
  })
  
  
  # ==========================================================
  # GRAPHIQUE DES ERREURS
  # ==========================================================
  
  output$graph_erreurs <- renderPlot({
    
    
    erreurs_logistique <-
      resultats_erreurs_logistique %>%
      select(
        Pclass,
        taux_faux_positifs,
        taux_faux_negatifs
      ) %>%
      mutate(
        Modele = "Régression logistique"
      )
    
    
    erreurs_rf <-
      resultats_erreurs_rf %>%
      select(
        Pclass,
        taux_faux_positifs,
        taux_faux_negatifs
      ) %>%
      mutate(
        Modele = "Random Forest"
      )
    
    
    erreurs <- bind_rows(
      erreurs_logistique,
      erreurs_rf
    )
    
    
    erreurs_long <-
      erreurs %>%
      pivot_longer(
        
        cols = c(
          taux_faux_positifs,
          taux_faux_negatifs
        ),
        
        names_to = "Type_erreur",
        values_to = "Taux"
      ) %>%
      
      mutate(
        
        Type_erreur = recode(
          
          Type_erreur,
          
          taux_faux_positifs =
            "Faux positifs",
          
          taux_faux_negatifs =
            "Faux négatifs"
        ),
        
        Pclass = paste(
          "Classe",
          Pclass
        )
      )
    
    
    ggplot(
      erreurs_long,
      aes(
        x = Pclass,
        y = Taux,
        fill = Type_erreur
      )
    ) +
      
      geom_col(
        
        position = position_dodge(
          width = 0.8
        ),
        
        width = 0.7
      ) +
      
      geom_text(
        
        aes(
          label = paste0(
            round(
              Taux * 100,
              1
            ),
            " %"
          )
        ),
        
        position = position_dodge(
          width = 0.8
        ),
        
        vjust = -0.4,
        size = 4
      ) +
      
      facet_wrap(
        ~Modele
      ) +
      
      scale_y_continuous(
        
        limits = c(0, 0.6),
        
        labels = function(x) {
          paste0(
            x * 100,
            " %"
          )
        }
      ) +
      
      labs(
        x = "Classe",
        y = "Taux d'erreur",
        fill = "Type d'erreur"
      ) +
      
      theme_minimal(
        base_size = 16
      ) +
      
      theme(
        legend.position = "bottom"
      )
  })
  
  
  # ==========================================================
  # INTERPRETATION DES BIAIS
  # ==========================================================
  
  output$interpretation_biais <- renderText({
    
    paste0(
      "La classe 3 présente le taux de faux négatifs le plus élevé ",
      "dans les résultats obtenus. Cela montre que l'accuracy globale ",
      "ne suffit pas à évaluer équitablement un modèle : ses performances ",
      "peuvent varier selon le groupe de passagers."
    )
  })
}


# ============================================================
# 13. LANCEMENT DE L'APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)