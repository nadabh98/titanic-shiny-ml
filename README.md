# Titanic — Machine Learning & Shiny App

Projet de Machine Learning réalisé en **R** autour du célèbre dataset Titanic.

L'objectif est de construire et comparer deux modèles de classification afin de prédire la survie des passagers, puis de mettre les résultats à disposition dans une application **Shiny interactive**.

## 🚀 Application en ligne

👉 [Accéder à l'application Shiny](https://titanicshinytml.shinyapps.io/Titanic/)

L'application permet notamment de :

- prédire la survie d'un passager ;
- comparer une régression logistique et un Random Forest ;
- visualiser les performances des modèles ;
- analyser les erreurs selon la classe du passager ;
- mettre en évidence certaines limites et biais du modèle.

## 🎯 Objectif

Le projet ne se limite pas à obtenir une bonne accuracy.

Il cherche également à comprendre **comment le modèle se comporte selon les profils de passagers**, notamment selon leur classe, et à analyser les erreurs de prédiction.

## 🤖 Modèles

Deux modèles de classification sont comparés :

### Régression logistique

Modèle de référence permettant d'étudier l'influence des variables explicatives sur la probabilité de survie.

### Random Forest

Modèle d'ensemble basé sur plusieurs arbres de décision, utilisé ici pour comparer ses performances avec la régression logistique.

Variables utilisées :

- `Pclass` — classe du passager
- `Sex` — sexe
- `Age` — âge
- `SibSp` — nombre de frères/sœurs ou conjoint à bord
- `Parch` — nombre de parents/enfants à bord

## 📊 Résultats

| Modèle | Accuracy |
|---|---:|
| Régression logistique | **77.65 %** |
| Random Forest | **82.12 %** |

Le Random Forest améliore l'accuracy d'environ **4,5 points de pourcentage** par rapport à la régression logistique.

L'analyse des erreurs montre toutefois que les performances ne sont pas identiques selon les classes de passagers. La classe 3 présente notamment un taux élevé de faux négatifs, ce qui montre pourquoi une seule métrique globale ne suffit pas pour évaluer un modèle.

## 🧠 Analyse

Le projet met en évidence plusieurs aspects importants d'un workflow Machine Learning :

**Données → Prétraitement → Entraînement → Évaluation → Analyse des erreurs → Déploiement**

L'analyse des erreurs permet d'aller au-delà de l'accuracy et d'identifier les profils pour lesquels le modèle rencontre davantage de difficultés.

## 🖥️ Application Shiny

L'application est organisée autour de plusieurs vues :

- 🎯 **Prédiction** — prédiction individuelle de survie
- ⚖️ **Effet de la classe** — analyse du comportement selon `Pclass`
- 🌲 **Comparaison des modèles** — comparaison Logistic Regression / Random Forest
- 📊 **Performance** — métriques des modèles
- ⚠️ **Biais et erreurs** — analyse des erreurs de classification

## 🛠️ Technologies

- **R**
- **Shiny**
- **Random Forest**
- **Régression logistique**
- **ggplot2**
- **dplyr**
- **tidyr**
- **bslib**
- **Titanic dataset**
- **shinyapps.io**

## 📁 Structure du projet

```text
titanic-shiny-ml/
│
├── app.R
├── projet-R-titanic.Rmd
└── README.md

👩‍💻 Auteur

Nada Ben Hammouda

Biomedical Engineer | AI & Data
