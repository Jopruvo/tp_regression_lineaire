library(dplyr)
library(ggfortify)
library(ggplot2)
library(ggpubr)


## Partie I - Préparation du jeu de données

# Importation du .csv et remplacement des valeurs na par 0
data <- read.csv("covidFranceData2021.csv", header=TRUE, stringsAsFactors=FALSE)
data[is.na(data)] <- 0

# Création des deux sous-dataframes
sousDataTotal <- data[,c("date", "total_cases", "total_deaths", "total_tests", "positive_rate", "tests_per_case", "total_vaccinations", "people_vaccinated", "people_fully_vaccinated")]
sousDataWeekly <- data[,c("date", "new_cases", "new_deaths", "icu_patients", "hosp_patients", "weekly_icu_admissions", "weekly_hosp_admissions", "new_tests", "positive_rate", "tests_per_case", "new_vaccinations")]

# Mise en places des lignes avec la date
rownames(sousDataTotal) <- sousDataTotal$date
sousDataTotal <- within(sousDataTotal, rm("date"))
rownames(sousDataWeekly) <- sousDataTotal$date
sousDataWeekly <- within(sousDataWeekly, rm("date"))

# Fonction
list_data <- function(fileName){
    df <- read.csv(fileName, header=TRUE, stringsAsFactor=FALSE)
    df[is.na(df)] <- 0
    df1 <- df[,c("date", "total_cases", "total_deaths", "total_tests", "positive_rate", "tests_per_case", "total_vaccinations", "people_vaccinated", "people_fully_vaccinated")]
    df2 <- df[,c("date", "new_cases", "new_deaths", "icu_patients", "hosp_patients", "weekly_icu_admissions", "weekly_hosp_admissions", "new_tests", "positive_rate", "tests_per_case", "new_vaccinations")]
    rownames(df1) <- df1$date
    df1 <- within(df1, rm("date"))
    rownames(df2) <- df2$date
    df2 <- within(df2, rm("date"))
    listOfDf <- list(df1, df2)
    return (listOfDf)
}

## Partie II - Régression linéaire

#  L'analyse de régression linéaire sert à prévoir la valeur d'une variable en fonction de la valeur d'une autre variable.

totalData <- list_data("covidFranceData2021.csv")[1]
weeklyData <- list_data("covidFranceData2021.csv")[2]

g_weekhosp <- ggplot(weeklyData$weekly_hosp_admissions, aes(x = "weekly_hosp_admissions", y = "date")) + geom_point()
