# EconometricStudy-Fecondite

# Analyse des Déterminants de la Natalité en France (1991-2019)

## Description
Ce projet analyse les facteurs influençant le taux de fécondité en France sur la période 1991-2019. Il utilise des techniques économétriques avancées pour identifier et quantifier l'impact de différentes variables socio-économiques sur la natalité.

## Structure du Projet
```
.
├── Data/
│   ├── Data R Ajustée.xlsx        # Données brutes
│   ├── Data_Work.xlsx             # Données après première transformation
│   ├── Data_Work2.xlsx            # Données sans outliers
│   ├── Data_Work3.xlsx            # Données après traitement des colinéarités
│   └── Data_Work4.xlsx            # Données finales pour l'analyse
├── Code Final Fec/                 # Dossier contenant les outputs
└── paste.txt                       # Code R principal
```

## Méthodologie
L'analyse se déroule en plusieurs étapes clés :

1. **Prétraitement des données**
   - Interpolation linéaire des données annuelles en données trimestrielles
   - Création de variables décalées (lag)
   - Stationnarisation des séries temporelles
   - Suppression des outliers

2. **Traitement des colinéarités**
   - Analyse des corrélations entre variables
   - Tests VIF (Variance Inflation Factor)
   - Réduction des variables redondantes

3. **Modélisation économétrique**
   - Régression OLS avec transformation logarithmique
   - Tests des hypothèses de Gauss-Markov
   - Correction de l'autocorrélation avec ARIMA(4,0,0)
   - Tests de robustesse

## Variables Clés
- Variable dépendante : Taux de fécondité (variation relative)
- Variables explicatives principales :
  - Taux d'emploi des femmes
  - Emploi précaire des femmes
  - Prix des loyers
  - Performance boursière
  - Niveau d'éducation
  - Et autres indicateurs socio-économiques

## Tests Statistiques Effectués
- Test de Shapiro-Wilk (normalité)
- Tests de Breusch-Pagan et White (hétéroscédasticité)
- Test de Durbin-Watson et Breusch-Godfrey (autocorrélation)
- Test KPSS (stationnarité)
- Test de saisonnalité
- Test de Sargan et Hausman (endogénéité)

## Résultats Principaux
- Le modèle explique environ 72% de la variance du taux de fécondité (R² ≈ 0.72)
- Identification d'impacts significatifs positifs :
  - Taux d'emploi des femmes
  - Performance boursière
  - Proportion de femmes cadres
- Impacts négatifs significatifs :
  - Prix des loyers
  - Perception du niveau de vie passé

## Prérequis
Packages R nécessaires :
```R
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(tseries)
library(lmtest)
library(car)
library(AER)
library(sandwich)
```

## Installation
1. Cloner le repository
2. Installer les packages R requis
3. Configurer les chemins d'accès aux données dans le script principal

## Utilisation
1. Exécuter le script principal dans R/RStudio
2. Les résultats seront générés dans le dossier "Code Final Fec"

## Limitations
- Spécificité au contexte français
- Période d'étude limitée (1991-2019)
- Exclusion de l'année 2020 (COVID-19)
- Possibles variables omises

## Auteur
Romain Fehri

## Date
20 Décembre 2024

## Notes
- Les données doivent être dans le format exact spécifié
- Certains chemins de fichiers peuvent nécessiter des ajustements selon votre configuration
- La documentation détaillée des résultats est disponible dans le code source

## Licence
Modified MIT License (with Commercial Restriction)
Please refer to the document named License

Copyright (c) 2025 Mr. FEHRI
