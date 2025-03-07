#' Générer un rapport d'analyse
#'
#' Cette fonction compile un rapport Quarto avec des paramètres spécifiques.
#'
#' @param commune Nom de la commune.
#' @param departement Code du département.
#' @param output Chemin de sortie du rapport.
#'
#' @return Génère un rapport HTML.
#' @import quarto
#' @import dplyr
#' @import readr
#' @export

generer_rapport <- function(commune, departement, output) {
  # Vérifier que les paramètres sont bien fournis
  if (missing(commune) | missing(departement) | missing(output)) {
    stop("Erreur : Tous les paramètres (commune, departement, output) doivent être fournis.")
  }

  print(paste("Valeur de commune en entrée :", commune))
  print(paste("Valeur de departement en entrée :", departement))

  # Vérifier l'existence du fichier rapport.qmd
  qmd_path <- system.file("rapport.qmd", package = "projetFinal")
  if (qmd_path == "") stop("Fichier rapport.qmd introuvable.")

  # Vérifier l'existence du fichier des données
  data_path <- "data-raw/elus-conseillers-municipaux-cm.csv"
  if (!file.exists(data_path)) stop("Le fichier des données est introuvable : ", data_path)

  # Charger les données avec le bon séparateur
  data <- readr::read_delim(data_path, delim = ";")

  # Vérifier les noms de colonnes disponibles
  print("Noms des colonnes dans le fichier :")
  print(colnames(data))

  # Renommer les colonnes pour correspondre aux vrais noms du fichier CSV
  data <- data |>
    rename(
      departement = `Code du département`,
      nom_departement = `Libellé du département`,
      commune = `Libellé de la commune`
    )

  # Vérifier si les colonnes renommées existent bien
  if (!"commune" %in% colnames(data) | !"departement" %in% colnames(data)) {
    stop("Erreur : Les colonnes 'commune' et 'departement' ne sont pas correctement renommées.")
  }

  # Nettoyer les valeurs pour éviter les erreurs de casse et d'espaces
  data <- data |>
    mutate(
      commune = tolower(trimws(as.character(commune))),
      departement = as.character(trimws(departement))
    )

  commune <- tolower(trimws(as.character(commune)))
  departement <- as.character(trimws(departement))

  # Vérifier les valeurs uniques après nettoyage
  print("Valeurs uniques dans la colonne 'commune' après nettoyage :")
  print(unique(data$commune))

  print("Valeurs uniques dans la colonne 'departement' après nettoyage :")
  print(unique(data$departement))

  # Vérifier si la commune et le département existent bien dans les données
  if (!(commune %in% unique(data$commune))) {
    stop(paste("Erreur : La commune '", commune, "' n'est pas trouvée dans les données."))
  }
  if (!(departement %in% unique(data$departement))) {
    stop(paste("Erreur : Le département '", departement, "' n'est pas trouvé dans les données."))
  }

  # Filtrer les données pour la commune et le département donnés
  data_filtre <- dplyr::filter(data, commune == !!commune, departement == !!departement)

  # Vérifier si des données sont trouvées après le filtrage
  if (nrow(data_filtre) == 0) {
    stop(paste("Aucune donnée trouvée pour", commune, "dans le département", departement))
  }

  print("Données filtrées :")
  print(data_filtre)

  # Générer le rapport avec Quarto
  quarto::quarto_render(
    input = qmd_path,
    output_file = output,
    execute_params = list(commune = commune, departement = departement)
  )

  print(paste("Rapport généré avec succès :", output))
}


