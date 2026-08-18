#' Benutzer an Referenzbenutzer angleichen
#'
#' Ermittelt für jeden Benutzer die fehlenden
#' Rollen gegenüber einem Referenzbenutzer
#' und erzeugt die benötigten D365-Importentitäten.
#'
#' @details
#'
#' Die Funktion arbeitet auf der Berechtigungsmatrix,
#' die von \code{sec_create_permission_matrix()} erzeugt wurde.
#'
#' Für jeden Benutzer (außer dem Referenzbenutzer) wird geprüft,
#' welche Rollen des Referenzbenutzers fehlen.
#'
#' Benutzer, bei denen keine Rollen fehlen,
#' werden übersprungen.
#'
#' Aus den fehlenden Rollen werden zwei
#' D365-Importentitäten erzeugt:
#'
#' \describe{
#'
#' \item{roles_to_add}{
#' Entity: System security user role organization.
#' Enthält alle fehlenden Rollenzuweisungen mit
#' \code{ASSIGNMENTMODE = "Manual"} und
#' \code{ASSIGNMENTSTATUS = "Enabled"}.
#' }
#'
#' \item{assignments_to_add}{
#' Entity: System security user role organization assignment.
#' Enthält nur organisationsspezifische Zuweisungen
#' (\code{ORGANIZATIONID != "All"}) mit
#' \code{ORGANIZATIONTYPE = "LegalEntity"}.
#' }
#'
#' }
#'
#' @param permission_matrix
#' Berechtigungsmatrix. Ergebnis von
#' \code{sec_create_permission_matrix()}.
#'
#' Erwartete Spalten:
#'
#' \itemize{
#'   \item \code{SECURITYROLENAME}
#'   \item \code{SECURITYROLEIDENTIFIER}
#'   \item \code{ORGANIZATIONID}
#'   \item eine Spalte pro Benutzer (Wert \code{"x"} = Rolle vorhanden)
#' }
#'
#' @param reference_user
#' Benutzerkennung des Referenzbenutzers.
#' Muss als Spalte in \code{permission_matrix} vorhanden sein.
#'
#' @return
#'
#' Liste mit drei Elementen:
#'
#' \describe{
#'
#' \item{roles_to_add}{
#' Tibble mit fehlenden Rollenzuweisungen.
#' Spalten: \code{USERID}, \code{SECURITYROLEIDENTIFIER},
#' \code{ASSIGNMENTMODE}, \code{ASSIGNMENTSTATUS},
#' \code{SECURITYROLENAME}.
#' Leer wenn keine Unterschiede.
#' }
#'
#' \item{assignments_to_add}{
#' Tibble mit fehlenden Organisationszuweisungen.
#' Spalten: \code{USERID}, \code{SECURITYROLEIDENTIFIER},
#' \code{ORGANIZATIONTYPE}, \code{ORGANIZATIONID}.
#' Leer wenn keine Unterschiede.
#' }
#'
#' \item{comparison}{
#' Tibble zur Vergleichsdokumentation.
#' Zeigt alle fehlenden Rollen pro Benutzer.
#' Spalten: \code{USERID}, \code{SECURITYROLEIDENTIFIER},
#' \code{SECURITYROLENAME}, \code{ORGANIZATIONID}.
#' }
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' permission_matrix <- sec_create_permission_matrix(e)
#'
#' result <- sec_generate_role_alignment(
#'   permission_matrix = permission_matrix,
#'   reference_user    = "wibule02"
#' )
#'
#' result$roles_to_add
#'
#' result$assignments_to_add
#'
#' result$comparison
#'
#' }
#'
#' @seealso
#' \code{\link{sec_create_permission_matrix}}
#' \code{\link{sec_compare_to_reference_user_details}}
#'
#' @export
sec_generate_role_alignment <- function(
    permission_matrix,
    reference_user
) {

  #====================================================
  # VALIDIERUNG
  #====================================================

  if (!reference_user %in% names(permission_matrix)) {
    stop(paste(
      "Referenzbenutzer nicht gefunden:",
      reference_user
    ))
  }

  #====================================================
  # BENUTZERSPALTEN ERMITTELN
  #====================================================

  user_columns <- setdiff(
    names(permission_matrix),
    c(
      "SECURITYROLENAME",
      "SECURITYROLEIDENTIFIER",
      "ORGANIZATIONID"
    )
  )

  target_users <- setdiff(user_columns, reference_user)

  #====================================================
  # REFERENZROLLEN
  #====================================================

  reference_permissions <-
    permission_matrix |>
    dplyr::filter(
      .data[[reference_user]] == "x"
    ) |>
    dplyr::select(
      SECURITYROLEIDENTIFIER,
      SECURITYROLENAME,
      ORGANIZATIONID
    )

  #====================================================
  # ERGEBNISLISTEN
  #====================================================

  comparison_list        <- list()
  roles_to_add_list      <- list()
  assignments_to_add_list <- list()

  #====================================================
  # BENUTZER VERGLEICHEN
  #====================================================

  for (current_user in target_users) {

    user_permissions <-
      permission_matrix |>
      dplyr::filter(
        .data[[current_user]] == "x"
      ) |>
      dplyr::select(
        SECURITYROLEIDENTIFIER,
        SECURITYROLENAME,
        ORGANIZATIONID
      )

    missing_permissions <-
      dplyr::anti_join(
        reference_permissions,
        user_permissions,
        by = c("SECURITYROLEIDENTIFIER", "ORGANIZATIONID")
      )

    if (nrow(missing_permissions) == 0) {
      next
    }

    #==================================================
    # Vergleichsdokumentation
    #==================================================

    comparison_list[[current_user]] <-
      missing_permissions |>
      dplyr::mutate(
        USERID = current_user
      ) |>
      dplyr::select(
        USERID,
        dplyr::everything()
      )

    #==================================================
    # Entity: System security user role organization
    #==================================================

    roles_to_add_list[[current_user]] <-
      missing_permissions |>
      dplyr::distinct(
        SECURITYROLEIDENTIFIER,
        SECURITYROLENAME
      ) |>
      dplyr::mutate(
        USERID           = current_user,
        ASSIGNMENTMODE   = "Manual",
        ASSIGNMENTSTATUS = "Enabled"
      ) |>
      dplyr::select(
        USERID,
        SECURITYROLEIDENTIFIER,
        ASSIGNMENTMODE,
        ASSIGNMENTSTATUS,
        SECURITYROLENAME
      )

    #==================================================
    # Entity: System security user role organization
    #         assignment
    #==================================================

    assignments_to_add_list[[current_user]] <-
      missing_permissions |>
      dplyr::filter(
        ORGANIZATIONID != "All"
      ) |>
      dplyr::mutate(
        USERID           = current_user,
        ORGANIZATIONTYPE = "LegalEntity"
      ) |>
      dplyr::select(
        USERID,
        SECURITYROLEIDENTIFIER,
        ORGANIZATIONTYPE,
        ORGANIZATIONID
      )
  }

  #====================================================
  # RÜCKGABE
  #====================================================

  comparison       <- dplyr::bind_rows(comparison_list)
  roles_to_add     <- dplyr::bind_rows(roles_to_add_list)
  assignments_to_add <- dplyr::bind_rows(assignments_to_add_list)

  list(
    roles_to_add       = roles_to_add,
    assignments_to_add = assignments_to_add,
    comparison         = comparison
  )
}
