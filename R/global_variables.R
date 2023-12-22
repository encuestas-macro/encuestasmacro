
#' Read details form yaml file
#'
#' @param group string wiht the component needed: "een", "eoe"
#'
#' @return a list with the existing values
read_details_file <- function(group) {
  yaml::read_yaml(system.file("details.yaml", package = "encuestasmacro"))[[group]]
}

eem_details <- read_details_file("eem")


utils::globalVariables(
  c(
    ".data",
    "ano",
    "crecimiento_interanual",
    "dia",
    "fecha",
    "grupo",
    "horizonte",
    "inflacion_diciembre_obs",
    "ipc_vi",
    "ipc_vm",
    "mes",
    "name",
    "partida",
    "periodo",
    "tc_diciembre_obs",
    "tc_mes_obs",
    "tpm",
    "tpm_diciembre_obs",
    "tpm_mes_obs",
    "tpm_trimestre_obs",
    "trimestre",
    "value",
    "variable",
    "variable_label",
    "venta",
    "year"
  )
)
