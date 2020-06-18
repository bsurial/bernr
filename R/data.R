#' ICD 10 codes.
#'
#' This dataset is derived from the the ICD10gm package and currently compiles a useful dataset from the 2018 version.
#'
#'
#' @format An tibble with 16,058 rows and 7 variables
#' \describe{
#'   \item{icd_normcode}{ICD-10 code, detailed}
#'   \item{label}{Detailed ICD-10 label, corresponds to icd_normcode}
#'   \item{icd_code_short}{ICD-10 code, abbreviated to first three digits, useful if no detailed information is needed}
#'   \item{label_short}{Non-detailed ICD-10 label, corresponds to code}
#'   \item{icd_block_first}{first ICD-10 codes from current block of codes}
#'   \item{block_labe}{Label of current block}
#'   \item{chapter_label}{Name of ICD-10 chapter in which codes are organized}
#' }
#'
#' @keywords datasets
#'
#' @references Donnachie (2020), Metadata Processing for the German Modification of the ICD-10 Coding System
#' (\href{https://edonnachie.github.io/ICD10gm}{github})
#'
#' @source \href{https://doi.org/10.5281/zenodo.2542833}{github}
"icd_codes"

