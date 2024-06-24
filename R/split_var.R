#' Split a character variable into multiple variables
#'
#' @description
#' \code{split_var} separates a character variable with a length exceeding `length` into multiple variables
#' with the longest not exceeding `length` with corresponding delimiter, which is suitable for data manipulation.
#'
#' @param data A data frame or tibble.
#' @param target_var A character variable in input data set that need to be split, like \code{product_term}.
#' if the name of \code{target_var} is same as \code{outvar_name}, then the name of \code{target_var} is appended with a prefix, \code{(.)}.
#' @param outvar_name A string which is the prefix of similar variable names, like \code{"CMDECOD"}.
#' @param sep A character vector that is separator delimiting collapsed values.
#'  Default: `;`.
#' @param len A scalar integer(a length-one numeric) to assign the length that every variable shouldn't exceed.
#' @return A tibble including a series of variable calling \code{outvar_name}, which is invisible object.
#' @examples
#'
#' test_df <- data.frame(
#'   id = 1:2,
#'   text = c(
#'     paste(strrep("a", 150), ";", strrep("a", 50), strrep("a", 130), sep = ";"),
#'     paste(strrep("a", 150), strrep("a", 30), strrep("a", 150), strrep("a", 80), sep = ";")
#'   )
#' )
#'
#'
#' new_test_df <- test_df %>% dso_split_var(text, "text", "\\;")
#'
#' @export
split_var <- function(data, target_var, outvar_name, sep = ";", len = 200L) {

  .need_split_var <- rlang::enquo(target_var)

  stopifnot(is.data.frame(data),
            length(rlang::quo_get_expr(.need_split_var)) == 1L,
            rlang::is_string(outvar_name),
            rlang::is_string(sep),
            rlang::is_integer(len))

  # Remove a series of variable named outvar_name with numeric suffix
  .to_keep <- names(data)[!grepl(paste0(outvar_name, "([0-9]+)$"), names(data))]
  .new_data <- data %>%
    dplyr::mutate(dso_unique_id = if (nrow(.) == 0L) {
      numeric(0L)
    } else {
      dplyr::row_number()
    }) %>%
    dplyr::select(dplyr::all_of(.to_keep), dso_unique_id)


  # if (is.na(sep)) sep <- "\\;"
  # separate string
  .split_data <- .new_data %>%
    dplyr::mutate(dso_split_var = !!.need_split_var) %>%
    tidyr::separate_rows(dso_split_var, sep = sep) %>%
    dplyr::group_by(dso_unique_id, !!!rlang::syms(.to_keep)) %>%
    dplyr::mutate(
      dso_var1 = if (is.null(dso_cumsum(nchar(dso_split_var), x1 > {{len}}))) {
        numeric(0L)
      } else {
        dso_cumsum(nchar(dso_split_var), x1 > {{len}})
      },
      dso_var2 = ifelse(nchar(dso_split_var) == dso_var1, TRUE, FALSE),
      dso_subid = dplyr::coalesce(cumsum(dso_var2), 1L),
      dso_text = dplyr::coalesce(dso_split_var, "")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dso_unique_id, !!!rlang::syms(.to_keep), dso_subid) %>%
    dplyr::summarise(dso_text = paste(dso_text, collapse = sep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("dso_text", ~ dplyr::if_else(stringr::str_squish(.) == "", NA_character_, .)) %>%
    # Avoid intermediate separator causing redundant symbols at the end
    dplyr::mutate(dso_text = gsub(paste0("(", sep, "+)$"), "", dso_text)) %>%
    tidyr::pivot_wider(names_from = dso_subid, values_from = dso_text, names_prefix = "dso_text") %>%
    dplyr::select(-dso_unique_id)

  # if the name of target_var and .out_var is same, then rename target_var to paste(., target_var)

  if (identical(rlang::as_label(.need_split_var), outvar_name)) {
    names(.split_data)[names(.split_data) == outvar_name] <- paste0(".", outvar_name)
  }

  # modify the name of series variable calling outvar_name
  .text_name <- names(dplyr::select(.data = .split_data, dplyr::starts_with("dso_text")))

  if (length(.text_name) == 1L) {
    names(.split_data)[names(.split_data) %in% .text_name] <- outvar_name
  } else {
    names(.split_data)[names(.split_data) %in% .text_name] <- c(outvar_name, paste0(outvar_name, 1L:(length(.text_name) - 1)))
  }

  invisible(.split_data)
}


dso_cumsum <- function(num, cond) {
  .cond <- rlang::quo_squash(rlang::enquo(cond))

  Reduce(function(x1, x2) {
    x1 <- sum(x1, (x2 + 1L), na.rm = TRUE)
    if (eval(.cond)) {
      x1 <- 0L
      x1 <- sum(x1, x2, na.rm = TRUE)
    }
    return(x1)
  }, num, accumulate = TRUE)
}
