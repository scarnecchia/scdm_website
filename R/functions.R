process_values <- function(val) {
  if (all(!is.na(c(val$value, val$value_map)))) {
    paste(val$value, "=", val$value_map)
  } else if (any(!is.na(c(val$value, val$value_map)))) {
    ifelse(!is.na(val$value_map), paste(val$value, "=", val$value_map), val$value)
  } else {
    val$value
  }
}

# Helper function to process notes
process_guidance <- function(n) {
  if (all(!is.na(c(n$label, n$text)))) {
    paste0("<b>", n$label, "</b>", ": ", n$text)
  } else {
    n$text
  }
}

process_notes <- function(n) {
  .n <- n[[1]]


  if (length(.n) == 1) {
    paste0("[^", .n, "]")
  } else {
    .n <- .n %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::select(value)

    paste0("[^", .n$value, "]")
  }

}

clean_up = function(n) {
  n %>%
    dplyr::mutate(
      values = purrr::map(values, ~ stringr::str_c(process_values(.x), collapse = "<br>")),
      guidance = purrr::map(guidance, ~ ifelse(!is.na(.x), stringr::str_c(process_guidance(.x), collapse = "<br>"), NA_character_)),
      notes = purrr::map(notes, ~ ifelse(!is.na(.x), stringr::str_c(process_notes(.x), collapse = ""), NA_character_)),
      notes = purrr::map(notes, ~ if(length(.) == 0) NA else .),
      notes = purrr::map(notes, ~ if(length(.) > 1) stringr::str_flatten(.) else .),
      variable_name = dplyr::if_else(!is.na(notes), glue::glue("{variable_name}{notes}"), variable_name)
    )
}

# Main function to transform JSON to HTML
json_to_html <- function(x) {
  x %>%
    dplyr::mutate(
      variables = purrr::map(variables, ~ clean_up(.x)))
}
