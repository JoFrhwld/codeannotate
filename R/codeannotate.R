#' Code Annotate
#'
#' @description
#' Add code annotation markup to a code chunk in a quarto document.
#'
#' @details
#' After installing this RStudio addin, when lines of code are selected,
#' the markup for [quarto code annotations](https://quarto.org/docs/authoring/code-annotation.html)
#' will be added.
#'
#' Setting up a custom keybinding is recommended.
#'
#' @seealso [remove_codeannotate()]
#'
#' @export
codeannotate <- function(){

  chunk <- codechunk$new()

  if(length(chunk$context$selection) < 1){
    return()
  }

  chunk |>
    get_existing_annotation() |>
    insert_new_annotation() |>
    renumber_annotation() |>
    sanitize_rows() |>
    new_line_annotation() ->
    chunk

  write_chunk(chunk)

}


#' Remove Code Annotation
#'
#' @description
#' Remove code annotation markup.
#'
#' @details
#' If a selected line of code has quarto code annotation markup, this
#' will remove it.
#'
#' Setting up a custom keybinding is recommended.
#'
#' @seealso [codeannotate()]
#'
#' @export
remove_codeannotate <- function(){


  chunk <- codechunk$new()

  if(length(chunk$context$selection) < 1){
    return()
  }

  chunk |>
    get_existing_annotation()|>
    remove_annotaton()|>
    renumber_annotation() |>
    sanitize_rows() |>
    new_line_annotation() ->
    chunk

  write_chunk(chunk)

}

write_chunk <- function(chunk){
  rstudioapi::modifyRange(
    chunk$chunk_range,
    stringr::str_flatten(
      chunk$chunk_tibble$lines,
      collapse = "\n"
    )
  )

  rstudioapi::setSelectionRanges(
    chunk$original_range
  )
}

get_existing_annotation <- function(chunk){
  chunk$chunk_tibble |>
    dplyr::mutate(
      annotation = stringr::str_extract(
        lines,
        "#<\\d+>\\s*?$"
      )
    ) ->
    chunk$chunk_tibble

  return(chunk)
}

insert_new_annotation <- function(chunk){
  beginning <- chunk$selection_range$start["row"]
  ending <- chunk$selection_range$end["row"]

  chunk$chunk_tibble |>
    dplyr::mutate(
      annotation = dplyr::case_when(
        rown >= beginning & rown <= ending ~ "NEW",
        .default = annotation
      )
    )->
    chunk$chunk_tibble
  return(chunk)
}

renumber_annotation <- function(chunk){

  chunk$chunk_tibble |>
    dplyr::mutate(
      group_id = dplyr::consecutive_id(annotation)
    ) |>
    dplyr::filter(
      !is.na(annotation)
    ) |>
    dplyr::mutate(
      annotation = dplyr::consecutive_id(group_id),
      annotation = stringr::str_glue("#<{annotation}>")
    )   |>
    dplyr::select(
      rown, annotation
    ) ->
    annotation_rows

  chunk$chunk_tibble |>
    dplyr::select(
      -annotation
    ) |>
    dplyr::left_join(
      annotation_rows,
      by = "rown"
    )->
    chunk$chunk_tibble

  return(chunk)
}

sanitize_rows <- function(chunk){

  chunk$chunk_tibble |>
    dplyr::mutate(
      lines = stringr::str_remove(
        lines,
        "#<\\d+>\\s*?$"
      ) |>
        stringr::str_trim(side = "right")
    ) ->
    chunk$chunk_tibble

  return(chunk)
}

new_line_annotation <- function(chunk){

  chunk$chunk_tibble |>
    # dplyr::filter(
    #   !is.na(annotation)
    # ) |>
    dplyr::pull(lines) |>
    nchar() |>
    max() ->
    longest

  if(!any(!is.na(chunk$chunk_tibble$annotation))){
    return(chunk)
  }

  chunk$chunk_tibble |>
    dplyr::filter(
      !is.na(annotation)
    ) |>
    dplyr::mutate(
      nspaces = (longest - nchar(lines))  + 2,
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      padding = stringr::str_flatten(
        rep(" ", nspaces)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      lines = stringr::str_glue(
        "{lines}{padding}{annotation}"
      )
    ) |>
    dplyr::bind_rows(
      chunk$chunk_tibble |>
        filter(
          is.na(annotation)
        )
    ) |>
    dplyr::arrange(rown) ->
    chunk$chunk_tibble

  return(chunk)
}

remove_annotaton <- function(chunk){
  beginning <- chunk$selection_range$start["row"]
  ending <- chunk$selection_range$end["row"]

  chunk$chunk_tibble |>
    dplyr::mutate(
      annotation = case_when(
        rown >= beginning & rown <= ending ~ NA_character_,
        .default = annotation
      )
    ) ->
    chunk$chunk_tibble

  return(chunk)

}

