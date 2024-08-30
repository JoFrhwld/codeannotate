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

  selection <- get_selection()

  beginning <- selection$range$start
  ending <- selection$range$end

  code_lines <- get_code_lines(selection)

  code_lengths <- nchar(code_lines)
  longest <- max(code_lengths)

  nspaces <- (longest - code_lengths)+3

  spaces <- lapply(
    nspaces,
    \(x) rep(" ", x) |> stringr::str_flatten()
  ) |>
    unlist()

  new_code_lines <- code_lines

  for(i in seq_along(code_lines)){
    new_code_lines[i] <- stringr::str_glue(
      "{code_lines[i]}{spaces[i]}# <{i}>"
    )
  }

  ranges <- lapply(
    seq(
      beginning["row"],
      ending["row"]
    ),
    \(x) rstudioapi::document_range(
      start = c(x,1),
      end = c(x, Inf)
    )
  )

  rstudioapi::modifyRange(
    location = ranges,
    text = new_code_lines
  )


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
  selection <- get_selection()
  beginning <- selection$range$start
  ending <- selection$range$end

  code_lines <- get_code_lines(selection)

  ranges <- lapply(
    seq(
      beginning["row"],
      ending["row"]
    ),
    \(x) rstudioapi::document_range(
      start = c(x,1),
      end = c(x, Inf)
    )
  )

  rstudioapi::modifyRange(
    location = ranges,
    text = code_lines
  )
}


get_selection <- function(){
  context <- rstudioapi::getSourceEditorContext()

  if(length(context$selection) < 1){
    return()
  }

  selection <- context |>
    rstudioapi::primary_selection()

  expand_selection(selection)

  context <- rstudioapi::getSourceEditorContext()
  selection <- context |>
    rstudioapi::primary_selection()

  return(selection)
}

get_code_lines <- function(selection){

  selection$text |>
    stringr::str_split("\\n", simplify = T) |>
    stringr::str_remove("#\\s*<\\d+>$") |>
    stringr::str_trim("right") ->
    code_lines

  return(code_lines)

}

expand_selection <- function(selection){

  rstudioapi::setSelectionRanges(
    rstudioapi::document_range(
      start = c(
        selection$range$start["row"],
        0
      ),
      end = c(
        selection$range$end["row"],
        Inf
      )
    )
  )

}

