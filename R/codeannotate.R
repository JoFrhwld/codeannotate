#' Code Annotate
#' @export
codeannotate <- function(){

  selection <- get_selection()

  beginning <- selection$range$start
  ending <- selection$range$end

  longest <- selection$text |>
    stringr::str_split("\\n", simplify = T) |>
    stringr::str_remove("#\\s*<\\d+>$") |>
    stringr::str_trim("right") ->
    code_lines

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

