#' codechunk class
#' @export
codechunk <- R6::R6Class(
  classname = "codechunk",

  public = list(
    selected = FALSE,
    chunk = FALSE,
    context = "",
    selection = "",
    selection_lines = "",
    document_lines = "",
    chunk_lines = "",
    chunk_range = "",

    initialize = function(){
      self$context <- rstudioapi::getSourceEditorContext()

      if(length(self$context$selection) < 1 ){
        self$selected = F
        return()
      }

      self$selection <- primary_selection(self$context)

      self$selection_lines <- private$get_selection_lines()
      self$document_lines <- self$context$contents

      self$chunk_range <- private$get_chunk_range()
      self$chunk_lines <- private$get_chunk_lines()

    }
  ),

  private = list(
    get_selection_lines = function(){
      selection <- rstudioapi::primary_selection(
        self$context
      )

      beginning <- selection$range$start["row"]
      ending <- selection$range$end["row"]

      return(self$context$contents[beginning:ending])


    },

    get_chunk_range = function(){

      chunk_start = 1
      chunk_end = length(self$document_lines)

      doc_len <- length(self$document_lines)

      selection_start <- self$selection$range$start["row"]
      selection_end <- self$selection$range$end["row"]
      rev_lines = self$document_lines[selection_start:1]
      fwd_lines = self$document_lines[selection_end:doc_len]



      has_chunk_start <- stringr::str_detect(
          rev_lines,
          "^((```)|\\{\\S+\\})"
        )

      if(any(has_chunk_start)){
        chunk_start <- (selection_start:1)[
          which(has_chunk_start)
        ][1] + 1
      }

      has_chunk_end <- stringr::str_detect(
        fwd_lines,
        "```$"
      )

      if(any(has_chunk_end)){
        chunk_end = (selection_end:doc_len)[
          which(has_chunk_end)
        ][1] - 1
      }

      chunk_range = document_range(
        start = c(chunk_start, 0),
        end = c(chunk_end, Inf)
      )

      return(chunk_range)
    },

    get_chunk_lines = function(){
      beginning = self$chunk_range$start["row"]
      ending = self$chunk_range$end["row"]

      chunk_lines = self$context$contents[beginning:ending]
      return(chunk_lines)
    }
  )
)

#' make code chunk
#' @export
make_code_chunk <- function(){
  chunk <<- codechunk$new()
}
