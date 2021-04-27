
#' Unnest a column of lists in a dataframe
#' @param df dataframe where a column to be unnested
#' @param column specific column to be unnested
#' @examples
#' # nodes <- unnestListCol(graph$nodes, "properties")
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#' @return an unnested dataframe for network visualization
#' @export 
#' @rdname unnestListCol

unnestListCol <- function(df, column = "properties") {
  tmp <- map(df[,column], .manipulate_list)
  tmp <- rbindlist(tmp, fill=TRUE)
  tmp <- cbind.data.frame(df[,which(colnames(df) != column)], tmp)
  tmp
}


