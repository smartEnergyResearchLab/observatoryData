#' Generates the information table for the start of each documentation RMarkdown doc
#' 
#' \code{create.info.table} reads the release date and gives basic info about the document
#'
#' Returns a flextable object
#'
#' @param vers Version, typically the data release date
#' @param auth document author
#' @param proj project: SERL
#' @param org organisation
#' @param col_width column widths from left to right
#'
#' @import flextable
#' @import datatable
#' @family smartMeter
#'
#' @author Ellen Webborn, \email{e.webborn@@ucl.ac.uk}
#' @export

create.info.table <- function(vers = release,
                              auth = "Ellen Webborn",
                              proj = "Smart Energy Research Lab (SERL)",
                              org = "University College London (UCL)",
                              col_widths = c(1.2, 3)) {
  it <- data.table(
    attributes = c("Creation date",
                   "Edition",
                   "Author",
                   "Project",
                   "Organisation"),
    info = c(as.character(Sys.Date()),
             vers,
             auth,
             proj,
             org)
  )
  
  itf <- flextable::flextable(it)
  
  for (i in 1:ncol(it)) {
    itf <- flextable::width(itf, j = i, width = col_widths[i])
  }
  
  itf <- flextable::delete_part(x = itf,
                     part = "header")
  
  itf <- flextable::border_remove(itf)
}