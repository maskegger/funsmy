#' Nicer connection syntax to a table within a database schema.
#' @param con Connection string to the database.
#' @param schema Schema within the database. In DCVTS, each round is a different schema.
#' @param table Connection to the specific table within the database schema.
#' @return Connection to the corresponding table is returned.
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' tblmy(con, "r2", "responses")
#' tblmy(con, "r2", "summary")
#' }
tblmy <- function(con, schema, table) {
  tbl(con, in_schema(schema, table))
}

#' Write the object to a table within a database schema.
#' @param con Connection string to the database.
#' @param schema Schema within the database. In DCVTS, each round is a different schema.
#' @param table Connection to the specific table within the database schema.
#' @param object R object to be written to the database.
#' @details Any previous table is overwritten.
#' @importFrom tictoc tic toc
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' dbWriteTablemy(con, "r2", "responses")
#' dbWriteTablemy(con, "r2", "summary")
#' }
dbWriteTablemy <- function(con, object, schema, table) {
  tic()
  DBI::dbWriteTable(conn = con, Id(schema = schema, table = table), object, temporary = FALSE, overwrite = TRUE)
  toc()
}

#' Compute a process and output results to a table within a database schema.
#' @param con Connection string to the database.
#' @param object Pipeline object to be computed.
#' @param schema Schema within the database. In DCVTS, each round is a different schema.
#' @param table Connection to the specific table within the database schema.
#' @details `computemy()` also checks if a table previously exists and if it does, it removes the table and writes the new computation to the table. It also prints how long it to perform the computation within the database.
#' @importFrom tictoc tic toc
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' computemy(con, "r2", "responses")
#' computemy(con, "r2", "summary")
#' }
computemy <- function(con, object, schema, table) {
  tic()
  if (DBI::dbExistsTable(con, Id(schema = schema, table = table))) {
    DBI::dbRemoveTable(conn = con, Id(schema = schema, table = table))
    dplyr::compute(object, in_schema(schema, table), temporary = FALSE, overwrite = TRUE)
  }
  else {
    dplyr::compute(object, in_schema(schema, table), temporary = FALSE, overwrite = TRUE)
  }
  toc()
}
