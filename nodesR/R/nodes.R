#' # Nodes
#' #
#' #
#'
#' #'@export
#' dopOption <- function() {
#'
#' }
#'
#' #' dopMeta
#' #'@return dopMetaObject
#' #'@export
#' dopMeta <- function(...) {
#'
#' }
#'
#' #' Create a new Datum comutation node in your project
#' #'
#' #'@param project     project containing that node (default to main project)
#' #'@param meta        Use \code{\link{dopMeta}} to set metadata to the output of this node
#' #'@param chunk       function that identify chunks of data and returns a table of chunk identifiers
#' #'@param import      function to convert chunks identifier into data
#' #'@return dopNodeObject
#' #'@examples
#' #'  dopNewImportNode(import={
#' #'      data.frame(
#' #'          time=Sys.time(),
#' #'          pid=Sys.getpid()
#' #'          )
#' #'  })
#' #'
#' #'  dopNewImportNode(
#' #'      meta=dopMeta(project.name="Monitor PID over time"),
#' #'      import={
#' #'          data.frame(
#' #'              time=Sys.time(),
#' #'              pid=Sys.getpid()
#' #'          )
#' #'       }
#' #'  )
#' #'@export
#' dopNewImportNode <- function(projectPath=".", meta, chunk, import, options) {
#'
#' }
#'
#' #' Create a new Datum comutation node in your project
#' #'
#' #'@param project     project containing that node (default to main project)
#' #'@param meta        Use \code{\link{dopMeta}} to set metadata to the output of this node
#' #'@param chunk       function that identify chunks of data and returns a table of chunk identifiers
#' #'@param import      function to convert chunks identifier into data
#' #'@return dopNodeObject
#' #'@export
#' dopNewProcessNode <- function(project, meta, sourceNode, process, options) {
#'
#' }
