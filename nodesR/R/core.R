#' #' List that can be copied by reference
#' #'
#' #'@param id Name to show on \code{print()} for that list
#' #'@export
#' dopRefList <- function(id) {
#'
#'   if (is.null(.GlobalEnv$.dopRefListLastId)) .GlobalEnv$.dopRefListLastId <- 0
#'   .GlobalEnv$.dopRefListLastId <- .GlobalEnv$.dopRefListLastId +1
#'
#'   if (missing(id)) {
#'     id <- .GlobalEnv$.dopRefListLastId
#'   }
#'
#'   lDopRefEnv <- new.env(emptyenv())
#'
#'   # This is usefull for ==
#'   lDopRefList <- list(uid=.GlobalEnv$.dopRefListLastId, id=id)
#'   lDopRefEnv$selfRef <- lDopRefList
#'
#'   class(lDopRefList) <- "DopRefList"
#'   attr(lDopRefEnv, "uid") <- .GlobalEnv$.dopRefListLastId
#'   attr(lDopRefList, "ref") <- lDopRefEnv
#'
#'   attr(lDopRefEnv, "id") <- id
#'
#'   return(lDopRefList)
#' }
#'
#' #'@export
#' `$.DopRefList` <- function(x, name) {
#'   lRef <- attr(x,"ref")
#'   lRef[[name]]
#' }
#'
#' #'@export
#' `==.DopRefList` <- function(a,b) {
#'   lRefa <- attr(a,"ref")
#'   lRefb <- attr(b,"ref")
#'   identical(lRefa,lRefb)
#' }
#'
#' #'@export
#' `[[.DopRefList` <- function(x, index) {
#'   if (is.numeric(index) ) {
#'     l_n <- names(x)[[index]]
#'   } else {
#'     l_n <- index
#'   }
#'
#'   lRef <- attr(x,"ref")
#'   l_ret <- lRef[[l_n]]
#'
#'   # protect against infinit loop
#'   if (inherits(l_ret, "DopRefList")) {
#'     return(paste0("RefListObject: use RefListObject$",l_n ," not RefListObject[[",l_n,"]] to access it."))
#'   } else {
#'     return(l_ret)
#'   }
#' }
#'
#'
#' #'@export
#' `[.DopRefList` <- function(x, index) {
#'   l_n <- names(x)
#'   lRef <- attr(x,"ref")
#'   lapply(l_n[index], function(x) lRef[[x]])
#' }
#'
#' #'@export
#' `$<-.DopRefList` <- function(x, name, value) {
#'   lRef <- attr(x,"ref")
#'   lRef[[name]] <- value
#'   attr(x, "ref") <- lRef
#'   x
#' }
#'
#' #'@export
#' `[[<-.DopRefList` <- function(x, name, value) {
#'   lRef <- attr(x,"ref")
#'   lRef[[name]] <- value
#'   attr(x, "ref") <- lRef
#'   x
#' }
#'
#' #'@export
#' names.DopRefList <- function(x) {
#'   l_n <- names(attr(x,"ref"))
#'   l_n[!l_n %in% c("selfRef")]
#' }
#'
#' #'@export
#' length.DopRefList <- function(x) {
#'   length(names(x))
#' }
#'
#'
#' #'@export
#' print.DopRefList <- function(pDopRefList) {
#'   lRef <- attr(pDopRefList,"ref")
#'
#'   cat("dopRefList:", attr(lRef, "id"),
#'       " (",attr(lRef, "uid"),")\n")
#'
#'   for ( lVar in names(pDopRefList) ) {
#'       cat("$",lVar,": ")
#'       str(pDopRefList[[lVar]])
#'   }
#' }
