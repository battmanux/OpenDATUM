# .onLoad <- function(libname, pkgnam) {
#   op <- options()
#   op.dop <- list(
#     dop.datawarehouse = "./DataWarehouse"
#   )
#   toset <- !(names(op.dop) %in% names(op))
#   if(any(toset)) options(op.dop[toset])
#
#   l_m <- attr(methods(class = "data.table"), "info")
#   for (m in l_m[l_m$from=="registered S3method", "generic" ]) {
#     l_func <- get(x = paste0(m,".data.table"), envir = asNamespace("data.table"))
#     print(paste0(m,".dopNode"))
#     assign(paste0(m,".dopNode"),
#            function(what) {
#              l_func(l_iris, args)
#            } ,envir =  .GlobalEnv )
#   }
#
#   invisible()
# }
#
# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage("OpenDatum: Open source data platform for data scientists\nhttp://www.opendatum.net/ ")
# }
#
# .onUnload <- function(...) {
#   cat("From command line?")
# }
