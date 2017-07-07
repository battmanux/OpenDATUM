#'@export
big.table <- function(code,
                      qualifiers=list(name="iris"),
                      attributes=list(modification_time=Sys.time()),
                      node_attributes=list(creation_time=Sys.time()),
                      env=parent.frame()) {
  code <- substitute(code)

  self <- new.env(emptyenv())

  self$env <- env

  self$code_list <- list()
  self$code_list[[fastdigest::fastdigest(code)]] <- code

  self$attributes <- node_attributes
  self$tables_attributes <- data.table::as.data.table(attributes)
  self$tables_attributes$attr_name_hash  <- fastdigest::fastdigest(levels(ordered(names(attributes))))
  self$tables_attributes$code_hash <- fastdigest::fastdigest(code)

  self$tables_qualifiers <- data.table::as.data.table(qualifiers)
  self$tables_list <- list()

  l_out <- eval(code, envir = env )
  if (inherits(l_out, "data.frame"))
    data.table::setDT(l_out)

  self$tables_list[[1]] <- l_out
  l_ret <- self$tables_list[[1]]
  attr(l_ret, ".internal.self_big.table") <- self
  class(l_ret) <- c("big.table",class(l_ret))

  return(l_ret)
}

#'@export
`[.big.table` <- function(bt, ...) {
  self <- attr(bt, ".internal.self_big.table")
  l_out <- data.table::rbindlist(self$tables_list, idcol = 'id', fill = T)
  self$tables_attributes[,id:=.N]
  self$tables_qualifiers[,id:=.N]
  l_out2 <- merge(self$tables_attributes, l_out, by="id")
  l_out3 <- merge(self$tables_qualifiers, l_out2, by="id")
  l_out4 <- data.table:::`[.data.table`(l_out3, ...)
  attr(l_out4, ".internal.self_big.table") <- self
  class(l_out4) <- c("big.table", class(l_out4))
  return(l_out4)
}

#'@export
print.big.table <- function(x) {
  l_data <- data.table::copy(x)
  class(l_data) <- grep("big.table",class(l_data), value = T, invert = T)
  l_attr <- unlist(lapply(names(l_data), function(i) length(unique(l_data[[i]])))) == 1
  l_attr_list  <- l_data[1,l_attr, with=F]
  l_data_table <- l_data[,!l_attr, with=F]

  cat("Attributes:\n")
  print(l_attr_list)
  cat("Table:\n")
  print(l_data_table, topn = 3, class = T)
  invisible(NULL)
}

#'@export
code <- function(x) {
  UseMethod("code",x)
}

#'@export
code.big.table <- function(x) {
  self <- attr(x, ".internal.self_big.table")
  self$code_list[1]
}

