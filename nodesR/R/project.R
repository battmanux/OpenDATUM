

#'@export
dopProject <- function(path=".") {
  lProject <- dopRefList("dopProjectDescription")
  lProject$path <- path
  return(lProject)
}
