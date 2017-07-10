# dxcR: Data eXChnage in R
#

default_server <- new.env(emptyenv())
default_server$listen_addr <- "0.0.0.0"
default_server$listen_port <- 8889
default_server$repository <- ".Dxcr_data"

#'@export
setServerOptions <- function( listen, port, repository) {
  if (!missing(listen))
    default_server$listen_addr <- listen
  if (!missing(port))
    default_server$listen_port <- port
  if (!missing(port))
    default_server$repository <- repository
}

#'@export
startDxcrServer <- function(background = TRUE) {
  if (background) {
    startServer <- function(host, port, app, interruptIntervalMs=100) {
      p <- parallel:::mcparallel({
        server <- httpuv::startServer(host, port, app)

        parallel:::sendMaster(server)

        on.exit({
          httpuv::stopServer(server)
          })

        while (TRUE) {
          httpuv:::service(interruptIntervalMs)
          Sys.sleep(0.001)
        }

      })
      return(attr(parallel:::readChild(p), "pid") )
    }
    default_server$stopServer <- function(pid) system(paste0("kill ",pid))
  }
  else {
    startServer <- httpuv::runServer
    default_server$stopServer <- httpuv::stopServer
  }

  # req$SERVER_NAME,
  # req$SERVER_PORT,
  # req$SCRIPT_NAME,
  # req$PATH_INFO,
  # req$QUERY_STRING
  # req$REQUEST_METHOD
  # req$REMOTE_ADDR
  #

  if (is.null(default_server$session_id)) {
    default_server$session_id <- startServer(
      default_server$listen_addr , default_server$listen_port,
      app = list(
        call=function(req){
          l_shares <- list.files(default_server$repository)

          l_path <- gsub("^/", "" ,req$PATH_INFO)
          if (!l_path %in% l_shares) {
            body <- paste('<h1>Http File Transfer in R (Dxcr)</h1>
                          Unknwon file ', req$PATH_INFO)
            return(list(
              status = 404L,
              headers = list(
                'Content-Type' = 'text/html'
              ),
              body = body
            ))
          } else {
            body <- body <- readBin(file(paste0(".Dxcr_data",l_path), raw = T, open = 'rb'), what = "raw", n = 1000)
            return(list(
              status = 200L,
              headers = list(
                'Content-Type' = "application/octet-stream",
                'Content-Disposition' = paste0('attachment;filename=\"',l_path,'\"')
              ),
              body = body
            ))
          }
          }
        )
      )
  }
}

#'@export
stopDxcrServer <- function() {
  if (!is.null(default_server$session_id)) {
    default_server$stopServer(default_server$session_id)
    default_server$session_id <- NULL
  }
}

#'@export
share_data <- function(data="nothing so far", public=FASLE, expiration=Sys.Date()+5) {
  startDxcrServer()

  return()
}

share_location <- function(path, public=FASLE, writable=FALSE, expiration=Sys.Date()+5) {
  startDxcrServer()

  return()
}


#'@export
post_to_location <- function(tocken, path, url, data) {
system('
curl \
  -F "userid=1" \
  -F "filecomment=This is an image file" \
  -F "image=@Dxcr.Rproj" \
  http://127.0.0.1:8889/toto3.php?tockeh=999
       ')
}

#'@export
get_from_location <- function(tocken, path, url, data) {

}
