default_lineage <- new.env(emptyenv())
default_lineage$element_index <- 0
default_lineage$var_list <- list()

#'@export
cleanElements <- function() {
  if (is.null(lineage))
    l_lineage <- default_lineage
  else
    l_lineage <- lineage
  l_lineage$element_index <- 0
  l_lineage$var_list <- list()
  invisible(NULL)
}

find_names_in <- function(search_names, lang) {
  l_ret <- search_names %in% as.character(lang)
  names(l_ret) <- search_names
  
  if (length(lang) > 1) {
    l_remaining_names <- search_names[!l_ret]
    l_remaining_found <- Reduce(`|`, lapply(lang, function(x) find_names_in(l_remaining_names, x)))
    
    l_ret[l_remaining_found==TRUE] <- TRUE
  }
  return(l_ret)
}

#'@export
#'
addElements <- function(code, force.as.element=F, node=NULL, lineage=NULL, env= parent.frame()) {
  if (is.null(lineage))
    l_lineage <- default_lineage
  else
    l_lineage <- lineage
  
  l_code_vars <- substitute(code)
  names_in_env <- ls(envir = env)
  l_uses_vars <- as.logical(find_names_in(names_in_env, l_code_vars)[names_in_env])
  
  l_lineage$element_index <- l_lineage$element_index+1
  l_var_table <- data.table::data.table(name=names_in_env)
  l_var_table[,addr:=""]
  l_var_table[,env_addr:=address(environment())]
  l_var_table[,returned:=FALSE]
  l_var_table[,uses:=l_uses_vars]
  for (n in l_var_table$name) {
    l_var <- get(n, envir = env)
    l_var_table[name==n, addr:=data.table::address(l_var)]
    l_var_table[name==n, class:=class(l_var)[[1]]]
    l_var_table[name==n, attr:=fastdigest::fastdigest(attributes(l_var))]
  }
  l_lineage$var_list[[l_lineage$element_index]] <- l_var_table
  
  l_last_var <- eval(code, envir = env)
  
  names_in_env <- ls(envir = env)
  l_uses_vars <- as.logical(l_uses_vars[names_in_env])
  l_uses_vars[is.na(l_uses_vars)] <- FALSE
  
  l_lineage$element_index <- l_lineage$element_index+1
  l_var_table <- data.table::data.table(name=names_in_env)
  l_var_table[,addr:=""]
  l_var_table[,env_addr:=address(environment())]
  l_var_table[,returned:=FALSE]
  l_var_table[,uses:=l_uses_vars]
  for (n in l_var_table$name) {
    l_var <- get(n, envir = env)
    l_var_table[name==n, addr:=data.table::address(l_var)]
    l_var_table[name==n, class:=class(l_var)[[1]]]
    l_var_table[name==n, attr:=fastdigest::fastdigest(attributes(l_var))]
    l_var_table[,returned:=data.table::address(l_var)==data.table::address(l_last_var)]
  }
  l_lineage$var_list[[l_lineage$element_index]] <- l_var_table
  
  invisible(NULL)
}


#'@export
plotElement <- function() {
  vars <- rbindlist(default_lineage$var_list, fill = T, idcol = "index")
  vars[,element:=floor( (index-1) / 2)]
  vars[,title:=name]
  vars[,uid:=paste0(addr, attr)]
  
  vars[uses==T,use_count:=.N, by=.(uid)]
  vars[is.na(use_count),use_count:=0L, by=.(uid)]
  vars[,use_count:=max(use_count), by=.(uid)]
  
  vars[returned==T,parent_index:=index[1], by=.(uid)]
  vars[is.na(parent_index),parent_index:=0]
  vars[,parent_index:=max(parent_index), by=.(uid)]
  
  vars[returned==T,output_index:=index]
  vars[is.na(output_index),output_index:=0]
  vars[,output_index:=max(output_index), by=.(element)]
  
  vars[,new:=.N==1, by=.(element, name)]
  vars[,modified:=.SD[,.N,by=.(addr, class, attr )]$N, by=.(element, name)]
  
  l_returned <- vars[returned==T]
  l_used <- vars[uses==T]
  l_local_vars <- l_used[!name %in% l_returned$name]
  l_pure_vars <-  vars[index==vars[uses==T | returned == T, .(.N,index=index[.N] ), by=.(element)][N==1]$index]
  
  nodes <- rbind(
    l_local_vars[,.(id=paste0(rep("imp_",.N),index), title, shape=rep("triangle",.N), color=rep('grey',.N), size=rep(6, .N) )],
    l_pure_vars[,.(id=paste0(rep("imp_",.N),index), title, shape=rep("triangle",.N), color=rep('black',.N), size=rep(6, .N) )],
    l_returned[,.(id=paste0(rep("proc_",.N),index), title, shape=rep("circle",.N), color=rep("blue",.N), size=rep(1, .N) )],
    l_returned[use_count==0,.(id=paste0(rep("exp_",.N),index), title, shape=rep("triangle",.N) , color=rep("grey",.N), size=rep(6, .N) )]
  )
  
  edges <- rbind(
    l_local_vars[,.(from=paste0(rep("imp_",.N),index), to=paste0(rep("proc_",.N),index))],
    l_pure_vars[,.(from=paste0(rep("imp_",.N),index), to=paste0(rep("proc_",.N),index))],
    vars[use_count>0 & new==FALSE,.(from=paste0(rep("proc_",.N),output_index), to=paste0(rep("proc_",.N),parent_index))],
    l_returned[use_count==0,.(from=paste0(rep("proc_",.N),index), to=paste0(rep("exp_",.N),index))]
  )
  
  visNetwork::visNetwork(nodes, edges)
}

