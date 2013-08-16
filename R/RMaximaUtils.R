#' Just a short cut
cc = function(...) {
  paste0(...)
}

#' Copy elements of an environment into a list
copy.into.list = function(dest=NULL, source=sys.frame(sys.parent(1)), names = NULL,exclude=NULL,overwrite=TRUE) {
  if (is.null(dest)) {
    if (is.list(source)) {
      return(source)
    } else {
      dest=list()
    }
  }
  stopifnot(is.list(dest))
  
  if (!overwrite) {
    excluder = c(exclude,names(dest))
  }
  if (is.environment(source)) {
    if (is.null(names))
      names = ls(envir=source)
    
    names = setdiff(names,exclude)
    for (na in names) {
      dest[[na]]=get(na,envir=source)
    }
  }
  if (is.list(source)) {
    if (is.null(names))
      names = names(source)
    names = setdiff(names,exclude)
    for (na in names) {
      dest[[na]]=source[[na]]
    }
  }
  return(dest)
}

#' transforms c("A","B") into "A\nB"
merge.lines = function(txt, collapse = "\n") {
  paste(txt,collapse=collapse)
}

#' transforms "A\nB" into c("A","B") 
sep.lines = function(txt, collapse = "\n") {
  library(stringr)
  if (length(txt)>1)
    txt = merge.lines(txt,collapse)
  str_split(txt,fixed(collapse))[[1]]
}


#' Creates a list that is named by the names of its arguments
named.list = function(...) {
  li = list(...)
  li.names = names(li)
  
  names = unlist(as.list(match.call())[-1])
  if (!is.null(li.names)) {
    no.names = li.names == ""
    names(li)[no.names] = names[no.names]
  } else {
    names(li) = names
  }
  li
}

#' Shortcut for named list
nlist = function(...) {
  named.list
}

#' Display stuff in a convenient form
display = function(...,collapse="\n",sep="",new.line=TRUE) {
  if (new.line) {
    str = paste("\n",paste(...,collapse=collapse,sep=sep),"\n",sep="")
  } else {
    str = paste(...,collapse=collapse,sep=sep)
  }
  invisible(cat(str,fill=FALSE))
  #print(str,quote=FALSE)
}

split.var.ind = function(var) {
  var = str_trim(var)
  pos=str_locate(var,fixed("["))[,1]
  left = substring(var,1,pos-1)
  ind = substring(var,pos+1,nchar(var)-1)
  left[is.na(pos)] = ind[is.na(pos)] = ""
  cbind(left,ind)
}

#' warpper to readLines, merge results to a single string
read.text = function(fn,merge.lines=FALSE,warn=FALSE) {
	con <- file(fn, "r")
	txt = readLines(con,warn=warn) # empty
	close(con)
	if (merge.lines) {
		txt = paste(txt,collapse="\n")
	}
	return(txt)
}


#' wrapper to writeLines opens and closes connection
write.text = function(text,fn,...) {
	con <- file(fn, "w",...)
	writeLines(text, con = con, sep = "\n", useBytes = FALSE)
	close(con)
}



#' Extract names of all variables from a maxima expression
extract.maxima.var = function(str) {
  restore.point("extract.maxima.var")
  #rerestore.point("extract.maxima.var")

  library(stringr)
  var = str_extract_all(str,'[a-zA-Z]+[0-9a-zA-Z_]*')
  fun = function(v) {
    v = unique(v)
    setdiff(v,.RMAXIMA.ENV$MAXIMA.KEYWORDS)
  }
  var = lapply(var,fun)
  return(var)
}


examples.extract.maxima.var = function() {
  extract.maxima.var("diff([x^2-y],[x])")
}

#' Locate positions (a stringtools pos) of all variables in a maxima expression string 
locate.maxima.var = function(str, var = extract.maxima.var(str)) {
  restore.point("locate.maxima.var")
  #rerestore.point("locate.maxima.var")
  
  library(stringtools)
  
  stopifnot(length(str)==1)
  
  if (is.list(var))
    var = unlist(var)
  all.var = str.extract.all(str,'[a-zA-Z]+[0-9a-zA-Z_]*',fixed=FALSE)[[1]]
  all.loc = str.locate.all(str,'[a-zA-Z]+[0-9a-zA-Z_]*',fixed=FALSE)[[1]]

  fun = function(v) {
    rows = all.var == v
    all.loc[rows,,drop=FALSE]
  }
  ret = lapply(var,fun)
  names(ret) = var
  ret
}
examples.locate.maxima.var = function() {
  locate.maxima.var("diff([x^2-y],[x])")  
}

#' Replace in a maxima expression string variables org by new
replace.maxima.var = function(str, org, new)  {
  restore.point("replace.maxima.var")
  #rerestore.point("replace.maxima.var")
  
  library(stringtools)
  
  stopifnot(length(str)==1)
  
  # Not yet very efficient
  for (i in seq_along(org)) {
    loc = locate.maxima.var(str,org[i])
    str = str.replace.at.pos(str,loc, list(rep(new[i],NROW(loc[[1]]))) )[[1]]
  }
  str
}
examples.replace.maxima.var = function() {
  replace.maxima.var("diff([x^2-y],[x])",c("x","y"),c("alpha","beta"))  
}