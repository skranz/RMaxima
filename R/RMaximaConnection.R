#' Interface between R and Maxima
#' Commands to establish connection to Maxima

#library(restoreDebug)
#library(skUtils)

.RMAXIMA.ENV = new.env(parent=.GlobalEnv)

start.maxima = function(out.path=tempdir(),maxima.path=NULL,use.pipe=TRUE,use.last=TRUE, use.log.file=TRUE,...)
{
  restore.point("start.maxima")
  #rerestore.point("start.maxima")
  
  #mx.closeConnection()
  mx = mx.default(out.path=out.path, maxima.path=maxima.path, use.pipe=use.pipe, use.log.file=use.log.file)

  .RMAXIMA.ENV$MAXIMA.KEYWORDS <- c(
    "sum","diff","integrate"  
  )
  
  
  try(mx <- open.maxima.pipe(mx,reopen=TRUE))
  MAXIMA_MX <<- mx
  invisible(mx)
}

open.maxima.pipe = function(mx,reopen=TRUE,verbose=!TRUE) {
  restore.point("open.maxima.pipe")
  #rerestore.point("open.maxima.pipe")
  # Check whether a pipe to maxima is already established
  if (!reopen) {
  if (exists("MAXIMA.PIPE")) {
    if (!is.null(MAXIMA.PIPE)) {
      if (isOpen(MAXIMA.PIPE)) {
        if (reopen) {
          rm(MAXIMA.PIPE)
          #close.maxima.pipe()
        } else {
          return(MAXIMA.PIPE);
        }
      }
    }
  }
  }
  #out.pipe = pipe(paste('"',mx$maxima.exec,'"' ,sep=""),"w+b")
  out.pipe = pipe(mx$maxima.exec,"w")
  
  #open(max.pipe)	
  if (!is.null(mx$maxima.header)) {
    header = read.text(mx$maxima.header,merge = FALSE)
  } else {
    header = ""
  }
  if (mx$use.log.file) {
    if (!file.exists(mx$logfile))
      file.create(mx$logfile)
    
    header = c(header,
      'display2d : false;',
      #'set_tex_environment_default ("?#S#", "?");',         
      paste('writefile("',mx$logfile,'");\n',sep=""),
      'load("linearalgebra");'
      #,'load("to_poly_solver");''
    )
    writeLines(header,out.pipe)
    MAXIMA.PIPE <<- out.pipe
    mx$pipe = out.pipe;
    mx$writeCon = out.pipe;
  
    mx$readCon = file(mx$logfile,open="r", blocking=FALSE)
    ret = readLines(mx$readCon)
    if (verbose)
      display(ret)
  } else {
    header = c(header
               ,'display2d : false;'
               #,'set_tex_environment_default ("?#S#", "?");'         
               )
    writeLines(header,out.pipe)
    MAXIMA.PIPE <<- out.pipe
    mx$pipe = out.pipe;
    mx$writeCon = out.pipe;
    
    if (verbose)
      display("Maxima pipe connection without logfile established...")
    
  }
  return(mx)
}

mx.closeConnection = function(mx=get.mx()) {
  try(close(mx$readCon),silent=TRUE)
  try(close(mx$writeCon),silent=TRUE)
  try(close(mx$pipe),silent=TRUE)
}

get.mx = function() {
  if (exists("MAXIMA_MX")) {
    mx=MAXIMA_MX
  } else {
    mx = start.maxima()
    MAXIMA_MX <<- mx
  }
  mx  
}

mx.default = function(maxima.path = NULL,  maxima.exec = NULL,  out.path=tempdir(), logfile = NULL,  outfile = NULL, orgscriptfile=NULL, scriptfile = NULL,  header.path = NULL,  maxima.header = NULL,  use.pipe = TRUE, use.log.file=TRUE
) {
  restore.point("mx.default")
  #rerestore.point("mx.default")
  mx = nlist(maxima.path,  maxima.exec,  out.path, logfile,  outfile ,  scriptfile,  header.path ,  maxima.header,  use.pipe, use.log.file)

  mx = copy.into.list(dest=mx, overwrite=FALSE, source=list(
         maxima.path="", 
         header.path=path.package("Rmaxima",quiet=TRUE)))

  # Only / works in Maxima path
  mx$out.path = gsub("\\","/",mx$out.path,fixed=TRUE)
  
  mx = copy.into.list(dest=mx, overwrite=FALSE, source=list(
    maxima.exec = get.maxima.exec(mx$maxima.path),
    logfile = paste(mx$out.path,"/maxima.log",sep=""),
    outfile = paste(mx$out.path,"/maxima_out.txt",sep=""),
    orgscriptfile = paste(mx$out.path,"/maxima_org.mac",sep=""),
    scriptfile = paste(mx$out.path,"/maxima_script.mac",sep="")))
  mx
}

get.maxima.exec = function(maxima.path) {
  if (!is.null(maxima.path)) {
    if (maxima.path == "") {
      maxima.exec = "maxima"
    } else {
      maxima.exec = paste(maxima.path,"/maxima",sep="")
    }
  }
  return(maxima.exec)
}



close.maxima.pipe = function() {
	if (exists("MAXIMA.PIPE")) {
		if (!is.null(MAXIMA.PIPE)) {
			if (isOpen(MAXIMA.PIPE)) {
				try(close(MAXIMA.PIPE))
			}
		}
	}
}

call.maxima = function(txt,mx=get.mx(),out.file = mx$outfile, use.pipe = mx$use.pipe,logfile=mx$logfile,all.silent=FALSE,clear=TRUE,show=FALSE,answers="") {
	restore.point("call.maxima.and.lyx")
	
	write.text("maxima error!",out.file)
  
  if (use.pipe) {
    # Start new log file
	  #writeLines('closefile();',mx$pipe)
    #if (file.exists(logfile)) {
    #  file.remove(logfile)
    #}
	  #writeLines(paste('writefile("',mx$logfile,'");\n',sep=""),mx$pipe)
	  if (clear) {
	    str = "kill(all);\n"
	    writeLines(str,mx$pipe)
	  }
  }
	
  
	# Generate a Maxima script from commands stored in txt
	txt = merge.lines(txt)	
  if (!all.silent) {
  	header = c(
  		paste('stream: openw("',out.file,'");',sep="")
  	)
  	
  	footer = c(
  		'newline(stream);',
  		'printf(stream,"~a","?? END MAXIMA CALL ??");',
  		'close(stream);'
  	)
  	txt = paste(paste(header,collapse="\n"),txt,paste(footer,collapse="\n"),sep="\n")
  }	
  	
	write.text(txt,mx$scriptfile)
	txt = sep.lines(txt)
  
  if (use.pipe) {
	  #mx$pipe = open.maxima.pipe(reopen=FALSE);
    #writeLines(txt,mx$pipe)
    
	  writeLines(paste('batch("',mx$scriptfile,'");',sep=""),mx$pipe);
    writeLines(answers,mx$pipe)
  } else {
    command = paste(mx$maxima.exec, " --batch ", '"',mx$scriptfile,'"',sep="")
    display("Call\n",command)
    #system(paste(maxima.bat," -h")) # command line parameters
    ret = system(command,intern = !TRUE,ignore.stdout = FALSE, ignore.stderr = FALSE)
  }
  #writeLines(txt,MAXIMA.PIPE)
}

read.maxima.out = function(mx=get.mx(),out.file = mx$outfile,logfile=mx$logfile,show=FALSE, time.out=20) {

	txt = read.text(out.file,merge=FALSE)
	restore.point("read.maxima.out")
	#rerestore.point("read.maxima.out")
	counter = 1 
	num = 0
  
  last.log.pos = 0
  
	while (TRUE) {
		if (length(txt)>0) {
    
			if (txt[NROW(txt)]=="?? END MAXIMA CALL ??") {
				break();
			}
		}
		Sys.sleep(0.1)
		#print("...")
		txt = read.text(out.file,merge=FALSE)
		if (num != NROW(txt)) {
			num = NROW(txt)
			counter = 1;
		}
		if ((counter %% 10)==0) {
      display(txt)
      if (last.log.pos==0) {
        if (counter==10) {
          display("Waiting for Maxima")
        } else {
          display(".",new.line=FALSE)          
        }
        
      }
      if (mx$use.log.file) {
        log.txt = readLines(logfile)
        if (length(log.txt)>last.log.pos) {
          display(log.txt[(last.log.pos+1):length(log.txt)],quote=FALSE)  
          last.log.pos = length(log.txt)
        }
      }
      #mx.write("unknown;")
     # mx.write("positive;")
			flush.console()
		}
		if (counter*0.1>time.out) {
      warning("Timeout for reading maxima.out...")
			display("Timeout for reading maxima.out...")
      
			flush.console()
			return(NULL)
		}
		counter=counter+1
	}
  #display("")
	txt = txt[-NROW(txt)]
	return(txt)
}

#' txt is a vector of maxima commmands
#' each line is a command without ;  at the end
add.maxima.printf = function(txt) {
  #txt = merge.lines(txt)
  #txt = sep.lines(txt,";")
  #txt = str_trim(txt)
  #txt = txt[txt!=""]
  
  txt = paste("printf(stream,",'"~a",',txt,'); \n newline(stream);',sep="")
  txt
}

#' Does not yet handle nested lists efficiently
mx.result.list = function(str) {
  library(stringr)
  restore.point("mx.result.list")
  #rerestore.point("mx.result.list")
  str = str_replace_all(str,fixed(" "),"")
  
  if (substr(str,1,1)=="[") {
    
    str = gsub("([^\\]])\\,",'\\1",',str, perl=TRUE)
    str=  gsub("\\,([^\\[])",',"\\1',str,perl=TRUE)
  
    str= gsub("\\[([^\\[])",'list("\\1',str, perl=TRUE) #)]
    str= gsub("\\[",'list(',str, perl=TRUE) #)]
    
    str= gsub("([^\\]])\\]",'\\1")',str,perl=TRUE) #)
    str= gsub("]",')',str,fixed=TRUE)
  
    #print(str,quote=FALSE)
    eval(parse(text=str))
  } else {
    str
  }
}

example.mx.result.list = function() {
  runmx("solve([y=2*x,x=y^2],[x,y])")
}

mx.write = function(str,  mx = get.mx()) {
  writeLines(str,mx$writeCon)
}

mx.read = function(n=-1L,mx=get.mx()) {
  str = readLines(mx$readCon,n)
  str
}

# Separate different input and output fields in a str
mx.sep.fields = function(txt) {
  
  library(stringr)
  tag = substr(txt,1,3)
  
  start.in  = which(tag == "(%i") #)  
  start.out = which(tag == "(%o") #)
  
  start.unknown = setdiff(1,c(start.in,start.out))
  start = c(start.unknown,start.in,start.out)
  
  txt.start = txt[c(start.in,start.out)]
  right.brace.pos = str_locate(substring(txt.start,1,10),fixed(")"))[,1]
  field.index = c(substring(txt.start,4,right.brace.pos-1), rep(NA,length(start.unknown)))

  # Remove field indices
  txt[c(start.in,start.out)] = substring(txt.start,right.brace.pos+1)
  
  txt = str_trim(txt)
  type = c(rep("i",length(start.in)),
           rep("o",length(start.out)),
           rep("u",length(start.unknown)))
  
  ord = order(start)
  start = start[ord]
  type = type[ord]
  field.index = field.index[ord]
  end = c(start[-1],length(txt)+1)-1
  
  fields = lapply(seq_along(start), function(i) txt[start[i]:end[i]])
  list(fields=fields,type=type,index=field.index)
  
}

example.mx.sep.fields = function() {
  mx = get.mx()
  mx.write("x:2$")
  mx.write("x^2;")
  mx.write("x:3$ x^2;")
  
  txt = mx.read()
  txt
  mx.sep.fields(txt)  
}

#' Run the maxima code given in str
#'
#' @param str Character vector with maxima code
#' @param clear if TRUE (default) clears all previous assignments by calling kill(all) in Maxima
#' @param time.out number of seconds to wait for result
#' @return a list with list and character representation of Maxima output
runmx = mx.run = function(str,clear=TRUE,use.pipe = NULL,show=FALSE,time.out=20,answers=rep(c("unknown;","positive;"),times=10),just.str=FALSE,assume=NULL,declare=NULL) {
  restore.point("runmx")
  #rerestore.point("runmx")
  mx = get.mx()
  if (is.null(use.pipe)) {
    use.pipe=mx$use.pipe
  }
  
  declare = mx.adapt.declare(declare)
  assume  = mx.adapt.assume(assume)
  
  str = c(declare,assume,str)
  
  str = merge.lines(str)
  str = sep.lines(str,";")
  str = str_trim(str)
  str = str[str!=""]
  

  
  call.str = str 

  writeLines(paste(call.str,";",sep=""),mx$orgscriptfile)
  str = add.maxima.printf(str)

  call.maxima(str,mx,use.pipe=use.pipe & (!show),clear=clear,answers=answers)
  ret = read.maxima.out(mx,time.out=time.out)
  str = ret[-length(ret)]
  if (length(c(declare,assume))>0) {
    str = str[-(1:length(c(declare,assume)))]
  }
  result.list = lapply(str,mx.result.list)
  if (just.str) {
    return(str)
  } else {
    list(li = result.list, call.str = call.str,str = str)
  }
}

example.runmx = function() {
  runmx("solve([y=2*x,x=y^2],[x,y])")
  runmx("solve([y=2*x,x=y^2],[x,y])",show=TRUE)
  

  #mx = start.maxima(out.path="D:/libraries/Rmaxima",use.pipe=TRUE)
  runmx("solve([y=2*x,x=y^2],[x,y])")
  #mx = start.maxima(out.path="D:/libraries/Rmaxima",use.pipe=TRUE)
  
  runmx("x : y;
        diff([2*x,x^2],x);")
  runmx("diff([2*x,x^2],x)",clear=FALSE)
  runmx("diff([2*x,x^2],x)",clear=TRUE)
  
  runmx("diff([2*x,x^2],x)")
}

.First.lib <- function(lib) 
{
  library(restoreDebug)
  print("Establishing maxima connection...",quote=FALSE)
  start.maxima()
  # next statement has no effect except on Windows XP Pro
}


# maximaStart <- function(mx=get.mx(),verbose = FALSE, method = c("socket", "system"), port = c(9736,9735,9734)[1])
# {
#   restore.point("maximaStart")
#   
#   server.string <- if(method == "socket") paste(" --server", port) else ""
#   
#   method <- match.arg(method)
#   if (method == "system") return()
#   if (!capabilities("sockets")) stop("no socket capabilties")
#   
#   cmd.str = paste(mx$maxima.exec, server.string)
#   #maximaStop(verbose = FALSE)
#   print(paste("Starting Maxima !",server.string))
#   # return path using defpath and deffile as fill-in defaults
#   
#   system(cmd.str, wait = FALSE)
#   
#   Sys.sleep(1)
#   assign(".mxCon", socketConnection(host = "127.0.0.1", port=9736, 
#                                      server = FALSE,
#                                      blocking = FALSE, open = "a+",
#                                      encoding = getOption("encoding")), .GlobalEnv)
#   invisible(0)
# }
# 
# maximaStart()
# 
# isConnection <- function(x) {
#   opened <- summary(x)$opened
#   identical(opened, "opened") || identical(opened, "closed")
# }
# 
# maximaStop <- function(verbose = TRUE) 
# {
#   if (exists(".mxCon", .GlobalEnv)) {
#     # if (isConnection(get(".mxCon", .GlobalEnv))) try(close(.mxCon))
#     .mxCon <- get(".maxiamaCon", .GlobalEnv)
#     if (isConnection(.mxCon)) {
#       writeLines("Exit();", .mxCon)
#       try(close(.mxCon))
#     }
#     rm(.mxCon, envir = .GlobalEnv)
#   }
#   # if (.Platform$OS.type == "windows") system("taskkill /im maxima.exe")
#   return(invisible(0))
# }
# 
# .Last.lib <- function(lib) 
# {
#   if (exists(".mxCon", .GlobalEnv)) maximaStop()
#   # next statement has no effect except on Windows XP Pro
# }
# 
# .First.lib <- function(lib) 
# {
#   library(restoreDebug)
#   print("Establishing maxima connection...",quote=FALSE)
#   start.maxima()
#   # next statement has no effect except on Windows XP Pro
# }
# 
# # proper counting of lines read in, and proper handling of them.
# 
# mx.run.socket <- function(str, verbose = FALSE, method, retclass = c("expression", "character", "unquote"), addSemi = TRUE, ...) {
#   # if connection does not exist or its not a connection
#   # or its closed, startup maxima.
#   if (!exists(".mxCon", .GlobalEnv) ||
#     !isConnection(get(".mxCon", .GlobalEnv)) ||
#     summary(get(".mxCon", .GlobalEnv))$opened == "closed")
#     maximaStart(verbose = isTRUE(verbose))
#     
#   mx.res <- c()
#   #	print(x)
#   if (!is.na(pmatch(verbose, c(TRUE, "input")))) 
#       cat("Sending to maxima:", str, "", sep = "\n")
#   .mxCon <- get(".mxCon", .GlobalEnv)
#   writeLines(str, .mxCon)
#     
#   delim <- "]"
#   mx.res <- c()
#   while (sum(delim == mx.res) < 2)
#   {
#     mx.out <- readLines(.mxCon)
#     mx.res <- c(mx.res, mx.out)
#   }
#   mx.res <- mx.res[mx.res != ""]
#   flush(.mxCon)
# }


