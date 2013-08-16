
#' Translate maxima code to R. VERY preliminary
maxima.to.r = function(str) {
  str = gsub("'diff","diff",str,fixed=TRUE)
  str  
}

#' Maxima matrix to an R matrix with the individual Maxima expressions
mx.to.r.matrix = function(mx.mat) {
  library(stringtools)
  library(stringr)
  start.pos = str.locate.first(mx.mat,"[")[,1]
  ret = str.locate.all(mx.mat,"]")[[1]]
  end.pos = ret[NROW(ret),1]
  str = substring(mx.mat,start.pos,end.pos)
  str = str_replace(str,fixed(" "),"")
  rows = str_trim(strsplit(str,split="],[",fixed=TRUE)[[1]])
  rows[1] = str.remove.ends(rows[1],1)
  rows[length(rows)] = str.remove.ends(rows[length(rows)],0,1)
  
  rows = str.split(rows,",",fixed=TRUE)
  
  mat = do.call(rbind,rows)
  return(mat)
}

#' An R matrix with individual Maxima strings to a Maxima matrix
r.to.mx.matrix = function(mat) {
  str = apply(mat,1,function(row) cc("[",cc(row,collapse=","),"]"))
  str = cc("matrix(",cc(str,collapse=","),")")
  str
}

#' A synonym for r.to.mx.matrix
mx.matrix = function(...) {r.to.mx.matrix(...)}

examples.r.to.mx.matrix = function() {
  mat = matrix(1:8,4,2)
  mat
  r.to.mx.matrix(mat)
  
  
  mx.mat = "matrix([1,5],[2,6],[3,7],[4,8])"
  mx.to.r.matrix(mx.mat)
}

#' Convert an R vector to a maxima list
mx.list = function(vec,brackets=TRUE) {
  if (is(vec,"cases"))
    return(mx.list(lapply(vec,mx.list)))
  if (brackets) {
    return(cc("[",cc(vec,collapse=","),"]")) 
  } else {
    return(cc(vec,collapse=","))
  }
}

#' Compute symbolically determinant of a matrix
mx.determinant = function(mat) {
  if (is.matrix(mat))
    mat = mx.matrix(mat)
  
  mx.run(cc("determinant(",mat,");"),just.str=TRUE)
}

#' Computes the hessian of a maxima expression
mx.hessian = function(f,var,to.r = TRUE) {
  restore.point("mx.hessian")
  #rerestore.point("mx.hessian")
  
  ret = mx.run(cc("hessian(",f,",",mx.list(var),");"),just.str=TRUE)
  
  if (to.r) {
    ret = mx.to.r.matrix(ret)
  }
  ret
}

examples.mx.hessian = function() {
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mx.hessian(f=f,var=var)
}

#' Computes the jacobian of a maxima expression
mx.jacobian = function(f,var,to.r = TRUE) {
  restore.point("mx.jacobian")
  #rerestore.point("mx.jacobian")
  
  ret = mx.run(cc("jacobian(",mx.list(f),",",mx.list(var),");"),just.str=TRUE)
  
  if (to.r) {
    ret = mx.to.r.matrix(ret)
  }
  ret
}

examples.mx.jacobian = function() {
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mx.jacobian(f=f,var=var)
}

#' Computes the bordered hessian matrix, used for concavity checks
mx.bordered.hessian = function(f,var,to.r = TRUE, jacobian=NULL, hessian = NULL) {
  restore.point("mx.bordered.hessian")
  #rerestore.point("mx.bordered.hessian")

  if (is.null(jacobian))
    jacobian = as.vector(mx.jacobian(f,var,to.r = TRUE))
  if (is.null(hessian))
    hessian = mx.hessian(f,var,to.r = TRUE)
  
  nv = length(var)+1
  mat = matrix("0",nv,nv)
  ind = 2:nv
  mat[1,ind] = jacobian
  mat[ind,1] = jacobian
  mat[ind,ind] = hessian
  
  if (!to.r) {
    mat = mx.matrix(mat)
  }
  mat
}

examples.mx.bordered.hessian = function() {
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mat = mx.bordered.hessian(f=f,var=var)
  mat
  mx.determinant(mat)
}

#' Get a symbolic condition for negative definiteness of a matrix
mx.negative.definite.cond = function(mat,with.descr=FALSE) {
  # Compute determinanet of r'th leading principal minor
  # and multiply by (-1)^r
  fun = function(r) {
    lead.princ.minor = mx.determinant(mat[1:r,1:r])
    cc(ifelse((-1)^r==-1,"-",""), "(",lead.princ.minor,")")  
  }
  cond = sapply(1:n,fun)
  
  if (!with.descr)
    return(cond)
  
  
  ret = list(
    descr=cc("if all cond are (weakly) positive for all variables ", cc(var,collapse=",")," then the matrix mat is (weakly) negative definite."),
    cond = cond
    )
  ret  
}

#' Returns the conditions from the principal minor test
#' that the function f is jointly quasi-concave in in all variables in var
#' @param f a string that specifies the Maxima formula of the function body
#' @param var a character vector specifying the variables
mx.concave.cond = function(f,var,with.descr=FALSE, hessian=NULL) {
  if (is.null(hessian))
    hessian = mx.hessian(f,var)

  cond = negative.definite.cond(hessian)
  
  if (!with.descr)
    return(cond)
  
  return(list(
    descr=cc("if all cond are strictly positive for all variables ", cc(var,collapse=",")," then the function ", f, " is concave."),
    cond = cond))
}

#' Perform comparative statics of a system of equations
#' 
#' Applies the implicit function theorem
mx.comparative.statics = function(F=all.lhs(eq.sys),var,par,eq.sys=NULL,...) {
  F = mx.list(F)
  var = mx.list(var)
  par = mx.list(par)
  code = paste0("-invert(jacobian(",F,",",var,")) . jacobian(",F,",",par,")");
  mx.run(code,...)
}

#' Returns the conditions from the principal minor test
#' that the function f is jointly quasi-concave in in all variables in var
#' @param f a string that specifies the Maxima formula of the function body
#' @param var a character vector specifying the variables
mx.quasi.concave.cond = function(f,var,with.descr=FALSE, jacobian=NULL, hessian=NULL) {
  bh = mx.bordered.hessian(f,var,jacobian=jacobian,hessian=hessian)
  n = NROW(bh)-1
  
  # Compute determinanet of r'th leading principal minor
  # and multiply by (-1)^r
  fun = function(r) {
    lead.princ.minor = mx.determinant(bh[1:(r+1),1:(r+1)])
    #cc("(",(-1)^r, ") * (",lead.princ.minor,")")
    cc(ifelse((-1)^r==-1,"-",""), "(",lead.princ.minor,")")  
  }
  cond = sapply(1:n,fun)
  
  if (!with.descr)
    return(cond)
  
  return(list(
    descr=cc("if all cond are strictly positive for all variables ", cc(var,collapse=",")," then the function ", f, " is quasi-concave. If one of the expressions is negative then the function is not quasi-concave."),
    cond = cond))
}

examples.mx.quasi.concave.cond = function(f,var) {
  f = "x^(1/2)*y^(1/2)"
  var = c("x","y")
  mx.quasi.concave.cond(f,var)

  f = "x^2+y^(1/2)"
  f = "(x^2)*(y^(1/2))"
  var = c("x","y")
  ret = mx.quasi.concave.cond(f,var)
  ret
  mx.sign(cond,assume="x>=0,y>=0")

  f = "(x^2)*(y^(1/2))"
  f = "x^(1/2)*y^(1/2)"
  
  mx.check.concavity(f,var,assume="x>0,y>0")
  
  pi.i =  "(a*(qi+Q_i)^(-b))*qi-c1*qi"
  dpi.i = mx.diff(pi.i,"qi")
  ddpi.i = mx.diff(dpi.i,"qi")
  
  
  mx.check.concavity(pi.i,"qi", assume="a>0,b>0,b<1,c1>0,Q_i>0,qi>0")
  
}

#' Returns sufficient conditions from a principal minor test
#' that the function f is jointly concave in in all variables in var
#' @param f a string that specifies the Maxima formula of the function body
#' @param var a character vector specifying the variables
mx.check.concavity = function(f,var,assume=NULL) {
  # Conditions for quasiconcavity
  
  hessian = mx.hessian(f,var)
  jacobian = mx.jacobian(f,var)
  
  if (FALSE) {
  conc.cond = mx.concave.cond(f,var,hessian=hessian)
  conc.sign = mx.sign(conc.cond,assume=assume)

  if (all(conc.sign=="pos")) {
    conc = "TRUE"
  } else if (any(qc.sign=="neg")) {
    conc = "FALSE"
  } else {
    qc = "unkown"
  }
  }
  
  qc.cond = mx.quasi.concave.cond(f,var,hessian=hessian,jacobian=jacobian)
  qc.sign = mx.sign(qc.cond,assume=assume)

  if (all(qc.sign=="pos")) {
    qc = "true"
  } else if (any(qc.sign=="neg")) {
    qc = "false"
  } else {
    qc = "unknown"
  }
  
  if (qc=="true") {
    strict.qc.sign = mx.sign(jacobian,assume=assume)
    if (all(strict.qc.sign=="pos")) {
      strict.qc = "true"
    } else {
      strict.qc = "unknown"      
    }   
  } else {
    strict.qc = qc
  }
  
  sufficient = list(
    quasi.concave = cc(qc.cond,">0"),
    strict.quasi.concave=c(cc(qc.cond,">0"),cc(jacobian,">0"))
  )
  ret = nlist(jacobian,hessian,sufficient=sufficient,
              strict.quasi.concave=strict.qc,quasi.concave=qc)  
  ret
}

#' Tries to deduce the sign of a maxima expression
mx.sign = function(expr,assume=NULL,declare=NULL,just.str=TRUE) {
  mx.run(cc("sign(",expr,");"),assume=assume,declare=declare,just.str=just.str)
}

#' Takes a system of equations L=R and gets L-R 
get.all.lhs = function(eq) {
  return(paste(lhs(eq)," - (", rhs(eq),")"))
}

#' Separates from a vector of (in-)equalities the lhs, comparison operator and rhs
lhs.dir.rhs  = function(str) {
  pos1 = str_locate(str, "<")
  pos2 = str_locate(str,">")
  pos3 = str_locate(str,"=")
  pos.left = pmin(pos1[,1],pos2[,1],pos3[,1], na.rm=TRUE)
  pos.right = pmax(pos1[,2],pos2[,2],pos3[,2], na.rm=TRUE)
  lhs = str_trim(substring(str,1,pos.left-1))    
  dir = str_trim(substring(str,pos.left,pos.right))
  rhs = str_trim(substring(str,pos.right+1))
  lhs[is.na(lhs)] = str[is.na(lhs)]
  list(lhs=lhs,dir=dir,rhs=rhs)
}
examples.lhs.dir.rhs = function(str) {
  lhs.dir.rhs(str<-c("x+5>=y","a+4", "4=3"))
}

#' Extracts the lhs from a maxima equation
lhs = function(str) {
  pos=str_locate(str,fixed("="))[,1]
  substring(str,1,pos-1)  
}


#' Extracts the rhs from a maxima equation
rhs = function(str) {
  pos=str_locate(str,fixed("="))[,1]
  substring(str,pos+1)  
}


mx.adapt.declare = function(declare) {
  if (is.null(declare)) {
    return(declare)
  }
  declare = str_trim(declare)
  declare.rows = substring(declare,1,8)=="declare("
  
  declare[!declare.rows] = paste("declare(",declare[!declare.rows],");")
  declare  
}

mx.adapt.assume = function(assume) {
  if (is.null(assume)) {
    return(assume)
  }
  assume = str_trim(assume)
  assume.rows = substring(assume,1,7)=="assume("
  
  assume[!assume.rows] = paste("assume(",assume[!assume.rows],");")
  assume  
}

#' Solve eq for variables var
mx.solve = function(eq,var,radcan=!TRUE,to_poly_solve=!TRUE,semi.colon=";", just.code = FALSE,declare=NULL,assume=NULL, real.only=TRUE) {
  restore.point("mx.solve")
  #rerestore.point("mx.solve")
  
  #if (is.list(eq))
  #  stop("Not yet implemented for equations")
  var.names = var
  eq = mx.list(eq)
  var = mx.list(var)
  
  if (to_poly_solve) {
    str = paste("to_poly_solve(",eq,",",var,")",sep="")
  } else {
    str = paste("solve(",eq,",",var,")",sep="")
    
    if (radcan) {
      str = paste("ev(",str,",radcan)")
    }
  }
  str = paste(str,semi.colon)
  
  declare = mx.adapt.declare(declare)
  assume  = mx.adapt.assume(assume)
  
  str = c(declare,assume,str)
  
  if (!just.code) {
    ret = runmx(str)
    li = ret$li[[1]]
    
    num.sol = length(li)
    if (real.only & num.sol>0) {
      restore.point("jhjhj")
      has.imaginary = sapply(li, function(sols) {
        any(grepl("%i",unlist(sols),fixed=TRUE))
      })
      li = li[!has.imaginary]
      num.sol = length(li)
    }
    
    if (num.sol > 1) {
      cases = new.cases(li)
      return(cases)
    } else {
      res = unlist(li[[1]])
      names(res) = names(li[[1]])
      return(res)
    }
    #return(eval(parse(text=runmx(str))))
  } else {
    return(str)
  }
}

examples.mx.solve = function() {
  
  runmx("solve([y=2*x,x=y^2],[x,y])")
  eq = c("y=2*x","x=y^2")
  var = c("x","y")
  mx.solve(eq,var)
}

#' Simplifies an expression
#' 
#' By default uses gcfac from the Maxima library scifac
mx.simplify = function(str, simplify.code = c("gcfac(factor(...));"[1]), just.code = FALSE,declare=NULL,assume=NULL) {

  code = str_replace(simplify.code,fixed("(...)"),paste("(",str,")"))
  
  declare = mx.adapt.declare(declare)
  assume  = mx.adapt.assume(assume)
  
  code = c(declare,assume,code)
  
  
  if (!just.code) {
    return(runmx(str))
  } else {
    return(str)
  }
}

#' Differentiate an expression
mx.diff = function(expr,var,times=1, just.code = FALSE,declare=NULL,assume=NULL,just.str=TRUE) {
  restore.point("mx.diff")
  #rerestore.point("mx.diff")
  
  code = paste("diff(",expr,",",var,",",times,");",sep="")
  declare = mx.adapt.declare(declare)
  assume  = mx.adapt.assume(assume)
  code = c(declare,assume,code)
  
  
  if (just.code) {
    return(code)  
  }
  ret = return(mx.run(code,just.str=just.str))
}

examples.mx.diff = function() {
  mx.diff("x^3+y","x",1)
  mx.diff("(a-b*q)*q-c*q-c2*q^2","q",1)
  mx.diff("(a-b*q)*q-c*q-c2*q^2","q",2)
  
}

#' Generate Kuhn-Tucker-Conditions (usable in a MCP) for a given objective
mx.gams.mcp = function(expr,var,fun.name = "fun",has.lo=rep(TRUE,length(var)),has.up=rep(TRUE,length(var)),
lo=paste("lo_",var,sep=""),up=paste("up_",var,sep=""),
shadow.lo = paste("mu_lo_",var,sep=""),shadow.up = paste("mu_up_",var,sep=""),
constr.lo=paste("constr_lo_",var,sep=""), constr.up=paste("constr_up_",var,sep=""),       
                          declare=NULL,assume=NULL,simplify=TRUE) {
  restore.point("mx.gams.mcp")
  #rerestore.point("mx.gams.mcp")
  stopifnot(length(expr)==1)
    
  nv = length(var)
  code = rep("",nv)
  
  
  
  first.diff = mx.diff(expr,var,just.str = TRUE)
  
  second.diff = mx.diff(expr,var,2,just.str = TRUE)
  
  slo = paste("+",shadow.lo)
  slo[!has.lo] = ""
  sup = paste("-",shadow.up)
  sup[!has.up] = ""
  
  FOC.comment = paste("\n* Derivative w.r.t. ", var, " of ", fun.name, ": ", expr, "\n", sep="")
  FOC.name = paste(fun.name,"_FOC_",var,sep="")
  FOC  = paste(FOC.comment,FOC.name," .. (",first.diff,slo,sup,") =e= 0;",sep="")
  FOC = sep.lines(merge.lines(FOC,collapse="\n"),collapse="\n")
  
  
  
  make.ineq.constr = function(i) {
    if (has.up[i]) {
      up.constr = paste(constr.up[i]," .. ", up[i],"-",var[i]," =g= 0;", sep="")
    } else {
      up.constr = ""
    }
    if (has.lo[i]) {
      lo.constr = paste(constr.lo[i]," .. ",var[i]," =g= ", lo[i],";", sep="")
    } else {
      lo.constr = ""
    }
    c(lo.constr,up.constr)
  }
  ineq.constr = t(sapply(seq_along(var),make.ineq.constr)) 
  var.tag = paste("Variable ", var, ";")
  positive_var = paste("Positive Variable ", c(shadow.lo[has.lo],shadow.up[has.up]), ";")
  
  complementarity = c(paste(constr.lo[has.lo],".",shadow.lo[has.lo],sep=""),
                      paste(constr.up[has.up],".",shadow.up[has.up],sep=""))
  model = c("* Complementarity constraints are specified after model",
    paste("model my_model /", paste(complementarity, collapse=","), " / ;"),
    "solve my_model using mcp;")
  
  equations = c("Equations", FOC.name, constr.lo[has.lo], constr.up[has.up], ";")
  
  ind = sep.lines(merge.lines(split.var.ind(var)[,2],","),",")
  ind = ind[ind!=""]
  Sets = ""
  
  if (length(ind)>0) {
    Sets = c("Sets ", unique(ind),";")  
  }
  
  post_solve = c(paste("*Parameters", fun.name, ";"),paste("*",fun.name,"=", expr,";"), paste("*Display", fun.name,";"))
  all = c("
* GAMS Code Skeleton generated by R
* Needs manual adjusted to define stuff over sets!

",
  Sets,"",var.tag,positive_var,"",equations, "",FOC,"",ineq.constr,"", model,"",post_solve)
  
  all = str_replace_all(all,fixed("["),"(")
  all = str_replace_all(all,fixed("]"),")")
  
  cat(all,fill=!TRUE,sep="\n")
  writeClipboard(merge.lines(all,"\n"))
  cat("\nWritten to clipboard...")
  return(invisible(nlist(first.diff = first.diff, second.diff=second.diff,all)))
}
examples.gams.mcp = function() {
  inv.demand = "(a*(q1+q2)^b)" # iso.elastic
  cost  = "(c1*q1+c2*q1^2)+(c1*q2+c2*q2^2)" # quadratic
  profit = paste(inv.demand, "*(q1+q2) +",cost)
  profit  
  mx.gams.mcp(profit,c("q1","q2"),"profit")

  inv.demand = "(a*(q[i]+q_i)^b)" # iso.elastic
  cost  = "(c1*q[i]+c2*q[i]^2)" # quadratic
  profit = paste(inv.demand, "*(q[i]) +",cost)
  profit  
  mx.gams.mcp(profit,c("q[i]"),"profit")
    
}

