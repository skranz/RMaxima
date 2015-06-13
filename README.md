RMaxima
===============================================================

**Date: 2013-08-15**

**Author: Sebastian Kranz**

RMaxima is an interface between R and the computer algebra system Maxima. In addition the package has some functions for mathematical operations.

## Experimental, not robust and not well documented

RMaxima is an experimental project that has not grown out an early stage. While it usually runs on my computer, errors often occur. Unfortunately, it is not well documented and currently, I don't actively work on it.

## Installation under Windows

Sorry, so far the package is only tested under windows. You first must install Maxima separately. You should also add the /bin directory of Maxima to the Windows PATH variable.

You can install the package from GitHub in the usual fashion.

```s
library(devtools)
install_github("skranz/restorepoint")
install_github("skranz/stringtools")
install_github("skranz/RMaxima")
```


## Trying out


```s
library(restorepoint)
library(RMaxima)

start.maxima()
mx.run("solve([x^2-y=0],x)", just.str = TRUE)

f = "x^2+y^2+x^2*y^2"
var = c("x", "y")
mx.hessian(f = f, var = var)
```


