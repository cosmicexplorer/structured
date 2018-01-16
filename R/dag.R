### dag.R -- insert description here
### This file is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 3, or (at your option)
### any later version.

### This file is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @import magrittr



### parameters and products (ins and outs and ins)
### these use the SAME TYPES!!!!
setClass('Value', representation())



isValidTypeName <- function(x) {
    is.character(x) &&
        (length(x) == 1) &&
        isTRUE(x != '')
}

setClass('TypeName', slots=c(name='character'))
setMethod('initialize', 'TypeName', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    stopifnot(isValidTypeName(.Object@name))
    .Object
})

setGeneric('isType', function(typeName, object) {
    standardGeneric('isType')
})

setMethod('isType', signature(
    typeName='TypeName',
    object='ANY'
), function(typeName, object) {
    is(object, typeName@name)
})

### some useful macros to define tasks
## check if list, not empty, all named, no repeated names
checkUniquelyNamedList <- function(x) {
    stopifnot(is.list(x))
    stopifnot(length(x) > 0)
    listNames <- names(x)
    stopifnot(!is.null(listNames))
    stopifnot(!any(listNames == ''))
    stopifnot(!anyDuplicated(listNames))
    x
}

setClass('NamedArgumentList', slots=c(args='list'))
setMethod('initialize', 'NamedArgumentList', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@args <- checkUniquelyNamedList(.Object@args)
    .Object
})

setClass('ValueArgumentList', contains='NamedArgumentList')

setClass('TypedArgumentList', contains='NamedArgumentList')
setMethod('initialize', 'TypedArgumentList', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@args <- lapply(.Object@args, function(typeNameStr) {
        new('TypeName', name=typeNameStr)
    })
    .Object
})

setGeneric('checkTypedArgs', function(argTypes, argValues) {
    standardGeneric('checkTypedArgs')
})

## NOTE: checks types in order of declared types, not invoked values
setMethod('checkTypedArgs', signature(
    argTypes='TypedArgumentList',
    argValues='ValueArgumentList'
), function(argTypes, argValues) {
    declaredNames <- names(argTypes@args)
    givenNames <- names(argValues@args)
    stopifnot(setequal(declaredNames, givenNames))
    for (name in declaredNames) {
        type <- argTypes@args[[name]]
        value <- argValues@args[[name]]
        stopifnot(isType(type, value))
    }
    argValues
})

### define types by strings (?) because then they don't have to exist until
### "runtime"
setClass('Signature', slots=c(
    inputs='TypedArgumentList',
    output='TypeName'
))

setClass('TypedFunction', slots=c(
    signature='Signature',
    f='function'
))

setClass('TypedCall', slots=c(
    fun='TypedFunction',
    argValues='ValueArgumentList'
))

setGeneric('evaluate', function(typedCall, env) {
    standardGeneric('evaluate')
})

setMethod('evaluate', signature(
    typedCall='TypedCall',
    env='environment'
), function(typedCall, env) {
    fun <- typedCall@fun
    sig <- fun@signature
    argValues <- checkTypedArgs(sig@inputs, typedCall@argValues)
    result <- do.call(fun@f, argValues@args, envir=env)
    stopifnot(isType(sig@output, result))
    result
})

setGeneric('asCheckedClosure', function(typedFunction) {
    standardGeneric('asCheckedClosure')
})

getArgumentListOfCall <- function(arg) {
    stopifnot(is.call(arg))
    arg %>% as.list %>% .[-1]
}

## TODO: make the 'print' of this look prettier -- show the real function body
## somehow, along with type-checking information. use separate class?
## yes! make it say:
## function(a: 'numeric', b: 'character') -> 'character' {
##   # (body of real function goes here)
## }

setMethod('asCheckedClosure', signature(
    typedFunction='TypedFunction'
), function(typedFunction) {
    sig <- typedFunction@signature
    def <- typedFunction@f
    retFun <- function(...) {
        args <- match.call() %>% getArgumentListOfCall()
        pf <- parent.frame()
        argValues <- new('ValueArgumentList', args=args)
        call <- new('TypedCall', fun=typedFunction, argValues=argValues)
        evaluate(call, pf)
    }
    argNames <- sig@inputs@args %>% names
    formals(retFun) <- rep(list(bquote()), length(argNames)) %>%
        setNames(argNames) %>%
        as.pairlist
    retFun
})



### tasks
## setClass('Task', slots=c())
