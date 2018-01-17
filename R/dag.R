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
#' @import pryr



### parameters and products (ins and outs and ins)
### these use the SAME TYPES!!!!
## ???


isValidTypeName <- function(x) {
    is.character(x) &&
        (length(x) == 1) &&
        isTRUE(x != '')
}

setClass('TypeName', slots=c(name='character'))
setMethod('initialize', 'TypeName', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!isValidTypeName(.Object@name)) {
        stop(sprintf("invalid type name: '%s'", .Object@name))
    }
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
    if (!is.list(x) ||
        length(x) <= 0) {
        stop(sprintf("argument should be a non-empty list: '%s'", x))
    }
    listNames <- names(x)
    if (is.null(listNames) ||
        any(listNames == '') ||
        anyDuplicated(listNames)) {
        stop(sprintf(
            "list must have unique, nonempty names for each element: '%s'",
            x))
    }
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

makeNamedNoValueAlist <- function(names) {
    if (!is.character(names) ||
        length(names) <= 0) {
        stop(sprintf("argument should be a non-empty character vector: '%s'",
                     names))
    }
    rep(list(bquote()), length(names)) %>%
        setNames(names) %>%
        as.pairlist()
}

setGeneric('toAlist', function(x) {
    standardGeneric('toAlist')
})

setMethod('toAlist', signature(
    x='TypedArgumentList'
), function (x) {
    x@args %>% names %>% makeNamedNoValueAlist
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
    if (!setequal(declaredNames, givenNames)) {
        stop(sprintf(
            "declared argument: '%s' differ from invocation arguments: '%s'",
            declaredNames, givenNames))
    }
    for (name in declaredNames) {
        type <- argTypes@args[[name]]
        value <- argValues@args[[name]]
        if (!isType(type, value)) {
            stop(sprintf("type: '%s' check failed for argument: '%s'",
                         type@name, value))
        }
    }
    argValues
})

setClass('FunctionBody', slots=c(bracedBody='{'))

setClass('Signature', slots=c(
    inputs='TypedArgumentList',
    output='TypeName'
))

setClass('TypedFunction', slots=c(
    signature='Signature',
    f='function'
))

setGeneric('makeTypedFunction', function(signature, body) {
    standardGeneric('makeTypedFunction')
})

setMethod('makeTypedFunction', signature(
    signature='Signature',
    body='FunctionBody'
), function(signature, body) {
    inputs <- signature@inputs
    argNames <- names(inputs@args)
    outType <- signature@output
    new('TypedFunction',
        signature=signature,
        f=pryr::make_function(args=toAlist(inputs),
                              body=body@bracedBody))
})

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
    output <- sig@output
    if (!isType(output, result)) {
        stop(sprintf("type: '%s' check failed for result: '%s'",
                     output@name, result))
    }
    result
})

getArgumentListOfCall <- function(arg) {
    if (!is.call(arg)) {
        stop(sprintf("argument: '%s' must be a call", arg))
    }
    arg %>% as.list %>% .[-1]
}

setGeneric('asCheckedClosure', function(typedFunction) {
    standardGeneric('asCheckedClosure')
})

## TODO: make the 'print' of this look prettier -- show the real function body
## somehow, along with type-checking information. use separate class?
## yes! make it say:
## function(a: 'numeric', b: 'character') -> 'character' {
##   # (body of real function goes here)
## }
## also make sure to expand any <S4 object of class "TypedFunction"> etc

setMethod('asCheckedClosure', signature(
    typedFunction='TypedFunction'
), function(typedFunction) {
    sig <- typedFunction@signature
    body <- body(typedFunction@f)
    inputs <- sig@inputs
    argNames <- names(inputs@args)
    retFun <- function(...) {
        args <- getArgumentListOfCall(match.call())
        argValues <- new('ValueArgumentList', args=args)
        call <- new('TypedCall', fun=typedFunction, argValues=argValues)
        evaluate(call, environment())
    }
    formals(retFun) <- toAlist(inputs)
    retFun
})





### wrappers, operators, and macros
params <- function(...) {
    new('TypedArgumentList', args=list(...))
}

type <- function(name) {
    new('TypeName', name=name)
}

setGeneric('%->%', function(lhs, rhs) {
    standardGeneric('%->%')
})

setMethod('%->%', signature(
    lhs='TypedArgumentList',
    rhs='TypeName'
), function(lhs, rhs) {
    new('Signature', inputs=lhs, output=rhs)
})

setMethod('%->%', signature(
    lhs='TypedArgumentList',
    rhs='character'
), function(lhs, rhs) {
    new('Signature', inputs=lhs, output=new('TypeName', name=rhs))
})

setGeneric('%:%', function(lhs, rhs) {
    standardGeneric('%:%')
})

setMethod('%:%', signature(
    lhs='Signature',
    rhs='{'
), function(lhs, rhs) {
    makeTypedFunction(lhs, new('FunctionBody', bracedBody=rhs))
})

setMethod('%:%', signature(
    lhs='Signature',
    rhs='function'
), function(lhs, rhs) {
    new('TypedFunction', signature=lhs, f=rhs)
})


### tasks
## setClass('Task', slots=c())
