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



### error types
setClass('Exception')


validateTypeNameString <- function(s) {
    if (!(is.character(s) &&
          (length(s) == 1) &&
          isTRUE(s != ''))) {
        stop(sprintf("'%s' is not a valid name string", s))
    }
    s
}

setClass('Type', slots=c(name='character'))
setMethod('initialize', 'Type', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@name <- validateTypeNameString(.Object@name)
    .Object
})

setGeneric('isType', function(type, object) {
    standardGeneric('isType')
}, valueClass='logical')

setMethod('isType', signature(
    type='Type',
    object='ANY'
), function(type, object) {
    is(object, type@name)
})

checkType <- function (type, object) {
    if (!isType(type, object)) {
        stop(sprintf("type: '%s' check failed for result: '%s'",
                     type@name, object))
    }
    object
}

setGeneric('extendsType', function(super, sub) {
    standardGeneric('extendsType')
}, valueClass='logical')

setMethod('extendsType', signature(
    super='Type',
    sub='Type'
), function(super, sub) {
    extends(sub@name, super@name)
})

setClass('TypedPromise', slots=c(type='Type'))

## gets the type of the argument with `class`
setClass('TypedLiteral', contains='TypedPromise', slots=c(value='ANY'))
setMethod('initialize', 'TypedLiteral', function(.Object, value, ...) {
    objectType <- new('Type', name=class(value))
    .Object <- callNextMethod(.Object, type=objectType, ...)
    .Object@value <- value
    .Object
})

setClass('TypedOption', contains='TypedPromise', slots=c(
    hasValue='logical',
    value='ANY'
))
setMethod('initialize', 'TypedOption', function(.Object, hasValue, value, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (!hasValue && !is.null(value)) {
        stop(sprintf(paste(
            "a TypedOption with hasValue = FALSE must have value = NULL,",
            "but its value was '%s'"),
            value))
    }
    .Object@hasValue <- hasValue
    .Object@value <- value
    .Object
})

setGeneric('makeOption', function(value, type) {
    standardGeneric('makeOption')
}, valueClass='TypedOption')

setMethod('makeOption', signature(
    value='ANY',
    type='missing'
), function(value) {
    value %>%
        class %>%
        new('Type', name=.) %>%
        makeOption(value, type=.)
})

setMethod('makeOption', signature(
    value='ANY',
    type='Type'
), function(value, type) {
    if (is.null(value)) {
        new('TypedOption', type=type, hasValue=FALSE, value=NULL)
    } else {
        value <- checkType(type, value)
        new('TypedOption', type=type, hasValue=TRUE, value=value)
    }
})

setClass('NameSet', slots=c(names='character'))
setMethod('initialize', 'NameSet', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    nm <- .Object@names
    if (length(nm) <= 0) {
        stop("a NameSet should have at least one name")
    }
    if (any(nm == '') ||
        anyDuplicated(nm)) {
        stop(sprintf("names in a NameSet should be nonempty and unique: %s",
                     formatCharVector(nm)))
    }
    .Object
})

stopCollectingStrings <- function(fmt, strs, ..., collapse=", ") {
    if (is.null(strs)) {
        NULL
    } else if (!is.character(strs)) {
        stop(sprintf("the argument 'strs' should be a character vector: '%s'",
                     strs))
    } else if (length(strs) > 0) {
        pasted <- paste0(strs, collapse=collapse)
        stop(sprintf(fmt=fmt, pasted, ...))
    } else {
        strs
    }
}

setClass('Environment', slots=c(
    nameSet='NameSet',
    innerEnv='environment'))
setMethod('initialize', 'Environment', function(.Object, nameSet, innerEnv, ...) {
    .Object <- callNextMethod(.Object, ...)
    envNames <- names(innerEnv)
    remainingNames <- setdiff(nameSet@names, envNames)
    stopCollectingStrings(
        fmt=paste(
            "the names in the NameSet are not all in the environment.",
            "missing: [%s]"),
        strs=remainingNames)
    .Object@nameSet <- nameSet
    .Object@innerEnv <- innerEnv
    .Object
})

setClass('NamedValue', slots=c(
    name='character',
    value='ANY'
))

formatCharVector <- function(strs, fmt="'%s'", collapse=", ", surround="[%s]") {
    if (is.null(strs)) {
        'NULL'
    } else {
        if (is.list(strs)) {
            strs <- unlist(strs)
        }
        strs %>%
            sprintf(fmt=fmt, .) %>%
            paste0(collapse=collapse) %>%
            sprintf(fmt=surround, .)
    }
}

## check if list, not empty, all named, no repeated names
validateUniquelyNamedList <- function(x) {
    if (!is.list(x) ||
        length(x) <= 0) {
        stop(sprintf("argument should be a non-empty list: '%s'", x))
    }
    listNames <- names(x)
    if (is.null(listNames)) {
        stop("list names cannot be NULL")
    }
    if (any(listNames == '') ||
        anyDuplicated(listNames)) {
        stop(sprintf(
            paste("list must have unique, nonempty names for each element.",
                  "the names were: %s"),
            formatCharVector(listNames)))
    }
    x
}

setClass('UniquelyNamedList', slots=c(args='list'))
setMethod('initialize', 'UniquelyNamedList', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@args <- validateUniquelyNamedList(.Object@args)
    .Object
})

setAs(from='UniquelyNamedList', to='Environment', function(from) {
    listForm <- from@args
    listEnv <- listForm %>% as.environment
    listForm %>%
        names %>%
        new('NameSet', names=.) %>%
        new('Environment', nameSet=., innerEnv=listEnv)
})

setGeneric('getEnv', function(fromEnv, name) {
    standardGeneric('getEnv')
}, valueClass='NamedValue')

setMethod('getEnv', signature(
    fromEnv='Environment',
    name='character'
), function(fromEnv, name) {
    fromEnv@innerEnv %>%
        get(name, envir=.) %>%
        new('NamedValue', name=name, value=.)
})

setGeneric('sameNames', function(lhs, rhs) {
    standardGeneric('sameNames')
}, valueClass='logical')

setMethod('sameNames', signature(
    lhs='NameSet',
    rhs='NameSet'
), function(lhs, rhs) {
    setequal(lhs@names, rhs@names)
})

setGeneric('namedIterate', function(env, visitor) {
    standardGeneric('namedIterate')
}, valueClass='list')

setMethod('namedIterate', signature(
    env='Environment',
    visitor='function'
), function(env, visitor) {
    envNames <- env %>% .@nameSet %>% .@names
    namedPairs <- lapply(envNames, function(name) {
        getEnv(env, name) %>%
            .@value %>%
            new('NamedValue', name=name, value=.)
    })
    namedPairs %>% lapply(visitor)
})

unlistFilter <- function(listArg, pred=is.null) {
    listArg %>%
        Filter(x=., f=Negate(pred)) %>%
        unlist
}

setClass('TypeRequirements', contains='Environment')
setMethod('initialize', 'TypeRequirements', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    nonTypeEntryNames <- .Object %>% namedIterate(function(pair) {
        if (is(pair@value, 'Type')) {
            NULL
        } else {
            pair@name
        }
    }) %>% unlistFilter
    stopCollectingStrings(
        fmt=paste(
            "entries in TypeRequirements should all be instances of Type: ",
            "invalid entries [%s]"),
        strs=nonTypeEntryNames)
    .Object
})

setClass('TypedEnvironment', contains='Environment')
setMethod('initialize', 'TypedEnvironment', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    untypedEntryNames <- .Object %>% namedIterate(function(pair) {
        if (is(pair@value, 'TypedPromise')) {
            NULL
        } else {
            pair@name
        }
    }) %>% unlistFilter
    stopCollectingStrings(
        fmt=paste(
            "entries in TypedEnvironment should all be",
            "(instances of) subclasses of TypedPromise: ",
            "invalid entries [%s]"),
        strs=untypedEntryNames)
    .Object
})

setGeneric('checkTypedArgs', function(argTypes, argPromises) {
    standardGeneric('checkTypedArgs')
}, valueClass='TypedEnvironment')

## NOTE: checks types in order of declared types, not invoked values
setMethod('checkTypedArgs', signature(
    argTypes='TypeRequirements',
    argPromises='TypedEnvironment'
), function(argTypes, argPromises) {
    declaredNames <- argTypes@nameSet
    givenNames <- argPromises@nameSet
    if (!sameNames(declaredNames, givenNames)) {
        stop(sprintf(paste("declared argument names: %s",
                           "differ from invocation argument names: %s"),
                     formatCharVector(declaredNames@names),
                     formatCharVector(givenNames@names)))
    }
    wrongTypedEntryNames <- declaredNames@names %>% lapply(function(name) {
        declaredType <- argTypes %>% getEnv(name) %>% .@value
        promiseType <- argPromises %>% getEnv(name) %>% .@value %>% .@type
        if (extendsType(super=declaredType, sub=promiseType)) {
            NULL
        } else {
            sprintf(paste("argument '%s' has type '%s',",
                          "which does not extend the declared type '%s'"),
                    name, promiseType@name, declaredType@name)
        }
    }) %>% unlistFilter
    stopCollectingStrings(fmt="errors in type checking arguments:\n%s",
                          strs=wrongTypedEntryNames,
                          collapse="\n")
    argPromises
})

setClass('FunctionBody', slots=c(bracedBody='{'))

setClass('PureSignature', slots=c(
    inputs='TypeRequirements',
    output='Type'
))

setGeneric('makePureSignature', function(lhs, rhs) {
    standardGeneric('makePureSignature')
}, valueClass='PureSignature')

setMethod('makePureSignature', signature(
    lhs='TypeRequirements',
    rhs='Type'
), function(lhs, rhs) {
    new('PureSignature', inputs=lhs, output=rhs)
})

setMethod('makePureSignature', signature(
    lhs='TypeRequirements',
    rhs='character'
), function(lhs, rhs) {
    new('PureSignature', inputs=lhs, output=new('Type', name=rhs))
})

## setClass('EffectSignature', contains='PureSignature', slots=c())

## TODO: have some way to wrap errors in the TypedFunction class (i.e. specify
## recognized errors for use in a tryCatch)
setClass('TypedFunction', slots=c(
    signature='PureSignature',
    f='function'
))

setGeneric('makeTypedFunction', function(signature, body) {
    standardGeneric('makeTypedFunction')
}, valueClass='TypedFunction')

setMethod('makeTypedFunction', signature(
    signature='PureSignature',
    body='FunctionBody'
), function(signature, body) {
    inputs <- signature@inputs
    new('TypedFunction',
        signature=signature,
        f=pryr::make_function(args=toEmptyAlist(inputs),
                              body=body@bracedBody))
})

setMethod('makeTypedFunction', signature(
    signature='PureSignature',
    body='function'
), function(signature, body) {
    new('TypedFunction', signature=signature, body=body)
})

setMethod('makeTypedFunction', signature(
    signature='PureSignature',
    body='{'
), function(signature, body) {
    makeTypedFunction(signature, new('FunctionBody', bracedBody=body))
})

setClass('TypedCall', contains='TypedPromise', slots=c(
    fun='TypedFunction',
    argValues='TypedEnvironment'
))
setMethod('initialize', 'TypedCall', function(.Object, fun, argValues, ...) {
    sig <- fun@signature
    resultType <- sig@output
    .Object <- callNextMethod(.Object, type=resultType, ...)
    .Object@argValues <- sig@inputs %>%
        checkTypedArgs(argTypes=., argPromises=argValues)
    .Object@fun <- fun
    .Object
})

setGeneric('evaluate', function(typedCall, inEnv) {
    standardGeneric('evaluate')
})

setMethod('evaluate', signature(
    typedCall='TypedCall',
    inEnv='missing'
), function(typedCall) {
    evaluate(typedCall, environment())
})

setMethod('evaluate', signature(
    typedCall='TypedCall',
    inEnv='environment'
), function(typedCall, inEnv) {
    fun <- typedCall@fun
    sig <- fun@signature
    argsNamedList <- typedCall@argValues %>%
        .@innerEnv %>%
        as.list %>%
        lapply(function(x) {
            stopifnot(is(x, 'TypedLiteral'))
            x@value
        })
    result <- do.call(fun@f, argsNamedList, envir=inEnv)
    output <- sig@output
    checkType(sig@output, result)
})

makeEmptyAlistFromNames <- function(names) {
    if (!is.character(names) ||
        length(names) <= 0) {
        stop(sprintf("argument should be a non-empty character vector: '%s'",
                     names))
    }
    rep(list(bquote()), length(names)) %>%
        setNames(names) %>%
        as.pairlist
}

setGeneric('toEmptyAlist', function(x) {
    standardGeneric('toEmptyAlist')
}, valueClass='pairlist')

setMethod('toEmptyAlist', signature(
    x='character'
), function(x) {
    makeEmptyAlistFromNames(x)
})

setMethod('toEmptyAlist', signature(
    x='Environment'
), function(x) {
    x@nameSet@names %>% toEmptyAlist
})

getArgumentListOfCall <- function(arg) {
    if (!is.call(arg)) {
        stop(sprintf("argument: '%s' must be a call", arg))
    }
    arg %>% as.list %>% .[-1]
}

setGeneric('asCheckedClosure', function(typedFunction) {
    standardGeneric('asCheckedClosure')
}, valueClass='function')

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
    inputs <- sig@inputs
    retFun <- function(...) {
        uniqNameArgs <- match.call() %>%
            getArgumentListOfCall %>%
            lapply(function(x) { new('TypedLiteral', value=x) }) %>%
            new('UniquelyNamedList', args=.)
        argValues <- uniqNameArgs@args %>%
            names %>%
            new('NameSet', names=.) %>%
            new('TypedEnvironment',
                nameSet=., innerEnv=as(uniqNameArgs, 'environment'))
        call <- new('TypedCall', fun=typedFunction, argValues=argValues)
        evaluate(call, environment())
    }
    formals(retFun) <- toEmptyAlist(inputs)
    retFun
})



### wrappers, operators, and macros
type <- function(name) {
    new('Type', name=name)
}

lit <- function(x) {
    new('TypedLiteral', value=x)
}

reqs <- function(...) {
    argList <- list(...)
    uniqNameList <- new('UniquelyNamedList', args=argList)
    argsEnv <- as(uniqNameList, 'Environment')
    new('TypeRequirements', nameSet=argsEnv@nameSet, innerEnv=argsEnv@innerEnv)
}

params <- function(...) {
    list(...) %>%
        lapply(function(x) { new('Type', name=x) }) %>%
        do.call(reqs, args=.)
}

link <- function(...) {
    argList <- list(...)
    uniqNameList <- new('UniquelyNamedList', args=argList)
    argsEnv <- as(uniqNameList, 'Environment')
    new('TypedEnvironment', nameSet=argsEnv@nameSet, innerEnv=argsEnv@innerEnv)
}

literals <- function(...) {
    list(...) %>%
        lapply(function(x) { new('TypedLiteral', value=x) }) %>%
        do.call(link, args=.)
}

body <- function(bracedExpr) {
    substitute(bracedExpr)
}

`%->%` <- function(lhs, rhs) {
    makePureSignature(lhs, rhs)
}

`%:%` <- function(lhs, rhs) {
    rhsSub <- substitute(rhs)
    ## allow bare curly braces to be interpreted as function bodies
    if (is(rhsSub, '{')) {
        makeTypedFunction(lhs, rhsSub)
    } else {
        makeTypedFunction(lhs, rhs)
    }
}

`%=>%` <- function(lhs, rhs) {
    new('TypedCall', fun=rhs, argValues=lhs)
}


### tasks (/ scheduling?)
## setClass('Task', slots=c())
