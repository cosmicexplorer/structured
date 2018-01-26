### defs.R -- create a dynamic representation of typechecked execution
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



### types and values

validateTypeNameString <- function(s) {
    if (is.character(s) &&
        (length(s) == 1) &&
        isTRUE(s != '') &&
        isTRUE(!is.na(s))) {
        s
    } else {
        stop(sprintf("'%s' is not a valid name string", s))
    }
}

#' @export
setClass('Type', slots=c(name='character'))
setMethod('initialize', 'Type', function(.Object, name, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@name <- validateTypeNameString(name)
    .Object
})

#' @export
setGeneric('isType', function(type, object) {
    standardGeneric('isType')
}, valueClass='logical')

#' @export
setMethod('isType', signature(
    type='Type',
    object='ANY'
), function(type, object) {
    is(object, type@name)
})

#' @export
checkType <- function(type, object) {
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

#' @export
checkExtendsType <- function(super, sub) {
    if (!extendsType(super, sub)) {
        stop(sprintf("subtype: '%s' does not extend supertype: '%s'",
                     sub@name, super@name))
    }
    sub
}

#' @export
setClass('TypedPromise', slots=c(type='Type'))

#' @export
setClass('TypedLiteral', contains='TypedPromise', slots=c(value='ANY'))
setMethod('initialize', 'TypedLiteral', function(.Object, value, ...) {
    ## gets the type of the argument with `class`
    objectType <- new('Type', name=class(value))
    .Object <- callNextMethod(.Object, type=objectType, ...)
    .Object@value <- value
    .Object
})



### environments

validateArgNames <- function(nm) {
    if (any(nm == '') ||
        any(is.na(nm)) ||
        anyDuplicated(nm)) {
        stop(sprintf(paste("argument names should be nonempty and non-NA.",
                           "invalid argument names: %s"),
                     formatCharVector(nm)))
    }
    nm
}

#' @export
setClass('NameSet', slots=c(names='character'))
setMethod('initialize', 'NameSet', function(.Object, names, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (length(names) <= 0) {
        stop("a NameSet should have at least one name")
    }
    .Object@names <- validateArgNames(names)
    .Object
})

stopCollectingStrings <- function(outerFmt, strs, ...) {
    if (is.null(strs)) {
        NULL
    } else if (!is.character(strs)) {
        stop(sprintf("the argument 'strs' should be a character vector: '%s'",
                     strs))
    } else if (length(strs) > 0) {
        pasted <- formatCharVector(strs, ...)
        stop(sprintf(fmt=outerFmt, pasted))
    } else {
        strs
    }
}

#' @export
setClass('OrderedBindings', slots=c(
    nameSet='NameSet',
    innerEnv='environment'))
setMethod('initialize', 'OrderedBindings', function(.Object, nameSet, innerEnv, ...) {
    envNames <- names(innerEnv)
    remainingNames <- setdiff(nameSet@names, envNames)
    stopCollectingStrings(
        outerFmt=paste("some names in the NameSet are not in the environment.",
                       "missing: %s"),
        strs=remainingNames)
    .Object <- callNextMethod(.Object, ...)
    .Object@nameSet <- nameSet
    .Object@innerEnv <- innerEnv
    .Object
})

#' @export
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

setAs(from='UniquelyNamedList', to='OrderedBindings', function(from) {
    listForm <- from@args
    listEnv <- listForm %>% as.environment
    listForm %>%
        names %>%
        new('NameSet', names=.) %>%
        new('OrderedBindings', nameSet=., innerEnv=listEnv)
})

setAs(from='OrderedBindings', to='UniquelyNamedList', function(from) {
    orderedNames <- from %>% .@nameSet %>% .@names
    lapply(orderedNames, function(name) {
        from %>% getEnv(name) %>% .@value
    }) %>%
        setNames(nm=orderedNames) %>%
        new('UniquelyNamedList', args=.)
})

setGeneric('getEnv', function(fromEnv, name) {
    standardGeneric('getEnv')
}, valueClass='NamedValue')

setMethod('getEnv', signature(
    fromEnv='OrderedBindings',
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
    env='OrderedBindings',
    visitor='function'
), function(env, visitor) {
    envNames <- env %>% .@nameSet %>% .@names
    namedPairs <- lapply(envNames, function(name) {
        getEnv(env, name)
    })
    namedPairs %>% lapply(visitor)
})



### type requirements, and fulfilling those requirements

unlistFilter <- function(listArg, pred=is.null) {
    listArg %>%
        Filter(x=., f=Negate(pred)) %>%
        unlist
}

#' @export
setClass('TypeRequirements', contains='OrderedBindings')
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
        outerFmt=paste(
            "entries in TypeRequirements should all be",
            "instances of the Type class. invalid entries: %s"),
        strs=nonTypeEntryNames)
    .Object
})

#' @export
setClass('TypedInputs', contains='OrderedBindings')
setMethod('initialize', 'TypedInputs', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    untypedEntryNames <- .Object %>% namedIterate(function(pair) {
        if (is(pair@value, 'TypedPromise')) {
            NULL
        } else {
            pair@name
        }
    }) %>% unlistFilter
    stopCollectingStrings(
        outerFmt=paste("entries in TypedEnvironment should all be",
                       "(instances of) subclasses of TypedPromise.",
                       "invalid entries: %s"),
        strs=untypedEntryNames)
    .Object
})

setGeneric('getTypeOfArg', function(env, argName) {
    standardGeneric('getTypeOfArg')
}, valueClass='Type')

setMethod('getTypeOfArg', signature(
    env='TypeRequirements',
    argName='character'
), function(env, argName) {
    getEnv(env, argName)@value
})

setMethod('getTypeOfArg', signature(
    env='TypedInputs',
    argName='character'
), function(env, argName) {
    getEnv(env, argName)@value@type
})

#' @export
setClass('TypedImmediates', contains='TypedInputs')
setMethod('initialize', 'TypedImmediates', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    nonLiteralEntryNames <- .Object %>% namedIterate(function(pair) {
        if (is(pair@value, 'TypedLiteral')) {
            NULL
        } else {
            pair@name
        }
    }) %>% unlistFilter
    stopCollectingStrings(
        outerFmt=paste("entries in TypedImmediates should all be",
                       "(instances of) subclasses of TypedLiteral.",
                       "invalid entries: %s"),
        strs=nonLiteralEntryNames)
    .Object
})

setGeneric('checkTypedArgs', function(argTypes, argPromises) {
    standardGeneric('checkTypedArgs')
}, valueClass='TypedInputs')

## NOTE: checks types in order of declared types, not invoked values
setMethod('checkTypedArgs', signature(
    argTypes='TypeRequirements',
    argPromises='TypedInputs'
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
    stopCollectingStrings(outerFmt="errors in type checking arguments: %s",
                          strs=wrongTypedEntryNames,
                          fmt="- %s",
                          surround="[\n%s\n]",
                          collapse="\n")
    argPromises
})

#' @export
setClass('FunctionSignature', slots=c(
    inputs='TypeRequirements',
    output='Type'
))

setGeneric('makeSignature', function(lhs, rhs) {
    standardGeneric('makeSignature')
}, valueClass='FunctionSignature')

setMethod('makeSignature', signature(
    lhs='TypeRequirements',
    rhs='Type'
), function(lhs, rhs) {
    new('FunctionSignature', inputs=lhs, output=rhs)
})

setMethod('makeSignature', signature(
    lhs='TypeRequirements',
    rhs='character'
), function(lhs, rhs) {
    forType <- new('Type', name=rhs)
    makeSignature(lhs, forType)
})



### execute code with checked types

#' @export
setClass('TypedFunction', slots=c(
    signature='FunctionSignature',
    f='function'
))

setClass('FunctionBody', slots=c(bracedBody='{'))

setGeneric('makeTypedFunction', function(signature, body) {
    standardGeneric('makeTypedFunction')
}, valueClass='TypedFunction')

setMethod('makeTypedFunction', signature(
    signature='FunctionSignature',
    body='FunctionBody'
), function(signature, body) {
    inputs <- signature@inputs
    new('TypedFunction',
        signature=signature,
        f=pryr::make_function(args=toEmptyAlist(inputs),
                              body=body@bracedBody))
})

setMethod('makeTypedFunction', signature(
    signature='FunctionSignature',
    body='function'
), function(signature, body) {
    new('TypedFunction', signature=signature, f=body)
})

setMethod('makeTypedFunction', signature(
    signature='FunctionSignature',
    body='{'
), function(signature, body) {
    makeTypedFunction(signature, new('FunctionBody', bracedBody=body))
})

#' @export
setClass('TypedCall', contains='TypedPromise', slots=c(
    fun='TypedFunction',
    argPromises='TypedInputs'
))
setMethod('initialize', 'TypedCall', function(.Object, fun, argPromises, ...) {
    sig <- fun@signature
    resultType <- sig@output
    .Object <- callNextMethod(.Object, type=resultType, ...)
    .Object@argPromises <- sig@inputs %>%
        checkTypedArgs(argTypes=., argPromises=argPromises)
    .Object@fun <- fun
    .Object
})

#' @export
setClass('ImmediatelyEvaluableTypedCall', contains='TypedCall', slots=c(
    immediates='TypedImmediates'
))
setMethod('initialize', 'ImmediatelyEvaluableTypedCall', function(.Object, immediates, ...) {
    .Object <- callNextMethod(.Object, argPromises=immediates, ...)
    .Object@immediates <- immediates
    .Object
})

setGeneric('makeTypedCall', function(lhs, rhs) {
    standardGeneric('makeTypedCall')
}, valueClass='TypedCall')

setMethod('makeTypedCall', signature(
    lhs='TypedInputs',
    rhs='TypedFunction'
), function(lhs, rhs) {
    new('TypedCall', fun=rhs, argPromises=lhs)
})

setMethod('makeTypedCall', signature(
    lhs='TypedImmediates',
    rhs='TypedFunction'
), function(lhs, rhs) {
    new('ImmediatelyEvaluableTypedCall', fun=rhs, immediates=lhs)
})

#' @export
setGeneric('evaluateNow', function(invocation, inEnv) {
    standardGeneric('evaluateNow')
})

#' @export
setMethod('evaluateNow', signature(
    invocation='ImmediatelyEvaluableTypedCall',
    inEnv='environment'
), function(invocation, inEnv) {
    fun <- invocation@fun
    sig <- fun@signature
    argValues <- invocation@immediates
    argsNamedList <- argValues %>%
        as('UniquelyNamedList') %>%
        .@args %>%
        lapply(function(x) { x@value })
    result <- do.call(fun@f, argsNamedList, envir=inEnv)
    output <- sig@output
    checkType(sig@output, result)
})

#' @export
setMethod('evaluateNow', signature(
    invocation='ImmediatelyEvaluableTypedCall',
    inEnv='missing'
), function(invocation) {
    evaluateNow(invocation, environment())
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
    x='OrderedBindings'
), function(x) {
    x@nameSet@names %>% toEmptyAlist
})

getArgumentListOfCall <- function(arg) {
    if (!is.call(arg)) {
        stop(sprintf("argument: '%s' must be a call", arg))
    }
    arg %>% as.list %>% .[-1]
}

#' @export
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

#' @export
setMethod('asCheckedClosure', signature(
    typedFunction='TypedFunction'
), function(typedFunction) {
    sig <- typedFunction@signature
    inputs <- sig@inputs
    retFun <- function(...) {
        argValues <- match.call() %>%
            getArgumentListOfCall %>%
            lapply(function(x) { new('TypedLiteral', value=x) }) %>%
            new('UniquelyNamedList', args=.) %>%
            as('OrderedBindings') %>% {
                new('TypedImmediates', nameSet=.@nameSet, innerEnv=.@innerEnv)
            }
        call <- new('ImmediatelyEvaluableTypedCall',
                    fun=typedFunction, immediates=argValues)
        evaluateNow(call)
    }
    formals(retFun) <- toEmptyAlist(inputs)
    retFun
})



### wrappers, operators, and macros

#' @export
type <- function(name) {
    new('Type', name=name)
}

#' @export
lit <- function(x) {
    new('TypedLiteral', value=x)
}

#' @export
reqs <- function(...) {
    list(...) %>%
        new('UniquelyNamedList', args=.) %>%
        as('OrderedBindings') %>% {
            new('TypeRequirements', nameSet=.@nameSet, innerEnv=.@innerEnv)
        }
}

#' @export
params <- function(...) {
    list(...) %>%
        lapply(function(x) { new('Type', name=x) }) %>%
        do.call(reqs, args=.)
}

#' @export
inputs <- function(...) {
    list(...) %>%
        new('UniquelyNamedList', args=.) %>%
        as('OrderedBindings') %>% {
            new('TypedInputs', nameSet=.@nameSet, innerEnv=.@innerEnv)
        }
}

#' @export
literals <- function(...) {
    list(...) %>%
        lapply(function(x) { new('TypedLiteral', value=x) }) %>%
        new('UniquelyNamedList', args=.) %>%
        as('OrderedBindings') %>% {
            new('TypedImmediates', nameSet=.@nameSet, innerEnv=.@innerEnv)
        }
}

#' @export
body <- function(bracedExpr) {
    substitute(bracedExpr)
}

#' @export
`%->%` <- function(lhs, rhs) {
    makeSignature(lhs, rhs)
}

#' @export
`%:%` <- function(lhs, rhs) {
    rhsSub <- substitute(rhs)
    ## allow bare curly braces to be interpreted as function bodies
    if (is(rhsSub, '{')) {
        makeTypedFunction(lhs, rhsSub)
    } else {
        makeTypedFunction(lhs, rhs)
    }
}

#' @export
`%=>%` <- function(lhs, rhs) {
    makeTypedCall(lhs, rhs)
}
