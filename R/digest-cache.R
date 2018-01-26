### digest-cache.R -- fingerprint and store values and invocations
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



### fingerprints
FINGERPRINT_LENGTH <- 32L

validateFingerprintString <- function(s) {
    if (is.character(s) &&
        (length(s) == 1) &&
        isTRUE(!is.na(s)) &&
        isTRUE(nchar(s) == FINGERPRINT_LENGTH)) {
        s
    } else {
        stop(sprintf("'%s' is not a valid fingerprint value", s))
    }
}

setClass('Fingerprint', slots=c(fpStr='character'))
setMethod('initialize', 'Fingerprint', function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@fpStr <- validateFingerprintString(.Object@fpStr)
    .Object
})

setGeneric('joinHash', function(lhs, rhs) {
    standardGeneric('joinHash')
}, valueClass='Fingerprint')

setMethod('joinHash', signature(
    lhs='Fingerprint',
    rhs='Fingerprint'
), function(lhs, rhs) {
    c(lhs@fpStr, rhs@fpStr) %>% fingerprint
})

setGeneric('fingerprint', function(x) {
    standardGeneric('fingerprint')
}, valueClass='Fingerprint')

## override for types that are hard to calculate digests for (?)
setMethod('fingerprint', signature(
    x='ANY'
), function(x) {
    new('Fingerprint', fpStr=digest::digest(x, algo='md5'))
})

setMethod('fingerprint', signature(
    x='TypedCall'
), function(x) {
    stop(paste("a TypedCall cannot be fingerprinted,",
               "only an ImmediatelyEvaluableTypedCall can."))
})

setMethod('fingerprint', signature(
    x='ImmediatelyEvaluableTypedCall'
), function(x) {
    new('Fingerprint', fpStr=digest::digest(x, algo='md5'))
})

setClass('FingerprintedObject', slots=c(fp='Fingerprint'))

setClass('FingerprintedLiteral', contains='FingerprintedObject', slots=c(
    typedLit='TypedLiteral'
))
setMethod('initialize', 'FingerprintedLiteral', function(.Object, typedLit, ...) {
    fp <- fingerprint(typedLit)
    .Object <- callNextMethod(.Object, fp=fp, ...)
    .Object@typedLit <- typedLit
    .Object
})

setClass('FingerprintedInvocation', contains='FingerprintedObject', slots=c(
    invocation='ImmediatelyEvaluableTypedCall'
))
setMethod('initialize', 'FingerprintedInvocation', function(.Object, invocation, ...) {
    fp <- fingerprint(invocation)
    .Object <- callNextMethod(.Object, fp=fp, ...)
    .Object@invocation <- invocation
    .Object
})


### content-addressable storage
