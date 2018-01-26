### schedule.R -- perform the work described in an execution graph
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



### wrapper objects
## TODO: make a pointer to the call from each call used as an input to the first
## call. instead of just the call, use a (mutable) reference class (RC) which
## records which inputs have arrived yet and which are still to come -- no need
## for synchronization because we can do this bit all in a single scheduling
## thread.
## TODO: how to actually do scheduling? thread pool? multiprocessing pool?
## something else???

setClass('InputArgument', contains='TypedLiteral', slots=c(
    argName='character'
))
setMethod('initialize', 'InputArgument', function(.Object, argName, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (length(argName) != 1) {
        stop(paste("a single InputArgument should be a character vector",
                   "of size 1. invalid InputArgument: %s"),
             formatCharVector(argName))
    }
    .Object@argName <- validateArgNames(argName)
    .Object
})

setClass('CallWrapper', contains='TypedFunction', slots=c(
    remaining='TypeRequirements'
))
setMethod('initialize', 'CallWrapper', function(.Object, remaining, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@remaining <- validateRemainingArgs(.Object@signature, remaining)
    .Object
})

## TODO: whittle the CallWrapper down to nothing, then when all its args are
## filled, schedule it with its fingerprint and destination!
