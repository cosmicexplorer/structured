tasks, scheduling, deps, dags, etc
==================================

# phases of execution
1. task def (deps, parameters w/ default, products)
2. "parameters" given values for each task (invocation)
3. execution
    - scheduled """intelligently"""
        - **GIVE THE PEOPLE CPU/MEM/FS/NETWORK DIALS!!!**
    - decision of where to serialize, where to execute, etc is done **HERE**

## task def
- define tasks "in code"
    - i.e. think of it as a description of the work to be done, separate from any specific values it acts on
- store task registrations in an env (so you can screw with it)

### definition of a task definition
1. name
    - unique in the **task def env**
2. deps (maybe not)
    - e.g. a task to download a file could be written as having no *deps*, but a required *parameter* containing the URLs to download
    - **a task dep is just another task**
        - a task dep is ONLY a task, and CANNOT set any parameters for the task it depends on
            - this helps to reduce magic and increase hermeticity
    - declaring a dep on another task is saying: "I know how to use the result of this task" -- the only thing the *current* task can use is the *product* of the task it depends on
    - **each dep must ALREADY be defined in the task def env**
        - this accidentally helps stop cyclical deps in the execution graph as well *i think?*
3. [parameters](#parameters) (definitely)
    - with defaults (usually)
    - *you could maybe make an operation that has no knobs to tweak, but only for the sake of argument*
4. products (results)
    - e.g. a PCA analysis would produce a list of vectors, maybe with some metadata
    - **this is some R data type**, *nothing more complex*

### types
- declared explicitly
- **NOT defined inline**
- stored in an env
- **types don't have a default value, parameters do**
- **TYPES SHARED FOR PRODUCTS AND PARAMETERS**
    - **the difference between an parameter and a product is that parameter values are known BEFORE the execution phase begins**
- make facility for "container" types?
    - e.g. lists of values?
    - maybe **NOT** -- find out if these could/should differ from streams
        - i think streams might be the answer

have:
1. a predicate (definitely)
    - if predicate fails, **DIE IN INVOCATION PHASE**
2. a "parent" type (maybe)
    - we're not trying to make a type system here
    - all we do is apply the predicate from the parent type, then the predicate for the current type

### parameters
- parameters have explicitly declared [types](#types), and maybe a default
    - **the default is a value, not a closure**
- unlike pants, we don't do nested parameters
    - they're complex and annoying to fingerprint
    - we don't want to encourage nesting tasks inside subsystems
        - prefer to have a single canonical task, and if variations are necessary, make different tasks, or make that variance into parameters
        - also, [products](#products) should always be the same type, NOT dependent on the parameters or dependencies!

have (already described elsewhere, kept here anyway):
1. name (unique within task's set of registered parameters)
2. type
3. default (maybe)

### products
**the difference between an parameter and a product is that parameter values are known BEFORE the execution phase begins**

have:
1. type

## invocation
- this is actually pretty straightforward, since we define parameters and (maybe) default values in [the task definitions](#task-def)

do (not necessarily in order):
1. check if required parameters are provided (error out if not)
2. check if all parameter values have the correct [types](#parameter-types)

## execution
1. schedule
    - concurrent execution depends on cache
2. execute
    - cache
        - serialize to disk / elsewhere **WHEN NECESSARY**
            - not all products are going to be feasible to cache on disk or elsewhere
            - some objects are so big they should be freed after use

### scheduling, caching, and concurrent execution
- scheduling, caching, and concurrent execution are intimately related
- caching is based on fingerprinting task [parameter values](#parameters) and [product values](#products)
- can be as complex as desired
- if done right, can be *extremely* cool

#### concurrent execution
- may require a separate framework for parallel execution *hint, hint*
- **NEED** to be able to support streaming parallel execution in the execution graph
    - can parallelize execution, but missing out on VAST amounts of concurrency if we can't do that same scheduling dynamically with streaming inputs
        - should still be very easy to integrate into caching/etc

# graphs
we need a DAG, obviously. i don't think we should go any deeper.

*we probably need more stuff, or maybe less*

1. [execution](#execution) graph has dynamic edge weights (not dynamic edges, though!)
    - want to able to inspect this graph in motion, to display progress and to schedule streaming concurrent execution
2. want to be able to query and mutate the task dep graph to substitute test tasks, or to override the implementation of some subgraph (e.g. a single task)
    - **this is VERY important -- can't require people to go through some dumb kitchen sink interface**

## graph impl

**lightweight** *(don't solve TSP)* framework to describe a DAG *(meaningless expression)*, query it *(how?)* and mutate it *(do we mutate differently than we query?)*.

prior work:
- `igraph` *(that should be a link)* is confusing and i don't think it was made for anything but graph theoretical computations

# parallel execution framework
something special needs to be done to schedule (and execute, and cache) everything, and that could and probably should be a separate library.

- keep everything (incl. scheduling) local / at the nodes!

questions:
- what do we need for this?
- does we need to write something else? do current offerings exist (in any way)?

progress
========

**TODO: this needs an MVP**

- [ ] define [graphs](#graphs)
    - [ ] start with really simple class in [model.R](model.R)
- [ ] define [types](#types)
- [ ] define [tasks](#tasks) with [parameters](#parameters) as above to form *task dep graph*
- [ ] set and validate parameters as above to form *[invocation](#invocation) graph*
- [ ] schedule execution through a *basic* [parallel execution framework](#parallel-execution-framework)
- [ ] go hard on [scheduling, caching, and concurrent execution](#scheduling-caching-and-concurrent-execution)
