tasks, scheduling, deps, dags, etc
==================================

**STORE EVERYTHING IN AN ENV**

# phases
1. task def (deps, options w/ default, products)
2. "options" given values for each task (invocation)
3. execution
    - scheduled """intelligently"""
    - decision of where to serialize, where to execute, etc is done **HERE**

## task def
- define tasks "in code"
    - i.e. think of it as a description of the work to be done, separate from any specific values it acts on
- store task registrations in an env (so you can screw with it)

### definition of a task definition
1. name
    - unique in the **task def env**
2. deps (maybe not)
    - e.g. a task to download a file could be written as having no *deps*, but a required *option* containing the URLs to download
    - **a task dep is just another task**
        - a task dep is ONLY a task, and CANNOT set any options for the task it depends on
            - this helps to reduce magic and increase hermeticity
    - declaring a dep on another task is saying: "I know how to use the result of this task" -- the only thing the *current* task can use is the *product* of the task it depends on
    - **each dep must ALREADY be defined in the task def env**
        - this accidentally helps stop cyclical deps in the execution graph as well *i think?*
3. [options](#options) (definitely)
    - with defaults (usually)
    - *you could maybe make an operation that has no knobs to tweak, but only for the sake of argument*
4. products (results)
    - e.g. a PCA analysis would produce a list of vectors, maybe with some metadata
    - **this is some R data type**, *nothing more complex*

### types
- declared explicitly
- **NOT defined inline**
- stored in an env
- **types don't have a default value, options do**
- **TYPES SHARED FOR PRODUCTS AND OPTIONS**
    - **the difference between an option and a product is that option values are known BEFORE the execution phase begins**

have:
1. a predicate (definitely)
    - if predicate fails, **DIE IN INVOCATION PHASE**
2. a "parent" type (maybe)
    - we're not trying to make a type system here
    - all we do is apply the predicate from the parent type, then the predicate for the current type

### options
- options have explicitly declared [types](#types), and maybe a default
    - **the default is a value, not a closure**
- unlike pants, we don't do nested options
    - they're complex and annoying to fingerprint
    - we don't want to encourage nesting tasks inside subsystems
        - prefer to have a single canonical task, and if variations are necessary, make different tasks, or make that variance into options
        - also, [products](#products) should always be the same type, NOT dependent on the options or dependencies!

have (already described elsewhere, kept here anyway):
1. name (unique within task's set of registered options)
2. type
3. default (maybe)

### products
**the difference between an option and a product is that option values are known BEFORE the execution phase begins**

have:
1. type

## invocation
- this is actually pretty straightforward, since we define options and (maybe) default values in [the task definitions](#task-def)

do (not necessarily in order):
1. check if required options are provided (error out if not)
2. check if all option values have the correct [types](#option-types)

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
- caching is based on fingerprinting task [option values](#options) and [product values](#products)
- can be as complex as desired
- if done right, can be *extremely* cool

#### concurrent execution
- may require a separate framework for parallel execution *hint, hint*
