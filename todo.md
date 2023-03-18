* [X] optimize
  * [X] tail opt
  * [X] remove types from expr
  * [X] rc all values
  * [X] rc all types?
* [X] change member access
* [X] add methods
* [X] struct
* [X] generic native func
* [X] generic user func
* [X] generic structs
* [X] recursion
* [X] generic native type
* [X] arrays
* [X] bools
* [X] rationals
* [X] strings
* [X] spliced arrays
* [X] sets
* [X] queues
* [X] inner functions
* [X] function types
* [X] return inner function?
* [X] fix closures, make the scope chain RC'd (or ARC'd) and find a smarter way to merge the closures
* [X] optional
* [X] cast (some(1) || error(''))
* [X] operator overloading
* [X] default params
* [X] default params in native functions
* [X] generic specialization (with UD, i think there's a bug)
* [X] less copies
* [X] more tail call optimizations? ([] + ... => ...)
* [X] avoid specializations for unknown types
* [X] limits
  * [X] depth limit 
  * [X] recursion limit
  * [X] size limit
* [X] unions
* [X] recursive unions
* [X] make struct binds use vectors
* [X] lambda functions
* [X] blank structs/unions
* [X] dynamic native functions (`Array<T> == Array<T>`)
* [X] tuples
* [X] tuple as recursive member
* [X] mappings
* [X] double quoted strings
* [X] bundle the root compilation scope and the interner to one struct
* [X] I think we turn static functions to real function all the time instead of once
* [X] root evaluation scope
* [X] delete every function, pub, and derive we can get away with
* [X] replace all vec params with slices
* [X] make it so we don't have to clone declarations
* [X] integrate rtcell into the root scopes
* [X] small ints shouldn't need heap allocation
* [X] make defaults evaluate at runtime
* [X] can `assert` detect when equality happens and report it?
* [X] make turbofish useful
* [ ] more std
  * [X] sets
  * [X] sorting
  * [X] dict delete
  * [X] dict pop
  * [X] hash methods
  * [X] cmp methods
  * [X] aggregators
  * [X] range
  * [X] filter
  * [X] any, all
  * [X] takewhile, skipwhile
  * [X] take, skip
  * [X] multizip
  * [X] unzip
  * [X] enumerate
  * [X] assert
  * [X] array multiplication
  * [X] str concat
  * [X] str multiplication
  * [X] str split
  * [X] str chars
  * [ ] regex?
  * [X] binary search/insort
  * [X] partial
  * [X] nth
  * [X] first/last
  * [X] default equals/hash/cmp/to_string for many types
  * [X] auto mapping for types with eq/hash
  * [X] auto sort
  * [X] infinite sequence
  * [X] slice sequence type
  * [X] ne for most native types
  * [X] member_wise
  * [X] is_error/if_error with error matching
  * [X] get_error
  * [X] dynamic "display"
  * [X] add str to something that implements to_str?
  * [X] `is_close`
  * [X] bool xor
  * [X] float pow
  * [X] sqrt
  * [X] nth root
  * [X] nth last
  * [X] aggregators
  * [X] reduce should be in generators
  * [X] everything in excel
  * [X] atan2
  * [X] geometric distribution
  * [ ] some things in python
    * [ ] got to type  https://docs.python.org/3/library/functions.html#type
  * [X] sequence::manifest
  * [ ] json?
  * [X] chain sequence?
  * [ ] set xor
  * [ ] xors
  * [ ] sleep
  * [ ] chunks
  * [ ] random
  * [ ] is_infinite
  * [ ] fold?
  * [ ] str len
  * [ ] array to generator
  * [ ] generator to array
  * [ ] filter should return a generator
  * [ ] generator takewhile/skipuntil
  * [ ] sequence reverse
  * [ ] mean/std for distributions
  * [ ] sort reverse (dyn)
  * [ ] matrices
  * [ ] time of day?
  * [ ] list shuffle
  * [ ] numerically safe addition
  * [ ] byte types?
* [ ] tests for:
  * [X] is_error
  * [X] if error (inc. tail recursion)
  * [X] display
  * [X] bool display
  * [X] display from UD type
  * [X] bool hash
  * [X] bool cmp
  * [X] float mod
  * [X] float div
  * [X] float ceil
  * [X] float trunc
  * [X] float cmp
  * [X] float to str
  * [X] int bitwise ops
  * [X] int pow
  * [X] int ne
  * [X] int hash
  * [X] int cmp
  * [X] sequence rpush
  * [X] sequence insert
  * [X] sequence set
  * [X] sequence swap
  * [X] str hash
  * [X] str cmp
  * [X] function call in default (inner function call in default?)
  * [X] internal structs
  * [X] test all kinds of failures
    * [X] including runtime violations
  * [X] get operator
  * [X] mapping get
  * [X] hexa/bin numbers
  * [X] scientific notation
  * [X] float neg
  * [X] is_close
  * [X] float sqrt
  * [X] float pow
  * [X] mapping entries
  * [ ] all the integer edge-cases
  * [ ] zip1
  * [ ] unzip1
  * [X] repeat (with n and infinite)
  * [ ] lambda with defaults
  * [ ] str find
  * [ ] str split edge cases
  * [ ] members for recursive types
  * [ ] errors:
    * [ ] VariableAsType
    * [ ] OverloadAsType
  * [ ] sin
  * [ ] turbofish with zero params
  * [ ] dyn with with zero params
  * [ ] sum/product/mean/geomean? with complex types
* [X] reduce macros in builtin/core
* [ ] better errors
  * [X] compilation error tracing
  * [X] dynamic binding error tracing
  * [ ] runtime error tracing
* [X] get operator
* [X] auto type for specialization (`foo::<., Stack<int>>(1, stack())`)
* [X] cells instead of dicts for namespaces
* [ ] All the TODOs
* [X] all the warnings
* [X] test scripts
* [X] imports/include files
* [X] turn this to a library
* [ ] documentation
* [X] `!` operator (as shorthand for .value)?
* [ ] code documentation
* [X] newline/escape chars in literals
* [X] straighten out the "errors in container/compound" question
* [ ] find and remove duplicates
* [X] string interpolation?
* [X] auto return type (`fn foo () -> $`)
* [X] in native functions, when you're going to return something big (like when adding two sequences), check that you have room for it first
* [X] type aliases?
* [X] total function call limit
* [X] maximum search limit (`count().nth(0, (x: int)->{x<0})`)
* [X] execution time limit
* [X] "side effect" limitations (allow/forbid sleep, display, assert)
* [X] declarations in lambdas
* [ ] cache xtypes?
* [ ] create a native type registry, so we don't discern them by name
* [ ] forward references?
* [ ] native dynamic functions?
* [ ] line number in RTCELL?
* [ ] warnings (like when assigning variable to magic method?)
* [ ] make [str].sum use join
* [ ] custom seeding for hash
* [ ] make sure nan inf and negzero are impossible