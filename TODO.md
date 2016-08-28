## internal note

### 
+ [ ] more debugging support
+ [ ] error report (support annotation)
+ [ ] full AST validation in unittest
+ [ ] dump type info in a declaration style (follow-use)

### parsing
+ [x] struct/union
+ [x] type class system
+ [x] parsing more type-names
+ [ ] parse initializer designator 
+ [x] support enum
+ [ ] make all errors type of Error, instead of string
+ [x] parsing typedef's
+ [ ] parsing sizeof
+ [x] namespaces (implicitly supported by scopes)
+ [ ] preprocossor (#includes)
+ [ ] parsing type cast

### semantic stage

+ int b[] = {1,2,3}
fill 3 into []
+ type cast
+ insert type conversion
+ struct/union: detect type loops (direct or indirect)
    this should be done in ast traversal stage
+ type checking
+ type propagation
+ insrt cast node (include rvalue casting)
+ lvalue check
