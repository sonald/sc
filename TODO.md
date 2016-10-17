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
+ [ ] type checking
+ [ ] type propagation (annotate type for every Ref)
+ [ ] insert necessary cast node (include rvalue casting)
+ [ ] lvalue check
+ [ ] all DeclRefExpr ref to defined variable
+ [ ] reserved keyword are not used as ID
+ [ ] implicit conversion
    this need castnode to be inserted and do type inference for expression.
    what about type of expression for varadic function argument at call-site?
    e.g `printf("%d\n", str[i] + str2[j])`, how to determine type of `str[i]+str2[j]`
