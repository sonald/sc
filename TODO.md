## internal note

### parsing
+ [x] struct/union
+ [x] type class system
+ [ ] parsing more type-names
+ [ ] support enum
+ [ ] make all errors type of Error, instead of string
+ [ ] parsing typedef's
+ [ ] parsing sizeof
+ [x] namespaces (implicitly supported by scopes)
+ [ ] preprocossor

### semantic stage

+ int b[] = {1,2,3}
fill 3 into []
+ type cast
+ insert type conversion
+ struct/union: detect type loops (direct or indirect)
    this should be done in ast traversal stage

