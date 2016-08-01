## internal note

### parsing
+ struct/union
+ type class system
+ parsing more type-names
+ support enum
+ make all errors type of Error, instead of string
+ parsing typedef's

### semantic stage

+ int b[] = {1,2,3}
fill 3 into []
+ type cast
+ insert type conversion
+ struct/union: detect type loops (direct or indirect)
    this should be done in ast traversal stage

