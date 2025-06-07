# sc
一个用 Go 编写的C99子集编译器，目前处于演示阶段。

## 功能 (Features)

本项目旨在实现一个C99子集的编译器。目前，其解析器已支持约90%的C99核心语法功能。未来的工作重点将转向语义分析和后端代码生成。

### 已支持特性

*   **基本数据类型**：整数 (integers)，字符 (chars)，指针 (pointers)。
*   **复合数据类型**：结构体 (structs) 和联合体 (unions)。
*   **枚举类型**：支持 `enum` 关键字。
*   **类型定义**：支持 `typedef`。
*   **表达式**：
    *   算术运算 (arithmetic)
    *   逻辑运算 (logical)
    *   位运算 (bitwise)
    *   比较运算 (comparison)
    *   赋值运算 (assignment)
    *   条件运算 (conditional)
    *   `sizeof` 运算符（部分支持，详情请参考 `TODO.md`）
*   **语句**：
    *   条件语句：`if-else`
    *   选择语句：`switch`
    *   循环语句：`while`, `do-while`, `for`
    *   跳转语句：`goto`, `break`, `continue`, `return`
*   **函数**：函数定义与调用。
*   **变量**：变量声明与基本初始化。
*   **作用域与命名空间**：通过作用域隐式支持基本的命名空间管理。

### 开发中及计划支持特性

语义分析和代码生成仍在积极开发中，许多C99的高级特性正在逐步完善。以下是一些正在开发或计划支持的功能（更多详情请参考 `TODO.md`）：

*   **预处理指令**：例如 `#include`（详见 `TODO.md` parsing 部分）。
*   **类型转换**：完整的类型转换机制（详见 `TODO.md` parsing 及 semantic stage 部分）。
*   **高级初始化列表**：针对数组、结构体等的完整初始化器支持（详见 `TODO.md` parsing 部分 "parse initializer designator"）。
*   **全面的类型检查**：（详见 `TODO.md` semantic stage 部分 "type checking"）。
*   **错误报告机制**：更完善的错误报告和注解支持。
*   **调试支持**：增强调试功能。

我们致力于逐步完善编译器对C99标准的支持。

## 编译与运行 (Compilation and Execution)

### 构建编译器

使用以下命令来构建 `sc` 编译器：

```bash
make
```

该命令会在 `bin/` 目录下生成名为 `sc` 的可执行文件。

### 使用编译器

编译器 `sc` 的基本使用方法如下：

```bash
bin/sc [选项] <源文件名.c>
```

例如，编译一个名为 `test.c` 的文件：

```bash
bin/sc test.c
```

默认情况下，编译器会为每个输入的 `.c` 文件生成一个对应的 `.o` 目标文件（例如，`test.c` 会生成 `test.o`）。

#### 命令行选项

编译器支持以下命令行选项：

*   `-verbose`：输出更详细的调试信息。
*   `-dump-tokens`：输出扫描阶段生成的词法单元 (Tokens)。
*   `-dump-ast`：输出解析阶段生成的抽象语法树 (AST)。
*   `-dump-llvm`：输出生成的LLVM中间表示 (LLVM IR)。
*   `-run`：即时编译并执行代码。此选项会使用LLVM的JIT执行引擎运行代码，而不是生成目标文件。

如果未提供输入文件名，编译器将从标准输入 (stdin) 读取源代码。

## 设计 (Design)

`sc` 编译器采用经典的多遍编译（Multi-pass）架构。源代码在被转换为目标代码之前，会依次经过以下几个主要阶段：

1.  **词法分析 (Lexical Analysis)**
    *   **目的**：将原始的C源代码文本分解为一系列的词法单元 (Tokens)，例如关键字、标识符、操作符、字面量等。
    *   **实现**：此阶段由 `lexer/scanner.go` 中的扫描器实现。

2.  **语法分析 (Parsing)**
    *   **目的**：基于词法单元流，构建程序的层级结构，即抽象语法树 (Abstract Syntax Tree, AST)。AST是后续编译阶段的核心数据结构。
    *   **实现**：此阶段由 `parser/parser.go` 中的解析器实现。AST节点的定义位于 `ast/ast.go`。

3.  **语义分析 (Semantic Analysis)**
    *   **目的**：检查AST的语义正确性。这包括类型检查（确保操作符和操作数类型兼容）、作用域分析、变量声明与使用匹配等。
    *   **实现**：此阶段主要由 `sema/typecheck.go` 中的类型检查器等模块实现。

4.  **代码生成 (Code Generation)**
    *   **目的**：遍历经过语义分析的AST，将其转换为目标代码。目前，`sc` 编译器生成LLVM中间表示 (Intermediate Representation, IR)。
    *   **实现**：此阶段由 `codegen/llvm_cg.go` 中的代码生成器实现。

### 核心数据结构：抽象语法树 (AST)

抽象语法树 (AST) 在整个编译流程中扮演着中心角色。它不仅是语法分析的产物，也是语义分析和代码生成阶段的主要输入。编译器通过遍历AST来收集信息、进行检查和转换代码。在 `ast/ast.go` 中定义的 `WalkAst` 函数（或类似的遍历模式）是实现这种遍历和处理的关键机制，它允许在AST的不同节点上应用访问者模式 (Visitor Pattern) 来执行特定操作。
