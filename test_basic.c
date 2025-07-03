// 基础C编译器功能测试文件

int main() {
    // 字符字面量测试
    char c = 'A';
    
    // 复合赋值操作符测试
    int x = 10;
    x += 5;  // 15
    x -= 3;  // 12
    x *= 2;  // 24
    x /= 4;  // 6
    x %= 5;  // 1
    
    // 位运算复合赋值
    int y = 8;
    y &= 12; // 8
    y |= 4;  // 12
    y ^= 3;  // 15
    y <<= 1; // 30
    y >>= 2; // 7
    
    // 三元条件操作符测试
    int a = 5;
    int b = 10;
    int max = (a > b) ? a : b;
    
    // sizeof操作符测试
    int size1 = sizeof(int);
    int size2 = sizeof(a);
    
    // 类型转换测试
    float f = 3.14;
    int i = (int)f;
    
    // 复合字面量测试
    int arr[] = {1, 2, 3, 4, 5};
    
    return 0;
}