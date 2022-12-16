```go
p = new (int)
*p
```
```x86asm
movq 8, %rax
call malloc
movq %rax, %rbp
```