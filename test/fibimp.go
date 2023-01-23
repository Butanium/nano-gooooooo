package main

import "fmt"

func fib(n int) int {
	i, j := 0, 1
	for ; n > 0; n-- {
		i, j = j, i+j
	}
	return i
}

func main() {
	fmt.Print("main\n")
	for i := 0; i <= 10; i++ {
		fmt.Print(fib(i))
		fmt.Print("\n")
	}
}
