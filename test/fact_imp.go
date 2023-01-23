package main

import "fmt"

func fact(n int) int {
	k := 1
	for ; n > 1; n-- {
		k = k * n
	}
	return k
}

func main() {
	for n := 0; n <= 10; n++ {
		fmt.Print(fact(n))
		fmt.Print("\n")
	}
}
