package main

import "fmt"

func fact(n int) int {
	if n <= 1 {
		return 1
	}
	return n * fact(n-1)
}

func main() {
	var want, _ = 120, "ignore"
	got := fact(5)
	fmt.Print("got", got, "want", want)
}
