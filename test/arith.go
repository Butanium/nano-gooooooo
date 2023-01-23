package main

import "fmt"

func main() {
	var a, b = 10, 3
	var sum = a + b
	var diff = a - b
	var prod = a * b
	var quot = a / b
	var rem = a % b
	fmt.Print("sum:", sum, "expected", 13)
	fmt.Print("diff:", diff, "expected", 7)
	fmt.Print("prod:", prod, "expected", 30)
	fmt.Print("quot:", quot, "expected", 3)
	fmt.Print("rem:", rem, "expected", 1)
}
