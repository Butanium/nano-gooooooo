package main

import "fmt"

// import "fmt"

func main() {
	// 	var a = 3
	// 	fmt.Print(a, &a)
	// 	a = 5

	// 	fmt.Print(a, &a)
	// 	var b = &a
	// 	fmt.Print("b=&a")
	// 	var x = 3
	// 	*b, x = 4, 8
	// 	fmt.Print(x, "=8", &a, &b)
	// 	for a < 6 {
	// 		a++
	// 		fmt.Print(a)
	// 	}
	// 	var c, d = one(), one()
	// 	fmt.Print(c, &a, &b, &c, d)
	var e, f = twoone()
	var g, h, i, j, k = five()
	fmt.Print(e, "=2", f, "=1")
	fmt.Print(g, "=1", h, "=2", i, "=3", j, "=4", k, "=5")
	k, j, i, h, g = five()
	fmt.Print(g, "=5", h, "=4", i, "=3", j, "=2", k, "=1")
	print_me(15, 2023)
	print_me(10, 2023)
	print_me(11, 2024)
	print_me(123, 2025)
	print_me(1235, 2032)
	print_me(1235, 2040)
}

func one() int {
	return 1
}

func twoone() (int, int) {
	return 2, 1
}

func five() (int, int, int, int, int) {
	return 1, 2, 3, 4, 5
}

func print_me(i int, j int) {
	var _ = "ignore"
	var x = 2
	var y = x + 2
	var z = y + 2
	fmt.Print("I print", i, j, x, y, z)
}
