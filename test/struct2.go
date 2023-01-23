package main

import "fmt"

type Point struct{ x, y int }

func point() {
	var p Point
	p.x = 1
	fmt.Print(p)
}

type Singleton struct{ x int }

func singleton() {
	var s Singleton
	// s.x = 1
	fmt.Print(s)
}

func main() {
	singleton()
}
