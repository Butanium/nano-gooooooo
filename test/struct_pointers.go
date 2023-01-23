package main

import "fmt"

type A struct{ sa string }

type B struct {
	a  A
	sb string
}

func main() {
	var eB B
	var eA A
	eA.sa = "structure e2"
	eB.sb = "structure e1"
	eB.a = eA

	pA := &eA
	p2 := &eB.a

	pA.sa = "pointeur 1"
	fmt.Print(eB.sb, pA.sa)
	p2.sa = "pointeur 2"

	fmt.Print(eB.sb, pA.sa)
}
