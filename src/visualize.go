// visualize.go  try to visualize AVL files (somehow)
// Alex Ray <ajray@ncsu.edu> 2011

package main

import (
    "fmt"
    "math"
)

type Airfoil struct {

}

func main() {
    fmt.Println("Hello, World")
    n := Naca{.02, .4, .12} // Cessna 2412 airfoil
    fmt.Println(n)
}
