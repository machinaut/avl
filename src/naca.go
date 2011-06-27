// naca.go  Generate NACA airfoils
// Alex Ray <ajray@ncsu.edu> 2011
// XXX unfinished and untested

package main

import (
    "math"
)

type Naca struct {
    C   float64 // maximum camber as fraction of the chord
    P   float64 // location of maximum camber as a fraction of the chord
    T   float64 // maximum thickness as a fraction of the chord
}

// Thickness at x position along chord from 0.0 to 1.0
func (n Naca) Thickness(x float64) float64 {
    if x > 1.0 || x < 0.0 { // outside of airfoil 
        return math.NaN()
    }
    return (0.29690*math.Pow(x, 0.5) +
        -0.12600*math.Pow(x, 1.0) +
        -0.35160*math.Pow(x, 2.0) +
        +0.28430*math.Pow(x, 3.0) +
        -0.10150*math.Pow(x, 4.0)) * n.T / 0.10
}

// Camber at x position along chord from 0.0 to 1.0
func (n Naca) Camber(x float64) float64 {
    if x > 1.0 || x < 0.0 { // outside of airfoil 
        return math.NaN()
    } else if x < n.P { // before point of max camber
        return n.C * (x) * (2.*n.P - x) / math.Pow(n.P, 2.)
    }   // after point of max camber
    return n.C * (1. - x) * (1. + x - 2.*n.P) / math.Pow(1.-n.P, 2.)
}

// Slope value at x position along chord from 0.0 to 1.0
// this is the slope of the mean camber line
func (n Naca) Slope(x float64) float64 {
    if x > 1.0 || x < 0.0 { // outside of airfoil 
        return math.NaN()
    } else if x < n.P { // before point of max camber
        return n.C / math.Pow(n.P, 2.) * 2.0 * (n.P - x)
    }   // after point of max camber
    return n.C / math.Pow(1.0-n.P, 2.) * 2.0 * (n.P - x)
}

// Theta value at x position along chord from 0.0 to 1.0
func (n Naca) Theta(x float64) float64 {
    if x > 1.0 || x < 0.0 { // outside of airfoil 
        return math.NaN()
    }
    return math.Atan(n.Slope(x))
}

