package main
import (
    "fmt"
)

type Configuration struct {
    Name string
    Mach float64
    IYsym, IZsym int
    Zsym float64
    Sref,Cref,Bref float64
    Xref,Yref,Zref float64
    CDp float64
}

type Surface struct {
    Name string
    Nchord,Nspan uint
    Cspace,Sspace float64
    Lcomp uint // XXX rename this later
    Ydupl float64
    Xscale,Yscale,Zscale float64
    Xtrans,Ytrans,Ztrans float64
    Angle   float64
    Wake,Albe,Load bool
}

type Section struct {
    Xle,Yle,Zle float64 // airfoil's leading edge location
    Chord float64 // the airfoil's chord  (trailing edge is at Xle+Chord,Yle,Zle)
    Angle float64 // incidence angle, taken as a rotation (+ by RH rule) about
                  // the surface's spanwise axis projected onto the Y-Z plane.

    Nspan uint  // number of spanwise vortices until the next section [ optional ]
    Sspace float64  // controls the spanwise spacing of the vortices      [ optional ]
    Airfoil Airfoil
    Claf float64
    Cdcl
    Design
    Control []Control


func ReadInputFile(filename string) *Configuration {
    return new(Configuration)
}
