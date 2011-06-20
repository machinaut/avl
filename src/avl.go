/***********************************************************************
    Module:  avl.f
 
    Copyright (C) 2002 Mark Drela, Harold Youngren
 
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
***********************************************************************/
package main // TODO avl
/***********************************************************************
     3-D Vortex Lattice code.
     See file avl_doc.txt for user guide.
     See file version_notes.txt for most recent changes.
***********************************************************************/
import ( 
    "fmt"
)

var (
version = 3.333
)

func Preamble() {
    fmt.Println(" ===================================================")
    fmt.Println("  Athena Vortex Lattice  Program       Version ", Version)
    fmt.Println("  Copyright (C) 2002   Mark Drela, Harold Youngren")
    fmt.Println("  This software comes with ABSOLUTELY NO WARRANTY,")
    fmt.Println("    subject to the GNU General Public License.")
    fmt.Println("  Caveat computor")
    fmt.Println(" ===================================================")
}

func main() {
    Preamble()
}

