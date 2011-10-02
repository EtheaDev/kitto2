/*************************************************
*      Perl-Compatible Regular Expressions       *
*************************************************/

/*
This is a library of functions to support regular expressions whose syntax
and semantics are as close as possible to those of the Perl 5 language. See
the file Tech.Notes for some information on the internals.

Written by: Philip Hazel <ph10@cam.ac.uk>

           Copyright (c) 1997-1999 University of Cambridge

-----------------------------------------------------------------------------
Permission is granted to anyone to use this software for any purpose on any
computer system, and to redistribute it freely, subject to the following
restrictions:

1. This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

2. The origin of this software must not be misrepresented, either by
   explicit claim or by omission.

3. Altered versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
-----------------------------------------------------------------------------
*/

/* pcrelib.cpp created by Jan Goyvaerts <vcl@jgsoft.com>
   Done to put the pcre library into a DLL for use with the TPerlRegEx Delphi VCL component
   Compiles with Borland C++Builder 3.0 */

#include <windows.h>
#pragma hdrstop
#include <condefs.h>

#include "pcre.h"
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
