// code.h --- Example C++ code with emphasis comments.

// Commentary:

// Copyright (C) 2019 Anders Lindgren

// Author: Anders Lindgren

// This file is part of the `highlight-doxygen' package.

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Commentary:

// Example C++ code with emphasis comments.

// Code:

//! *This is emphasized* by Doxygen.
//!
//! *This*is*emphasized* by Doxygen.
//!
//! This is _also emphasized_ by Doxygen.
//!
//! This is _also_emphasized_ by Doxygen.
//!
//! **This is emphasized alot** by Doxygen.
//!
//! **This*is*emphasized*alot** by Doxygen.
//!
//! This is __also emphasized alot__ by Doxygen.
//!
//! This is __also_emphasized_alot__ by Doxygen.
//!
//! This is ~~strikedthrough~~ by Doxygen.
//!
//! This is ~~striked~~through~~ by Doxygen.
//!
//! ~~Double tilde~~
//!
//! This is `a piece is code`.
//!
//! Double backtick:s ``xxx`yyy``.
//!
//! Double backtick:s ``xxx`yyy`` and ``xxx`yyy``.
//!
//! Note: The following example is from the Doxygen manual. However,
//! when passed through Doxygen, the quote to the left of "ls" is
//! rendered using a code font, whereas the one to the right isn't. My
//! conclusion is that the two *leftmost* backticks after "ls" is
//! considerend the end of the code area, and the rightmost is emitted
//! using a plain font.
//!
//! Double backtick:s ``var=`ls```.
//!
//! Likewise, seven out of the nine backticks are emitted using a
//! non-code font.
//!
//! Double backtick:s ``xxx`yyy`````````.
class TestEmphasize
{
};


//! This*is*not*emphasized* by Doxygen.
//!
//! This is not_emphasized by Doxygen.
//!
//! This is `not code' as the plain quote undo the effect of the
//! backtick.
//!
//! What about `this` 'this' `this' and `this`? (First and last is
//! highlighted.)
//!
//! This is ~~not striked through~~ by Doxygen.
class TestEmphasizeNegative
{
};


//! Test `code
//! spanning` multiple lines. (Handled by Doxygen.)
//!
//! Test ``code
//! with double ` spanning`` multiple lines. (Handled by Doxygen.)
//!
//! Test *emphasis
//! spanning* multiple lines. (Handled by Doxygen.)
class TestEmphasizeMultiLine
{
};


//! Test `code
//!
//! spanning` multiple paragraphs. (Not handled by Doxygen.)
//!
//! Test *emphasis
//!
//! spanning* multiple paragraphs. (Not handled by Doxygen.)
class TestEmphasizeMultiParagraphs
{
};
