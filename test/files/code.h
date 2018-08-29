// code.h --- Example C++ code with Doxygen comments.

// Commentary:

// Copyright (C) 2018 Anders Lindgren

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

// Example C++ code with Doxygen "code" blocks.

// Code:


//! This comment contains a "code" block.
//!
//! \code
//! if (true)
//! {
//!   ...
//! }
//! \endcode
//!
//! This text is after the code block.

class TestCodeKeyword
{
};

//! This comment contains a "code" block with a known extension.
//!
//! \code{.rb}
//! def foo(&block b)
//! {
//!   yield
//! }
//! \endcode
//!
//! This text is after the code block.

class TestRubyCodeKeyword
{
};

//! This comment contains a "code" block with an unknown extension.
//!
//! \code{.unknown}
//! def foo(&block b)
//! {
//!   yield
//! }
//! \endcode
//!
//! This text is after the code block.

class TestUnknownCodeKeyword
{
};

//! This comment contains a one-line "code" block.
//!
//! Note: When Doxygen renders the following paragraph, it is split
//! into three separate paragraphs, hence this isn't a good way to
//! write inline code snippets.
//!
//! Here comes some code \code while(true); \endcode but this isn't
//! code.
//!
//! More comments.

class TestCodeKeywordOneLine
{
};

//! Testing dot.  This relies on having the package
//! `graphviz-dot-mode' installed.
//!
//! \dot
//! digraph example {
//! node [shape=record, fontname=Helvetica, fontsize=10];
//!    b [ label="class B" URL="\ref B"];
//!    c [ label="class C" URL="\ref C"];
//!    b -> c [ arrowhead="open", style="dashed" ];
//! }
//! \enddot
//!
//! Note: The following doesn't work properly.
//!
//! \dot "Caption" width=10cm
//! digraph example {
//! node [shape=record, fontname=Helvetica, fontsize=10];
//!    b [ label="class B" URL="\ref B"];
//!    c [ label="class C" URL="\ref C"];
//!    b -> c [ arrowhead="open", style="dashed" ];
//! }
//! \enddot

class TestDotBlock
{
};

//! Test code block indented with tabs
//!
//! Aligned with spaces.
//!
//!         \code
//!     if (true)    // Spaces
//!	{            // Tab
//!	  true;      // Tab
//!     }            // Spaces
//!         \endcode
//!
//! Not aligned.
//!
//!         \code
//!   if (true)      // Spaces
//!   {              // Spaces
//!	true;        // Tab
//!   }              // Spaces
//!         \endcode

// code.h ends here
