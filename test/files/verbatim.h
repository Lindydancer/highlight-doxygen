// verbatim.h --- Example C++ code with Doxygen comments.

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

// Example C++ code with Doxygen "verbatim" blocks.

// Code:

// Note: It appears as though "verbatim" can't stretch across C++
// comments. Also, it emits formatting like leading asterisks.
//
// In most cases, a "\code" block or a MarkDown-style verbatim is
// perferred.

//! This comment contains a "verbatim" block.
//!
//! \verbatim
//! This is verbatim
//! \endverbatim
//!
//! More comments.

class TestVerbatimKeywordCpp
{
};


/*! This comment contains a "verbatim" block.
 *
 * The following will contain the " *":s in the output.
 *
 * \verbatim
 * This is verbatim
 * \endverbatim
 *
 * More comments.
 *
 * \verbatim
This is also verbatim
\endverbatim
 *
 */

class TestVerbatimKeywordC
{
};

//! This comment contains a "verbatim" block.
//!
//! Note: When Doxygen renders the following paragraph, it is split
//! into three separate paragraphs, hence this isn't a good way to
//! write inline code snippets.
//!
//! The following \verbatim is verbatim \endverbatim but this isn't.
//!
//! More comments.

class TestVerbatimKeywordOneLine
{
};

// verbatim.h ends here
