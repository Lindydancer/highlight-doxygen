// known-problems.h --- Example C++ code with Doxygen comments.

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

// Example C++ code with Doxygen comments demonstrates a number of
// known problems with the `highlight-doxygen' package.

// Code:

//! Doxygen comments with known problems.
class TestKnownProblemsClass
{
public:
  /*! This is a C-style block.
     The package can't highlight the following empty line as the rest
     of the block, because there are no characters on it to highlight.

   */
  void TestCStyleBlocksWithEmptyLines();

  /*! \brief Some text
   *  \namespace FakeKnownProblemNamespace
   *
   * This and the next comment should be highlighted as separate
   * comment blocks.
   */

  /*! \brief Some text
   *  \namespace FakeKnownProblemNamespace2
   */

  //! Code block with empty lines.  Again, there are no characters to
  //! put the highlighting on, to make it appear like a box.
  //!
  //! \code
  //!     if (true)
  //!     {
  //!
  //!     }
  //! \endcode
  void TestCodeBlockWithEmptyLine();

  //! Test code block where one "endcode" is missing.
  //!
  //! \code
  //!     if (true)
  //!     {
  //!       ...
  //!     }
  //!
  //! This should not be highlighted as code.
  //!
  //! \code
  //!     if (true)
  //!     {
  //!       ...
  //!     }
  //! \endcode
  void TestMissingEndCode();

  //! Test "verbatim".
  //!
  //! Currently, the `highlight-doxygen' package highlights verbatim
  //! blocks like code blocks.  However, in reality EVERYTHING between
  //! "verbatim" and "endverbatim" is included, even comment start
  //! characters.
  //!
  //! \verbatim
  //! This block is verbatim (including the //! on this and the next line).
  //! \endverbatim
  void TestVerbatim();

  //! Test "verbatim" across Doxygen comments.
  //! \verbatim
  //! This is verbatim.
  void TestVerbatimAcrossComments_DoxygenThinksThisIsPartOfTheComment();

  //! This is also verbatim.
  //! \endverbatim
  //!
  //! Text after "verbatim".

  void TestVerbatimAcrossComments2();
};

// For code line comments, the "*/" should not be highlighted.

int x;

/*! \var int x; */

// known-problems.h ends here
