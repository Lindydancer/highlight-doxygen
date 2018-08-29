// markdown.h --- Example C++ code with Doxygen comments.

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

// Example C++ code with Doxygen markdown-style blocks.

// Code:

//! This comment contains an implicit verbatim block.
//!
//!     This is verbatim
//!        And this too.
//!
//! Just check so that verbatim blocks aren't treated as code:
//!
//!     if (true)
//!     {
//!       ...
//!     }
//!
//! End of comment.

class TestVerbatimImplicit
{
public:
  /*!
   *
   * The following is not seen as a verbatim block by Doxygen
   *
   * Zero
   *  One
   *   Two
   *    Three
   *     Four
   *      Five
   *       Six
   *        Seven
   *         Eight
   *
   * Part of the following is seen as verbatim block by Doxygen.
   *
   *         Eight
   *        Seven
   *       Six
   *      Five
   *     Four <---- Last line of the block
   *    Three
   *   Two
   *  One
   * Zero
   *
   * This is more plain text.
   */
  void TestVariableIndentation();

  // The following two test shows that the block must be indented four
  // spaces more than the surrounding text, even if the surrounding
  // text starts one column to the right due to the "!" sign.

  /*! Test initial block.
   *
   *     Four spaces.  Not seen as block by Doxygen.
   */
  void TestInitialBlock4();

  /*! Test initial block.
   *
   *      Five spaces.  Seen as block by Doxygen.
   */
  void TestInitialBlock5();

  /*! Test block at the end.
   *
   * This is plain text.
   *
   *     This is a block
   */
  void TestBlockAtTheEnd();

  /*! Test if block can start without an empty line. (No, they can't.)
   *
   * Text
   *     Maybe block
   *
   * Text2
   *     Maybe block
   * Text3
   */
  void TestTightBlock();

  /*! Test if blocks indented a bit at a time is seen as a block.
   *
   * Text1
   *
   *    Text2
   *
   *       Text3
   *
   *           Text4   <- Indented with 4 spaces, seen as block.
   *
   *       Text5
   *
   *    Text6
   *
   * Text1
   *
   *    Text2
   *
   *       Text3
   *
   *           Text4   <- Indented with 4 spaces, seen as block.
   *       Text5 <- Not part of block.
   *    Text6
   */
  void TestStaggering();

  /*!
   * Test if a block can start with a more indented line.
   *
   *         Text8
   *        Text7
   *
   * Text after.
   */
  void TestMoreIndented();

  //!
  void TestEmptySlashSlashComment();

  //! Test code block at end of C++ comment.
  //!
  //!     if (test)
  //!     {
  //!       ..
  //!     }
  void TestEndOfSlashSlashComment();

  //! Test MarkDown code blocks after non-straight left margin.
  //!
  //! Alpha
  //!  Beta
  //!   Gamma
  //!
  //!     Is this a code block? (NO, according to Doxygen)
  //!
  //!   Alpha
  //!  Beta
  //! Gamma
  //!
  //!     Is this a code block? (YES, according to Doxygen)
  //!
  //! Alpha
  //!  Beta
  //! Gamma
  //!
  //!     Is this a code block? (YES, according to Doxygen)
  void TestNonStraightMargin();

  //! Test code block with empty lines
  //!
  //! Empty line (no spaces):
  //!
  //!     if (test)
  //!     {
  //!
  //!     }
  void TestEmptyLinesInCodeBlock();

  //! Test explicit code blocks.
  //!
  //! Explicit code block:
  //!
  //!     \code
  //!     if (test)
  //!     {
  //!
  //!     }
  //!     \endcode
  //!
  //! Explicit code block after markdown block:
  //!
  //!     Is this a code block? (YES, according to Doxygen)
  //!
  //!     \code
  //!     if (test)
  //!     {
  //!
  //!     }
  //!     \endcode
  //!
  //! Explicit code block before markdown block, same indentation:
  //!
  //!     \code
  //!     if (test)
  //!     {
  //!
  //!     }
  //!     \endcode
  //!
  //!     Is this a code block? (NO, according to Doxygen)
  //!
  //! Explicit code block before markdown block, more indentation:
  //!
  //!     \code
  //!     if (test)
  //!     {
  //!
  //!     }
  //!     \endcode
  //!
  //!         Is this a code block? (YES, according to Doxygen)
  //!
  //! Explicit code block before markdown block, commands indented:
  //!
  //!                         \code
  //!     if (test)
  //!     {
  //!
  //!     }
  //!                         \endcode
  //!
  //!         Is this a code block? (NO, according to Doxygen)
  void TestMixExplicitAndImplicit();
};

// markdown.h ends here
