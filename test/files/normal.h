// normal.h --- Example C++ code with Doxygen comments.

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

// This file contains most, if not all, doxygen commands.
//
// The main purpouse of this file is for regression testing of the
// Emacs package "highlight-doxygen".
//
// Doxygen comments with code examples are in separate files.

// Conventions:
//
// Constructs defined in this file are named TestXxx.
//
// Constructs defined using doxygen commands like "\class" are named
// FakeXxx or fakexxxx.


// ------------------------------------------------------------
// Commands that define construct somewhere else
//

// TODO: The [header-file] [header-name] syntax in all of the following:

//! \class fakeclass
//!
//! This is a fake class.

//! \class FakeClass
//!
//! This is a fake class.

//! \enum fakeenum
//!
//! This is a fake enum.

//! \enum FakeEnum
//!
//! This is a fake enum.

//! \interface fakeinterface
//!
//! This is a fake interface.

//! \interface FakeInterface
//!
//! This is a fake interface.

//! \namespace fakenamespace
//!
//! This is a fake namespace.

//! \namespace FakeNamespace
//!
//! This is a fake namespace.

//! \namespace fakeparent::fakenamespace
//!
//! This is a fake namespace.

//! \namespace FakeParent::FakeNamespace
//!
//! This is a fake namespace.

//! \struct fakestruct
//!
//! This is a fake struct.

//! \struct FakeStruct
//!
//! This is a fake struct.

//! \union fakeunion
//!
//! This is a fake union.

//! \union FakeUnion
//!
//! This is a fake union.

// Note: Objective-C.

//! \category fakecategory
//!
//! This is a fake category.

//! \category FakeCategory
//!
//! This is a fake category.

//! \protocol fakeprotocol
//!
//! This is a fake protocol.

//! \protocol FakeProtocol
//!
//! This is a fake protocol.



// --------------------
// Class
//

//! Class for testing "related" etc.
class TypeClass
{
};

//! A related function. \relates TypeClass.
void TestRelates(void);

//! This is a namespace.
namespace TestNamespace
{
}

//! This is a union.
union TestUnion
{
};

//! This is an enum.
enum TypeEnum
{
};


//! Template class.
//!
//! \tparam arraysize size of array.
template<N arraysize>
class TestTemplateClass
{
  int arr[arraysize];
};


//! Function that throws.
//!
//! \throw TestException
//! \throw Simple
//! \throw lowercase
//! \throw Qualified::Tip
void TestThrows() throw TestException;

//! Another function that throws.
//!
//! \throw std::out_of_range.
void TestThrows2() throw std::out_of_range;

// --------------------
// section-label
//

//! \if alabel
//! This is the "this" part.
//! \elseif alabel
//! This is the "else" part.
//! \endif

void TestSectionLabel();

//! \ifnot alabel
//! This is the "this" part.
//! \elseif alabel
//! This is the "else" part.
//! \endif

void TestSectionLabel2();


// --------------------
// Groups
//

//! \defgroup group1 This is a group title.
//! This is a group body.

//! \addtogroup group1
//! More group body.

//! \weakgroup my_weak_group
//! This is a weak group.

//! \ingroup group1
//! This is in group1.

void TestIngroup(void);

// TODO: Add support for specifying multiple groups.

//! \ingroup group1 my_weak_group
//! This is in group1 and my_weak_group.

void TestIngroup2(void);

//! \dir This/is/a/dir

void TestDir(void);

// TODO: If Doxygen can handle this, ensure we can too.

//! \dir "This is a path with spaces"

void TestDir2(void);

// TODO: Check how Doxygen handle this.

//! \dir a_dir This is not a dir.

void TestDir3(void);

// --------------------
// With filename arguments.

// Syntax:
//
//    \class name [<header-file>] [<header-name>]


// Note: The "." isn't written in verbatim, as that produces a link.

// Conclusion: <header-file> can be foo.h or "foo.h".

//! \class FakeClassNameP other.h
//! Class with other dot h

//! \class FakeClassNameQ "other.h"
//! Class with "other dot h"

//! \class FakeClassNamePP other.h sub/other.h
//! Class with other dot h sub/other dot h

//! \class FakeClassNamePQ other.h "sub/other.h"
//! Class with other dot h "sub/other dot h".

//! \class FakeClassNamePS other.h <sub/other.h>
//! Class with other dot h \<sub/other dot h\>.

/**
 * \a arg
 */

// Note: The following doesn't seem to work in doxygen.
//
// The documentation specifies:
//
//    \class name [<header-file>] [<header-name>]
//
// I guess that the documentation should read:
//
//    \class name [<header-file> [<header-name>]]

#if 0
//! \class FakeClassNameSubP sub/other.h
//! Class with sub/other dot h.

//! \class FakeClassNameSubQ "sub/other.h"
//! Class with "sub/other dot h".

// This fails badly, it generates "#include <<>".

//! \class FakeClassNameS <other.h>
//! Class with \<other dot h\>

//! \class FakeClassNameSubS <sub/other.h>
//! Class with \<sub/other dot h\>.
#endif


// TODO: This is another animal than the above.

//! \exception fakeexception
//!
//! This is a fake exception.

//! \exception FakeException
//!
//! This is a fake exception.

//! This is a \throw throw but this isn't.
//!


// ------------------------------------------------------------
// Commands that define constructs in the file
//

// Place this is a comment OF something.

//! Link to the other.h header file.



//! Commands that take a variable as argument.
//!
//! TODO: param
//!
//! This is a \a parameter but this isn't.
//!
//! TODO: Is this really a variable?
//!
//! This is \relates relates but this doesn't.
//!
//! This is \relatesalso relatesalso but this doesn't.
//!
//! This is a \var variable but this isn't.
//!
//! This is a \def variable but this isn't.
//!
//! This \implements FakeClass::FakeFunction.
//!
//! This \implements fakeclass::fakefunction.

class TestVarArgs
{
};



//! Test if the argument can be placed on another line.
//!
//! Highlighting for this is currently not supported.
//!
class TestLineBreaks
{
public:
  //! Test if \a
  //! my_argument can be on the next line.
  //!
  //! Check if there can be an empty line between the command and the
  //! agument, like \a
  //!
  //! arg2 here.
  //!
  //! Doxygen handles line breaks, but not empty lines.
  void TestLineBreaks(int my_argument, int arg2)
  {
  }
};


//! Commands that take a function name as arguments:
//!
//! This is a \retval function but this isn't.

class TestRetval
{
};


//! Commands that take a "word" as argument:
//!
//! This is a \a parameter but this isn't.
//!
//! This is a \p parameter but this isn't.
//!
//! This is an \anchor anchor but this isn't.
//!
//! This is \b bold but this isn't.
//!
//! This is \c code but this isn't.
//!
//! This is \e emphasized but this isn't.
//!
//! This is \em emphasized but this isn't.
//!
//! \retval ZERO    if \p parameter is zero
//! \retval NONZERO if \p parameter is non-zero

int TestWordFunction(int parameter)
{
  if (parameter > 0)
  {
    return NONZERO;
  }
  else
  {
    return ZERO;
  }
}

//! Parameter commands
//!
//! \param plain a plain parameter
//! \param [in] x an "in" parameter
//! \param [out] y an "in" parameter
//! \param [in,out] z an "in" parameter
//!
//! Without whitespace:
//!
//! \param[in]x an "in" parameter
//! \param[out]y an "in" parameter
//! \param[in,out]z an "in" parameter
//!
//! Partially written:
//!
//! \param[in]

void test(int plain, int x, int y, int z);


//! \sa TestOptional
//!
//! \sa Paragraph with TestOptional reference.
//!
//! \sa http://www.some-company.com/a%20page.html
//!
//! \sa This is a paragraph with links like this one
//! http://www.some-company.com/a%20page.html and
//! function_like_this_one().

class TestSeeAlso
{
};

// TODO: link copydoc xrefitem if ifnot elseif

//! Commands that take an optional argument:
//!
//! TODO: How?
//!
//! \cond
//!
//! \cond argument
//!
//! \cond argument1 argument2
//!
//! \endcond
//!
//! \endcond
//!
//! \endcond
//!
//! \dir
//!
//! \dir argument
//!
//! \dir argument1 argument2
//!
//! \~
//!
//! \~argument
//!
//! \~argument1 argument2

class TestOptional
{
};


//! \ref a_reference but this isn't.

class TestReference
{
};


//! Testing filenames:
//!
//! This is an \example example but this isn't.
//!
//! This is an \example example.cpp but this isn't.
//!
//! This is an \example "example.cpp" but this isn't.
//!
//! This is an \example "a/path/to/an/example.cpp" but this isn't.
//!
//! This is an \example "a\path\to\an\example.cpp" but this isn't.
//!
//! This is an \example "a path with spaces/example.cpp" but this isn't.
//!
//! This is an \example "a path with spaces\example.cpp" but this isn't.

class TestFilename1
{
};

//! Commands that take a file name as arguments:
//!
//! This is an \example example.h but this isn't.
//!
//! This is a \dont dont.h but this isn't.
//!
//! This is a \dontinclude dontinclude.h but this isn't.
//!
//! This is a \includelineno includelineno.h but this isn't.
//!
//! This is a \htmlinclude htmlinclude.html but this isn't.
//!
//! This is a \verbinclude verbinclude.h but this isn't.

class TestFilename2
{
};

//! Commands that take one word as arguments:
//!
//! This is an \addtopage addtopage but this isn't.
//!
//! This is a \defgroup defgroup but this isn't.
//!
//! This is a \weakgroup wekgroup but this isn't.
//!
//! This is a \page page but this isn't.
//!
//! This is an \anchor anchor but this isn't.
//!
//! This is a \ref ref but this isn't.
//!
//! This is a \section section but this isn't.
//!
//! This is a \subsection subsection but this isn't.

class TestWord2
{
};

//! Special:
//!
//! \ingroup arg1 arg2 arg3 arg4
//!
//! \dotfile filename "This is a caption" but this isn't.
//!
//! \image jpg filename "This is a caption"
//!
//! \image jpg filename "This is a caption" size=100

class TestSpecial
{
};

//! End-of-line code.
//!
//! \fn void f();
//!
//! \var int x;

/*!
 * \var int x;
 */

/*! \var int x; */


// ------------------------------------------------------------
// Copy commands

//! Test copy commands.
class TestCopy
{
public:
  //! A function.
  TestFunction(int);

  //! \copydoc TestFunction()
  int TestFunction2(int parameter);

  //! \copydoc TestFunction(int)
  int TestFunction3(int parameter);


  //! Function with types with spaces.
  int TestUnsignedLong(unsigned long parameter);

  //! \copydoc TestUnsignedLong(unsigned long)
  int TestUnsignedLong2(unsigned long parameter);

  //! \copydoc TestUnsignedLong(unsigned long) And some more text.
  int TestUnsignedLong3(unsigned long parameter);

  //! \copydoc TestUnsignedLong(unsigned long) \return a value.
  int TestUnsignedLong4(unsigned long parameter);
};



// normal.h ends here
