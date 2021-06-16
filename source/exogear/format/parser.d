/+

FormatString:
    FormatStringItem*
FormatStringItem:
    '%%'
    '%' Position Flags Width Separator Precision FormatChar
    '%(' FormatString '%)'
    '%-(' FormatString '%)'
    OtherCharacterExceptPercent
Position:
    empty
    Integer '$'
Flags:
    empty
    '-' Flags
    '+' Flags
    '#' Flags
    '0' Flags
    ' ' Flags
Width:
    empty
    Integer
    '*'
Separator:
    empty
    ','
    ',' '?'
    ',' '*' '?'
    ',' Integer '?'
    ',' '*'
    ',' Integer
Precision:
    empty
    '.'
    '.' Integer
    '.*'
Integer:
    Digit
    Digit Integer
Digit:
    '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
FormatChar:
    's'|'c'|'b'|'d'|'o'|'x'|'X'|'e'|'E'|'f'|'F'|'g'|'G'|'a'|'A'|'|'

enum Symbol : ubyte
{
	FormatString     = 0,
	FormatStringItem = 1,
	Position         = 2,
	Flags            = 3,
	Width            = 4,
	Separator        = 5,
	Precision        = 6,
	Integer          = 7,
	Digit            = 8,
	FormatChar       = 9
}



struct DefaultNode
{
	Symbol       symbol;
	ParseResult  match;
}

struct FormatStringParser(CallerNodeT, Char = char)
{
private:
	// These fields represent state that persists between calls into the
	// parser (ex: popFront).
	const(Char)[] corpus;
	size_t        cursor = 0;

	// Anything that persists during parser descent but NOT between external
	// calls into the parser should go into this structure.
	//
	// There should only be one of this during parsing. Pass by reference always.
	struct CallState
	{
		ParseFrame  root;
	}

public @nogc nothrow:
	struct ParseMatch
	{
		Symbol         symbol;
		const(Char)[]  text;
		size_t         pos;

		@property @nogc nothrow const
		bool success() {
			return match !is null;
		}
	}

	struct ParseFrame
	{
		ParseMatch  match;
		ParseFrame* parent = null; // Root-ward
		ParseFrame* child  = null;  // Leaf-ward
	}


	this(const(Char)[] text)
	{
		init(text);
	}

	/// This function is used by the constructor to initialize the parser.
	///
	/// Calling this on an existing parser is equivalent to replacing that
	/// parser by invoking the FormatStringParser's constructor with the
	/// same arguments.
	void init(const(Char)[] text)
	{
		this.corpus = text;
		this.cursor = 0;
		this.root = null;
	}

	@property bool empty() {
		return (this.cursor == 
	}

	void parse(Symbol whatToParse = Symbol.FormatString)(const(Char)[] text)
	{
		CallState state;
		state.root.match.pos = 0;
		state.root.match.symbol = whatToParse;

		dispatch!whatToParse(state, text);
	}

	private template dispatch(Symbol symbol)
	{
		import std.conv;
		import std.ascii : newline;
		import std.meta : Filter;
		import std.traits : hasUDA, isSomeFunction, Parameters, ReturnType;

		void parseFuncPrototype(ref CallState state);

		enum typeSeqsEqual_(alias seqA, alias seqB) =
			(seqA.length == 1 && is(seqA[0] == seqB[0]))
			|| (is(seqA[0] == seqB[0])
				&& typeSeqsEqual_!(seqA[1..$], seqB[1..$]));

		enum typeSeqsEqual(alias seqA, alias seqB) =
			seqA.length == seqB.length &&
			(seqA.length == 0
				|| seqsEqual_!(alias seqA, alias seqB) );

		enum isAlmostParserForSymbol(alias member) =
			isSomeFunction!member && hasUDA!(member, symbol);

		enum isParserForSymbol(alias member) =
			isSomeFunction!member && hasUDA!(member, symbol)
			&& typeSeqsEqual(Parameters!member, Parameters!parseFuncPrototype)
			&& is(ReturnType!member == ReturnType!parseFuncPrototype);

		alias funcsMatchingSymbol =
			Filter!( isAlmostParserForSymbol, __traits(allMembers, typeof(this)) );

		alias parseFuncs =
			Filter!( isParserForSymbol, funcsMatchingSymbol );

		enum fmtFunction(alias fn) =
			newline ~ "\t" ~ fn.stringof;

		static if ( parseFuncs.length > 1 )
			static assert(0, newline
				~ "Error: More than one parsing function declared for grammar symbol "
				~ to!string(symbol)~ "."~ newline~ "Functions matched are:"~
				~ staticMap!(fmtFunction, parseFuncs));
		else
		static if ( parseFuncs.length < 1 )
		{
			static if ( funcsMatchingSymbol.length > 0 )
			{
				// Ugh, natural languages and their dirty inflection habits.
				static if ( funcsMatchingSymbol.length == 1 )
				{
					enum s = "";
					enum pronoun = "its";
				}
				else
				{
					enum s = "s";
					enum pronoun = "their";
				}

				static assert(0, newline
					~ "Error: Found parsing function"~s~" declared for grammar symbol "
					~ to!string(symbol)~ ", but "~pronoun~" signature"~s~" don't match."~ newline
					~ "These functions were considered:"
					~ staticMap!(fmtFunction, parseFuncs));
			}
			else
				static assert(0, "No parsing functions declared for grammar symbol "~to!string(symbol)~".");
		}
		else
		{
			// So many things that can go wrong, and nothing did. Yay!
			alias dispatch = parseFuncs[0];
		}
	}

	@(Symbol.FormatString)
	void parseFormatString(ref CallState state)
	{
		
	}

	void resumeParse(InputRangeT)(const(Char)[] text, size_t atPos, InputRangeT symbolStack)
}


@(Symbol.FormatString)
ParseResult parseFormatSpec(const(char)[]
{

}


/// Returns: an index into 'text' where the match was found, or false
///   (defined by the 'index' type as any negative non-zero value)
///   if the match was not found.
private @nogc nothrow
index scanByteOneAtATimeUntil(ubyte toMatch)(const(ubyte)[] text)
{
	size_t len = text.length;
	for ( index i = 0; i < len; i++ )
		if ( text[i] == toMatch )
			return i;
	return index!false;
}


/// Returns: an index into 'text' where the match was found, or false
///   (defined by the 'index' type as any negative non-zero value)
///   if the match was not found.
private pure /*@nogc*/ // TODO: std.utf.decode is probably not @nogc because of the UTFException. It can be fixed with exogear.exception.
index scanUnicodeOneAtATimeUntil(dchar toMatch, Char)(const(Char)[] text)
{
	static assert(0, "Sorry, not implemented.");
}

/// If 'toMatch' is within the ASCII range of characters and 'Char' is char,
/// then this will call scanAsciiOneAtATimeUntil. For valid UTF-8 strings
/// this will result in the same outcome and avoid UTF-8 decoding overhead.
///
/// Returns: an index into 'text' where the match was found, or false
///   (defined by the 'index' type as any negative non-zero value)
///   if the match was not found.
private pure /*@nogc*/
index scanOneAtATimeUntil(dchar toMatch, Char)(const(Char)[] text)
{
	static if (toMatch < 0x80)
		return scanByteOneAtATimeUntil!(cast(ubyte)toMatch)(cast(const(ubyte)[]) text);
	else
		return scanUnicodeOneAtATimeUntil!(toMatch, Char)(text);
}


pure /*nogc*/
index scanUntil(Char, dchar toMatch)(const(Char)[] text)
	if ( is(Char == char) || is(Char == wchar) || is(Char == dchar) )
{
	static if ( is(Char == char) ) // Assumption: utf-8
	{
		static if ( toMatch < 0x80 )
			return scanUtf8Until!( cast(char)toMatch )(text);
		else
			return scanUntil!( toUTF8!([toMatch]) )(text);
	}
	else
	{
		static if ( is(Char == wchar) )
			static assert(0, "UTF-16 and UTF-32 strings not implemented in scanUntil. Was passed a UTF-16 string.");
		else
		static if ( is(Char == dchar) )
			static assert(0, "UTF-16 and UTF-32 strings not implemented in scanUntil. Was passed a UTF-32 string.");
		else
			static assert(0, "Cannot scan string type "~ typeof(text).stringof);
	}

}

pure /*nogc*/
index scanUntil(Char, Char[] toMatch)(const(Char)[] text)
	if ( is(Char == char) || is(Char == wchar) || is(Char == dchar) )
{
	size_t matchLen = toMatch.length;

	// If we pass this point without finding a matching first character
	// (the anchor), then this endeavor is hopeless because the rest of the
	// string can't possibly contain what we're trying to match.
	// The fastest/best thing we can do is return false without spending
	// any more cycles on the task.
	size_t scanAnchorLen = (text.length - matchLen) + 1;

	while (true)
	{
		index candidateIdx = scanUntil!(toMatch[0])( text[0 .. scanAnchorLen] );
		if ( !candidateIdx )
			return index!false;

		alias lower = candidateIdx;
		index upper = candidateIdx + matchLen;
		if ( text[lower .. upper] == toMatch )
			return candidateIdx;
	}
	assert(0);
}

//
alias LargestIntType  = uint64;
enum  largestIntWidth = largestIntType.sizeof;

// It might be worth adjusting this for certain os-cpu-compiler targets.
// In particular, compilers might optimize data types like uint64[4] (ex: SSE)
// or uint64[8] (ex: AVX512) using SIMD instructions. If the operations used
// are supported in those instruction sets, the compiler may be able to
// optimize those data types without needing the code to be written using
// vector intrinsics.
alias OptimalUniversalVectorType      = largestIntType;
enum  optimalUniversalVectorWidth     = largestIntWidth * 1;

/// This number can be bitwise ANDed (&) with a pointer to determine if the
/// pointer has proper alignment for the given vector type. If the result of
/// the AND expression is 0, then the pointer is aligned.
enum  optimalUniversalVectorAlignMask = optimalUniversalVectorWidth - 1;

/// Generates a value of IntVectorT in which each component has a value of 1.
/// The size of the components is given by 'nBitsInScalar'.
///
/// This uses a pattern seen on the Bit Twiddling Hacks site as ~0UL/255
/// ... which would expand to 0x01010101.
/// https://graphics.stanford.edu/~seander/bithacks.html
/// That was a less generic version that assumed bytes as the scalar, but
/// nonetheless works well at scaling up to arbitrarily large (vector) integers.
///
/// This version supports arbitrarily wide or narrow scalars, including
/// scalars that do not evenly divide the vector's bits. Any unallocated
/// bits in the vector are assumed to be the highest bits in the enclosing
/// integer(s) and will be cleared.
///
/// Caveat: This might not do what you expect if the scalar width does
/// not evenly divide IntVectorT AND IntVectorT is a static array or
/// SIMD vector intrinsic. For instance, with IntVectorT as uint16[3]
/// and nBitsInScalar=12, one might wish for 4 scalars to fit
/// (because 16*3 == 48, and 12*3 == 48) and thus expect 4 values of 1
/// to be kicking around somewhere in the uint16[3]. However, this
/// calculation will set each uint16 to 0x0001, with the high 4 bits of
/// every uint16 being the cleared leftover.
///
template intVecX0001(uint16 nBitsInScalar, IntVectorT)
{
	enum UIntVectorT = Unsigned!VectorT;
	enum nBitsRemaining = ((UIntVectorT.sizeof * 8) % nBitsInScalar);
	enum vectorMask = (~ cast(UIntVectorT)0) >> nBitsRemaining;
	alias one = vectorMask/((1UL << nBitsInScalar)-1);
}

/// Generates a value of IntVectorT in which each component has its highest
/// bit set to 1. The size of the components is given by 'nBitsInScalar'.
///
/// This version supports arbitrarily wide or narrow scalars, including
/// scalars that do not evenly divide the vector's bits. Any unallocated
/// bits in the vector are assumed to be the highest bits in the enclosing
/// integer(s) and will be cleared.
///
/// Caveat: This might not do what you expect if the scalar width does
/// not evenly divide IntVectorT AND IntVectorT is a static array or
/// SIMD vector intrinsic. For instance, with IntVectorT as uint16[3]
/// and nBitsInScalar=12, one might wish for 4 scalars to fit
/// (because 16*3 == 48, and 12*3 == 48) and thus expect 4 values of 0x800
/// to be kicking around somewhere in the uint16[3]. However, this
/// calculation will set each uint16 to 0x0800, with the high 4 bits of
/// every uint16 being the cleared leftover.
///
enum intVec1000X(uint16 nBitsInScalar, IntVectorT) =
	intVecX0001!(nBitsInScalar, IntVectorT) << (nBitsInScalar-1);

/// Places the given 'pattern' into each component of IntVectorT.
/// The size of the components is given by 'nBitsInScalar'.
/// The 'pattern' must have no bits higher numbered than 'nBitsInScalar'
/// that are set. In other words, the pattern must fit into the bits
/// provided for each scalar.
template fill(uint16 nBitsInScalar, ulong pattern, IntVectorT)
{
	static assert((pattern >> nBitsInScalar) == 0);
	alias fill = cast(IntVectorT)(one!(nBitsInScalar, IntVectorT) * pattern);
}


/// Scans the string 'text' for the first byte matching 'bytePattern', then
/// returns the index of that byte.
///
/// 'bytePattern' can be any value, including incomplete codepoints or invalid
/// bit patterns. No utf-8 decoding is done in this function, so searching for
/// such things will not cause any exceptions to be thrown. If UTF-8 validation
/// is required, it is expected to be performed by whatever function is calling
/// this one.
///
/// Functions that scan for valid unicode codepoints may use this function by
/// repeatedly (as-needed) finding the first byte in a codepoint sequence and
/// then validating the full sequence until a match is found. This methodology
/// may be faster than decoding every codepoint in the text, as this function
/// can use bit-wise optimizations or SIMD instructions to process more than
/// one byte or character at a time. The only downside is that such
/// a methodology may fail to match overlong utf-8 sequences, as such sequences
/// may encode the same codepoint but differ in the sequence's initial byte.
/// This may not be a significant consideration because overlong utf-8
/// sequences are considered invalid utf-8 and should not be encountered in
/// correctly encoded utf-8 text.
///
/// The current implementation uses bitwise operations on 64-bit integers
/// to process runs of text that have 1 or more 8-byte aligned
/// 8 byte blocks. SIMD instructions are currently unimplemented.
///
/// Returns false if the bytePattern is not present in the given string
///   (the index type defines any negative value to be false).
pure @nogc nothrow
index scanCharsUntil(char bytePattern)(const(char)[] text)
{
	enum ubyte ubytePattern = cast(ubyte)bytePattern;

	// Small string exclusion: just scan less-than-8 byte strings one byte
	// at a time. It will probably be faster than trying to figure out a bunch
	// of alignment just 
	if ( text.length < 8 )
		return scanByteOneAtATimeUntil!(ubytePattern)(text);
	else
		return scanEightOrMoreUtf8Until!(ubytePattern)(text);

}

pure @nogc nothrow
index scanEightOrMoreUtf8Until(ubyte bytePattern)(const(char)[] text)
{
	alias VectorT     = OptimalUniversalVectorType;
	enum  vectorWidth = optimalUniversalVectorWidth;
	enum  alignMask   = optimalUniversalVectorAlignMask;

	auto startPtr = text.ptr;
	auto endPtr   = text.ptr + text.length;

	auto alignedStart = (startPtr | alignMask) + 1;
	auto alignedEnd   = endPtr & ~alignMask;

	ptrdiff_t alignedStartPos = alignedStart - startPtr;
	ptrdiff_t alignedEndPos   = alignedEnd   - startPtr;

	if ( startPtr != alignedStart )
	{
		assert(alignedStart <= endPtr);

		// Check for an unaligned slice that doesn't have any aligned chunks
		// within it. In this case, like with the earlier short-string case,
		// we should probably just scan it one byte at a time.
		if ( endPtr != alignedEnd && alignedStart == alignedEnd )
			return scanByteOneAtATimeUntil!(bytePattern)(text);
		else
		{
			// At this point we've proven that the string has aligned chunks,
			// but we still need to trudge through the unaligned stuff at the
			// beginning. That happens here. We *might* exit early if we find
			// the target in these unaligned bytes.
			index pos = scanByteOneAtATimeUntil!(bytePattern)(text[0 .. alignedStartPos]);
			if ( pos )
				return pos;
		}
	}

	// At this point we've made it to the aligned portion of the string.
	// Trim the slice and dispatch to the most efficient implementation.
	auto alignedText = text[alignedStartPos .. alignedEndPos];
	auto vectorizedText = cast(const(VectorT)[])alignedText;

	static if ( toMatch < 0x80 )
		return scanAlignedAsciiUntil!(bytePattern, vectorWidth)(vectorizedText);
	else
		return scanAlignedUtf8Until!(bytePattern, vectorWidth)(vectorizedText);

	// What's left is the unaligned bit at the end. Just scan it in a plain
	// one-by-one fashion and we're done.
	if ( endPtr != alignedEnd )
		return scanByteOneAtATimeUntil!(bytePattern)(text[alignedEndPos .. $]);
	else
		return index!false;
}

// Implements the scanUntil function for cases where the bytePattern
// has its high bit set. The high bit being set requires a potentially
// slower process that uses one more ALU operation than 7-bit patterns.
// Of course, this process will work for patterns <0x80, but there is no
// reason to use it for those when a (most likely) more efficient process
// is available.
private pure @nogc nothrow
index scanAlignedUtf8Until(ubyte bytePattern, VectorT)(const(VectorT)[] text)
{


	...


}

/// Scans the byte-array 'text' for the first byte matching 'bytePattern',
/// then returns the index of that byte.
///
/// The "Ascii" in this function's name refers specifically to the
/// bytePattern, and not to the byte array 'text'. It is perfectly valid to
/// pass a utf-8 encoded string into the 'text' parameter (ex: a const(char)[]
/// casted to const(ubyte)[]). This function is used by scanUtf8Until
/// to implement scanning of byte patterns that are in the ASCII range (<0x80).
///
private pure @nogc nothrow
index scanAlignedAsciiUntil(ubyte bytePattern, size_t vectorWidth, VectorT)(const(VectorT)[] textBlocks)
	if ( bytePattern < 0x80 )
{
	// Early version that seemed to work at first, until I started computing what the rest of the bits were doing.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 01111111, 00001111, 00100101
	// v0 & 00100101 -> v1: ex: 00000000, 00100100, 00100001, 00000101, 00000001, 00000100, 00100000, 00100101, 00000101, 00100101 Only input bytes that have /some/ or all of pattern are non-zero (and <0x80).
	// v1 ^ 00100101 -> v2: ex: 00100101, 00000001, 00000100, 00100000, 00100100, 00100001, 00000101, 00000000, 00100000, 00000000 Only input bytes that have /some/ (NOT all) of pattern are non-zero (and <0x80).
	// v2 - 00000001 -> v3: ex: 00100100, 00000000, 00000011, 00011111, 00100011, 00100000, 00000100, 11111111, 00011111, 11111111 Set the high-bit of ONLY bytes with all zeroes (or >=0x80, but we've excluded that).
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 00000000, 10000000 Mask out all 7 lower bits in each byte, as the previous step has little control over those.
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       =0,       >0  (failed 1/10)

	// NOPE for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 01111111, 00001111, 00000001, 00000000, 00100101
	// v0 & 00100101 -> v1: ex: 00000000, 00100100, 00100001, 00000101, 00000001, 00000100, 00100000, 00100101, 00000101, 00000001, 00000000, 00100101 Only input bytes that have /some/ or all of pattern are non-zero (and <0x80).
	// v0 ^ v1       -> v2: ex: 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 00001010, 00000000, 00000000, 00000000 Only input bytes that have /some/ (NOT all) of pattern are non-zero (and <0x80).
	// v2 - 00000001 -> v3: ex: 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 00001001, 11111111, 11111111, 11111111 Set the high-bit of ONLY bytes with all zeroes (or >=0x80, but we've excluded that).
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000, 10000000 Mask out all 7 lower bits in each byte, as the previous step has little control over those.
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0,       >0  (succeeded 10/12)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 ^ 00100101 -> v1: ex: 01111111, 01011011, 01011110, 01111010, 01111110, 01111011, 01011111, 11011010, 01011010, 00101010, 00100100, 00100101, 10000000, 00000000
	// v1 & 01111111 -> v2: ex: 01111111, 01011011, 01011110, 01111010, 01111110, 01111011, 01011111, 01011010, 01011010, 00101010, 00100100, 00100101, 00000000, 00000000
	// v2 - 00000001 -> v3: ex: 01111110, 01011010, 01011101, 01111001, 01111101, 01111010, 01011110, 01011001, 01011001, 00101001, 00100011, 00100100, 11111111, 11111111
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 ^ 00100101 -> v1: ex: 01111111, 01011011, 01011110, 01111010, 01111110, 01111011, 01011111, 11011010, 01011010, 00101010, 00100100, 00100101, 10000000, 00000000
	// v2 - 00100101 -> v3: ex: 01011010, 00110110, 00111001, 01010101, 01011001, 01010110, 00111010, 10110101, 00110101, 00000001, 11111111, 00000000, 01011011, 11011011
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 + 11011010 -> v1: ex: 00110100, 01011000, 01010101, 00111001, 00110101, 00111000, 01010100, 11011001, 01011001, 11101001, 11011011, 11011010, 01111111, 11111111
	// v1 & 00100101 -> v5: ex: 00100100, 00000000, 00000101, 00100001, 00100101, 00100000, 00000100, 00000001, 00000001, 00100001, 00000001, 00000000, 00100101, 00100101
	// v5 ^ v0       -> v6: ex: 01111110, 01111110, 01111110, 01111110, 01111110, 01111110, 01111110, 01111110, 01111110, 00101110, 00000000, 00000000, 10000000, 00000000
	// Hmmmm... it selected 10100101. Which is wrong. But specific. Maybe it can be tuned...
	//
	// v0 - 00100101 -> v2: ex: 00110101, 01011001, 01010110, 00111010, 00110110, 00111001, 01010101, 11011010, 01011010, 11101010, 11011100, 11011011, 10000000, 00000000
	// v1 ^ v2       -> v3: ex: 00000001, 00000001, 00000011, 00000011, 00000011, 00000001, 00000001, 00000011, 00000011, 00000011, 00000111, 00000010, 11111111, 11111111
	// v2 & 10000000 -> v3: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 00000000, 10000000, 10000000, 10000000, 00000000, 10000000
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 - 00100100 -> v1: ex: 00110010, 01011010, 01010111, 00111011, 00110111, 00111010, 01010110, 11011011, 01011011, 11101011, 11011101, 11011100, 10000001, 00000001
	// v0 - 00100101 -> v2: ex: 00110101, 01011001, 01010110, 00111010, 00110110, 00111001, 01010101, 11011010, 01011010, 11101010, 11011100, 11011011, 10000000, 00000000
	// v1 ^ v2       -> v3: ex: 00000111, 00000011, 00000001, 00000001, 00000001, 00000011, 00000011, 00000001, 00000001, 00000001, 00000001, 00000111, 00000001, 00000001
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 - 00100101 -> v1: ex: 00110101, 01011001, 01010110, 00111010, 00110110, 00111001, 01010101, 11011010, 01011010, 11101010, 11011100, 11011011, 10000000, 00000000
	// v0 | v1       -> v4: ex: 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 11111111, 01111111, 11101111, 11011101, 11011011, 10100101, 00100101
	// v4 - 00100101 -> v6: ex: 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 11011010, 01011010, 11001010, 10111000, 10110110, 10000000, 00000000
	// v6 & 00100110 -> vC: ex: 00000010, 00000010, 00000010, 00000010, 00000010, 00000010, 00000010, 00000010, 00000010, 00000010, 00100000, 00100110, 00000000, 00000000
	// v6 & 00100101 -> vA: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00100100, 00000000, 00000000
	// v4 | v6       -> v9: ex: 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 11111111, 01111111, 11101111, 11111101, 11111111, 10100101, 00100101
	// v4 + 11011010 -> v6: ex: 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 11011001, 01011001, 11001001, 10110111, 10110101, 01111111, 11111111
	// v6 & 00100110 -> vB: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00100110, 00100101, 00100110, 00100110
	// v4 & v6       -> v7: ex: 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 01011001, 11011001, 01011001, 11001001, 10010101, 10010001, 00100101, 00100101
	// v4 + 10000001 -> v5: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 00000000, 01110000, 01011010, 01011100, 00100110, 10100110
	// v0 + 10000000 -> v2: ex: 11011010, 11111110, 11111011, 11011111, 11011011, 11011110, 11111010, 01111111, 11111111, 10001111, 10000001, 10000000, 00100101, 10100101
	// v1 ^ v2       -> v3: ex: 00000111, 00000011, 00000001, 00000001, 00000001, 00000011, 00000011, 00000001, 00000001, 00000001, 00000001, 00000111, 00000001, 00000001
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111110, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 - 00100101 -> v1: ex: 00110101, 01011001, 01010110, 00111010, 00110110, 00111001, 01010101, 11011001, 11011010, 01011010, 11101010, 11011100, 11011011, 10000000, 00000000
	// v0 | v1       -> v2: ex: 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 11111111, 11111111, 01111111, 11101111, 11011101, 11011011, 10100101, 00100101
	// v2 + 10000001 -> v3: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000, 00000000, 01110000, 01011010, 01011100, 00100110, 10100110
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0, 00000000,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0, 00000000,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

		// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 | 11011010 -> v1: ex: 11011010, 11111110, 11111011, 11011111, 11011011, 11011110, 11111010, 11111111, 11111111, 11011111, 11011011, 11011010, 11111111, 11111111
	// v0 & 00100101 -> v2: ex: 00000000, 00100100, 00100001, 00000101, 00000001, 00000100, 00100000, 00100101, 00100101, 00000101, 00000001, 00000000, 00100101, 00100101
	// v0 | 00100101 -> v3: ex: 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 01111111, 11111111, 01111111, 00101111, 00100101, 00100101, 10100101, 00100101
	// v0 & 11011010 -> v4: ex: 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 01011010, 11011010, 01011010, 00001010, 00000000, 00000000, 10000000, 00000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

	// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 - 00100101 -> v1: ex: 00110101, 01011001, 01010110, 00111010, 00110110, 00111001, 01010101, 11011010, 01011010, 11101010, 11011100, 11011011, 10000000, 00000000
	// v0 ^ 00100101 -> v1: ex: 01111111, 01011011, 01011110, 01111010, 01111110, 01111011, 01011111, 11011010, 01011010, 00101010, 00100100, 00100101, 10000000, 00000000
	// v1 | 11011010 -> v2: ex: 11111111, 11011011, 11011110, 11111010, 11111110, 11111011, 11011111, 11011010, 11011010, 11111010, 11011110, 11011011, 11011010, 11011010
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

		// Works for recognizing any byte-pattern that's <0x80.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 01011010, 01111110, 01111011, 01011111, 01011011, 01011110, 01111010, 11111111, 01111111, 00001111, 00000001, 00000000, 10100101, 00100101
	// v0 + 00100101 -> v1: ex: 01111111, 10100011, 10100000, 10000100, 01111110, 10000011, 10011111, 00100100, 10100100, 00110100, 00100110, 00100101, 11001010, 01001010
	// v0 ^ v1       -> v3: ex: 00100101, 11011101, 11011011, 11011011, 00100101, 11011101, 11100101, 11011011, 11011011, 00111011, 00100111, 00100101, 01101111, 01101111
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0,       >0  (succeeded 13/14)

		// Also NOPE if the pattern has a high-bit set.
	// Pattern to recognize: 10000101  (like '%', but with the 8th bit set instead of the 5th bit)
	// v0            -> v0: ex: 01111010, 11111110, 11111011, 01111111, 01111011, 01111110, 11111010, 11111111, 00001111, 10000101
	// v0 & 10000101 -> v1: ex: 00000000, 10000100, 10000001, 00000101, 00000001, 00000100, 10000000, 10000101, 00000101, 10000101
	// v0 ^ v1       -> v2: ex: 01111010, 01111010, 01111010, 01111010, 01111010, 01111010, 01111010, 01111010, 00001010, 00000000
	// v2 - 00000001 -> v3: ex: 01111001, 01111001, 01111001, 01111001, 01111001, 01111001, 01111001, 01111001, 00001001, 11111111
	// v3 & 10000000 -> v4: ex: 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 00000000, 10000000
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0  (succeeded 10/10)
	//
	// Might not work for input that has a high bit set when the pattern doesn't have a high bit set.
	// Pattern to recognize: 00100101  (ASCII '%')
	// v0            -> v0: ex: 11011010, 11111110, 11111011, 11011111, 11011011, 11011110, 11111010, 11111111, 10001111, 00100101
	// v0 & 00100101 -> v1: ex: 00000000, 00100100, 00100001, 00000101, 00000001, 00000100, 00100000, 00100101, 00000101, 00100101 Only input bytes that have /some/ or all of pattern are non-zero (and <0x80).
	// v0 ^ v1       -> v2: ex: 11011010, 11011010, 11011010, 11011010, 11011010, 11011010, 11011010, 11011010, 10001010, 00000000 Only input bytes that have /some/ (NOT all) of pattern are non-zero (and <0x80).
	// v2 - 00000001 -> v3: ex: 11011001, 11011001, 11011001, 11011001, 11011001, 11011001, 11011001, 11011001, 10001001, 11111111 Set the high-bit of ONLY bytes with all zeroes (or >=0x80, but we've excluded that).
	// v3 & 10000000 -> v4: ex: 10000000, 10000000, 10000000, 10000000, 10000000, 10000000, 10000000, 10000000, 10000000, 10000000 Mask out all 7 lower bits in each byte, as the previous step has little control over those.
	//                 want           =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       =0,       >0
	//                  got           >0,       >0,       >0,       >0,       >0,       >0,       >0,       >0,       >0,       >0  (failed 9/10)

	
	enum patternVec = fill!(8, bytePattern, VectorT);
	enum x01 = intVecX0001!(8, VectorT);
	enum x80 = intVec1000X!(8, VectorT);

	VectorT v0, v1, v2, v3, v4 = 0;

	size_t len = textBlocks.length;
	size_t i;
	for ( i = 0; i < len; i++ )
	{
		v0 = textBlocks[i];
		v1 = v0 & patternVec;
		v2 = v0 ^ v1;
		v3 = v2 - x01;
		v4 = v3 & x80;
		if ( v4 )
			break;
	}

	if ( i < len )
	{
		assert(v4 > 0);
		index pos = scanByteOneAtATimeUntil!(bytePattern)(
			cast(const(ubyte)[])(textBlocks[i .. i + 1]));
		if ( pos )
			return (i * vectorWidth) + pos;
	}

	return index!false;
}
+/
