/// Types that SHOULD be built-ins, but aren't.
module amp.types;

alias uint8  = ubyte;  /// 8-bit unsigned integer. D's ubyte.
alias uint16 = ushort; /// 16-bit unsigned integer. D's ushort.
alias uint32 = uint;   /// 32-bit unsigned integer. D's uint;
alias uint64 = ulong;  /// 64-bit unsigned integer. D's ulong;
//alias uint128 = ucent; // someday...

alias int8   = byte;  /// 8-bit signed integer. D's 'byte' type.
alias int16  = short; /// 16-bit signed integer. D's 'short' type.
alias int32  = int;   /// 32-bit signed integer. D's 'int' type.
alias int64  = long;  /// 64-bit signed integer. D's 'long' type.
//alias int128 = cent;

/// This is an integer primitive similar to ptrdiff_t, but with a different
/// boolean equivalency that makes it advantageous as an array index,
/// especially when used as a return value from a function or expression
/// that needs to be able to return both 0 as a valid index and also
/// signal non-existance (or other kinds of non-exceptional failure).
///
/// Specifically, any positive index value, including zero, is considered
/// to be a "truth" value.
/// Any negative index value, NOT including zero, is considered a "false"
/// value.
///
/// In cases where negative values might be valid indexes (ex: to indicate
/// distance from the end of an array), use ptrdiff_t instead.
struct index
{
public:
	ptrdiff_t impl;
	alias impl this;

	import std.traits : isIntegral;

	/// This constructor allows index types to be initialized using
	/// integer types, ex: 'index foo = 1;".
	this(T)(T other)
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		impl = other;
	}

	/// This operator overload forces all conversions to integers to be
	/// explicitly casted, with an exception for ptrdiff_t.
	@nogc nothrow T opCast(T)() const
		if ( isIntegral!T && !is(T == ptrdiff_t) )
	{
		return cast(T)impl;
	}

	/// This operator overload allows assignment of integer types into the
	/// index type, ex: 'index foo; foo = 1;".
	@nogc nothrow void opAssign(T)(T other)
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		impl = other;
	}

	/// This operator overload implements the index type's boolean equivalency.
	@nogc nothrow bool opCast(T : bool)() const
	{
		return (impl >= 0);
	}

	/// Explicit orderly conversion to supported integer types.
	@nogc nothrow size_t to(T : size_t)() const
		{ return cast(size_t)impl; }

	/// ditto
	@nogc nothrow ptrdiff_t to(T : ptrdiff_t)() const
		{ return cast(ptrdiff_t)impl; }
}

