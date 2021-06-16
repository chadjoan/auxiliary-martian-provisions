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

@safe:

/// This is an integer primitive similar to `ptrdiff_t`, but with a different
/// boolean equivalency that makes it advantageous as an array index,
/// especially when used as a return value from a function or expression
/// that needs to be able to return 0 as a valid index and yet still be able
/// to return a value equivalent to boolean false to signal non-existance
/// (or other kinds of non-exceptional failure).
///
/// -----
unittest
{
	index findTheMagic(const ubyte[] dump, ubyte magic) {
		foreach( size_t i, ubyte byteN; dump )
			if ( byteN == magic )
				return i;
		return false;
	}

	immutable ubyte[] hello = [0x48,0x65,0x6c,0x6c,0x6f,0x21];
	immutable ubyte[] no    = [0x6e,0x6f];

	ubyte aitch = 0x48;
	ubyte ell   = 0x6c;
	ubyte en    = 0x6e;
	ubyte oh    = 0x6f;

	// Normal index return that could also be done with `size_t` or `ptrdiff_t`.
	// Output in the affirmative evaluates to true.
	assert(hello.findTheMagic(ell));
	assert(hello.findTheMagic(ell) == 2);
	assert(no.findTheMagic(oh) == 1);

	// Ability to return 0 on success.
	assert(hello.findTheMagic(aitch) == 0);
	assert(no.findTheMagic(en) == 0);

	// Ability to return 0 on success AND...
	// output in the affirmative evaluates to true.
	assert(hello.findTheMagic(aitch));
	assert(no.findTheMagic(en));

	// Ability to return negatory status.
	assert(!hello.findTheMagic(en));
	assert(!no.findTheMagic(ell));
}
/// -----
///
/// Most integer types are considered to be equivalent to false when they
/// are equal to 0. The index type, on the other hand, is considered to be
/// equivalent to false when it is equal to index.min (which is equal to
/// `ptrdiff_t.min`).
/// All other index values, including 0, are considered equivalent to true.
///
/// -----
unittest
{
	index foo = false;
	assert(foo == index.min);
	assert(foo == ptrdiff_t.min);

	index bar;
	bar = 0;         assert(bar);
	bar = 1;         assert(bar);
	bar = -1;        assert(bar);
	bar = index.max; assert(bar);
}
/// -----
///
/// Boolean values can be written into index variables, just as they can be
/// written into variables typed as more common integer types. However, the
/// index type differs from other integer types in terms of what boolean
/// values translate into:
/// - A boolean value of true is translated into an index value of 0.
///     By contrast, other integer types typically acquire a value of 1.
/// - A boolean value of false is translated into an index value of index.min.
///     By contrast, other integer types typically acquire a value of 0.
///
/// -----
unittest
{
	index yep = true;
	assert(yep == 0);

	index nope = false;
	assert(nope == index.min);
}
/// -----
///
/// The above divergence from typical boolean-to-integer conversion is
/// complementary to the index type's divergence from typical integer-to-boolean
/// conversion, and allows boolean values to round-trip through an index value:
///
/// -----
unittest
{
	bool  yeah  = true;
	index yep   = yeah;
	bool  YESSS = yep;
	assert(yep == true);
	assert(YESSS == true);

	bool  nope = false;
	index nah  = nope;
	bool  NOOO = nah;
	assert(nah == false);
	assert(NOOO == false);
}
/// -----
///
/// The choice of index.min as a false-equivalent, as opposed to, for instance,
/// all negative values, allows index variables to be used to store differences
/// between indices or store negative offsets (ex: in parameters that use
/// negative values to indicate the number of elements from the end of a list).
///
/// By default, this is initialized to a false value: specifically `index.min`.
struct index
{
private:
	static @nogc nothrow ptrdiff_t fromBool(bool v)
	{
		// We desired these properties:
		// `bool{true == 1}  => index{true == 0}`
		// `bool{false == 0} => index{false == index.min}`
		//
		// This can be accomplished by subtracting one and shifting away the
		// excess set bits. 
		//
		// Example for true, if ptrdiff_t were a 16-bit value:
		//   0x0001 (boolean true)
		//   0x0000 (subtract 1)
		//   0x0000 (shift left 15)
		//
		// Example for false, if ptrdiff_t were a 16-bit value:
		//   0x0000 (boolean false)
		//   0xFFFF (subtract 1)
		//   0x8000 (shift left 15)
		//
		enum shiftBy = (ptrdiff_t.sizeof * 8) - 1;
		return ((cast(ptrdiff_t)v) - 1) << shiftBy;
	}

public:
	/// The index type is implemented as a single ptrdiff_t integer.
	///
	/// This initializes to ptrdiff_t.min (== index.min), which is
	/// what gives the index type's default value its equivalence to false.
	///
	ptrdiff_t impl = ptrdiff_t.min;

	/// Except for the exceptions documented on this type, it will behave
	/// just like a ptrdiff_t.
	alias impl this;

	import std.traits : isIntegral;

	/// This constructor allows index types to be initialized using
	/// integer types, ex: `index foo = 1;`.
	@nogc nothrow this(T)(T other)
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		impl = other;
	}

	/// These constructor allows index types to be initialized using boolean
	/// values. An input of true will result in an index of 0, while an input
	/// of false will result in an index of `index.min` (which is equal to
	/// `ptrdiff_t.min`).
	///
	/// This is mostly motivated by the desire to be able to return false from
	/// index-returning functions, and thus make the intent explicit.
	///
	/// There might not be any reason to ever assign true to an index, but
	/// implementing the one implies implementing the other.
	@nogc nothrow this(bool other)()
	{
		enum ptrdiff_t precompute = fromBool(other);
		impl = precompute;
	}

	/// ditto
	@nogc nothrow this(bool other)
	{
		impl = fromBool(other);
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

	/// This operator overload allows assignment of integer types into the
	/// index type, ex: 'index foo; foo = 1;".
	@nogc nothrow void opAssign(bool other)
	{
		impl = fromBool(other);
	}

	/// This operator overload implements the index type's boolean equivalency.
	@nogc nothrow bool opCast(T : bool)() const
	{
		return (impl != ptrdiff_t.min);
	}

	/// Explicit orderly conversion to supported integer types.
	@nogc nothrow size_t to(T : size_t)() const
		{ return cast(size_t)impl; }

	/// ditto
	@nogc nothrow ptrdiff_t to(T : ptrdiff_t)() const
		{ return cast(ptrdiff_t)impl; }
}

