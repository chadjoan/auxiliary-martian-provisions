/// Types that SHOULD be built-ins, but aren't.
module exogear.types;

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
	//@nogc nothrow
	index  find_first(const(ubyte)[] haystack, ubyte needle) {
		foreach( size_t i, ubyte byteN; haystack )
			if ( byteN == needle )
				return index(i);
		return index(false);
	}

	immutable(ubyte)[] hello = [0x48,0x65,0x6c,0x6c,0x6f,0x21];
	immutable(ubyte)[] no    = [0x6e,0x6f];

	ubyte aitch = 0x48;
	ubyte ell   = 0x6c;
	ubyte en    = 0x6e;
	ubyte oh    = 0x6f;

	// Normal index return that could also be done with `size_t` or `ptrdiff_t`.
	// Output in the affirmative evaluates to true.
	assert(find_first(hello, ell));
	assert(find_first(hello, ell) == 2);
	assert(find_first(no, oh) == 1);

	// Ability to return 0 on success.
	assert(find_first(hello, aitch) == 0);
	assert(find_first(no, en) == 0);

	// Ability to return 0 on success AND...
	// output in the affirmative evaluates to true.
	assert(find_first(hello, aitch));
	assert(find_first(no, en));

	// Ability to return negatory status.
	assert(!find_first(hello, en));
	assert(!find_first(no, ell));
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
	bool  YESSS = yep.to!bool;
	assert(yep == true);
	assert(YESSS == true);

	bool  nope = false;
	index nah  = nope;
	bool  NOOO = nah.to!bool;
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
	static pure @nogc nothrow ptrdiff_t from_bool(bool v)
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

public pure @nogc nothrow:
	/// The index type is implemented as a single ptrdiff_t integer.
	///
	/// This initializes to ptrdiff_t.min (== index.min), which is
	/// what gives the index type's default value its equivalence to false.
	///
	ptrdiff_t impl = ptrdiff_t.min;

	/// Except for the exceptions documented on this type, it will behave
	/// just like a ptrdiff_t.
	alias impl this;

	// We need to redefine these because the `alias impl this` will cause
	// them to behave as ptrdiff_t types, which leads to surprising behavior
	// that would fail the `index.min` unittest below.
	//
	// More generally, we expect the expressions `index.min` and `index.max`
	// to return an instance of the `index` type, not an instance of `ptrdiff_t`.
	//
	enum min = index(ptrdiff_t.min);
	enum max = index(ptrdiff_t.max);

	unittest
	{
		static assert(is(typeof(index.min)  == index));
		static assert(is(typeof(index.max)  == index));
		static assert(is(typeof(index.init) == index));
		assert(!index.min);
		assert( index.max);
		assert(!index.init);
	}

	import std.traits : isIntegral;

	/// This constructor allows index types to be initialized using
	/// integer types, ex: `index foo = 1;`.
	///
	/// It also allows explicit construction of course, ex: `index(1)`.
	this(T)(T other)
		if ( isIntegral!T && !is(T == bool) && T.sizeof <= impl.sizeof )
	{
		this.impl = other;
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
	this(bool other)()
	{
		enum ptrdiff_t precompute = from_bool(other);
		this.impl = precompute;
	}

	/// ditto
	this(T)(T other)
		if ( is(T == bool) )
	{
		this.impl = from_bool(other);
	}

	/+
	/// This operator overload forces all conversions to integers to be
	/// explicitly casted, with an exception for ptrdiff_t.
	T opCast(T)() const
		if ( isIntegral!T && !is(T == ptrdiff_t) )
	{
		return cast(T)this.impl;
	}
	+/
	// TODO: I don't think the above opCast does what I thought it did.
	// In other words, this static assert should fail:
	static assert( __traits(compiles, { size_t x = index(5); }) );
	// (but it doesn't).
	// So for now, we'll just have a normal cast operator with no special claims.

	///
	T opCast(T)() const
		if ( isIntegral!T )
	{
		return cast(T)this.impl;
	}

	/// This operator overload allows assignment of integer types into the
	/// index type, ex: 'index foo; foo = 1;".
	void opAssign(T)(T other)
		if ( isIntegral!T && !is(T == bool) && T.sizeof <= impl.sizeof )
	{
		this.impl = other;
	}

	void opAssign(T)(T other)
		if ( is(T == bool) )
	{
		this.impl = from_bool(other);
	}

	version(none)
	{
		// DON'T DO THIS.
		void opAssign(bool other)
		{
			this.impl = from_bool(other);
		}
		// This will take precedence over the templated version of the
		// function (ex: the one for /integers/).
		// This will then cause bizarre things to happen, like this:
		//   index  foo;
		//   foo = 0;
		//   assert(foo); // Shouldn't assert. Does assert.
		//
		// Such things happen because 0 is a boolean value.
		// Now, D would rather treat 0 as an 'int' than as a 'bool', but
		// template precedence logic will override that notion.
		// Be careful!

		// ( Similarly, I'm going to forbid constructors of this form: )
		this(bool other)
		{
			this.impl = from_bool(other);
		}
	}

	/// This operator overload implements equality between index instances.
	bool opEquals(const index other) const
	{
		return (this.impl == other.impl);
	}

	/// This operator overload implements equality between indices and integers.
	bool opEquals(T)(auto ref T other) const
		if ( isIntegral!T )
	{
		return (this.impl == other);
	}

	/// This operator overload implements part of the index type's boolean equivalency.
	bool opEquals(T : bool)(auto ref T other) const
	{
		return (this.opCast!bool == other);
	}
/+
	int opCmp(const index other) const
	{
		return this.opCmp(other.impl);
	}

	int opCmp(T)(const T other) const
		if ( isIntegral!T )
	{
		if ( this.impl < other )
			return -1;
		else
			return !!(this.impl - other); // 0 if equal, 1 if this.impl > other.
	}

    index opUnary(string op)()
    {
        mixin(op~" this.impl;");
        return this;
    }

	index opOpAssign(string op, T)(const T rhs)
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		mixin("this.impl "~op~"= rhs;");
		return this;
	}

	index opBinary(string op, T)(const T rhs) const
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		return index(mixin("this.impl "~op~" rhs"));
	}

	index opBinaryRight(string op, T)(const T lhs) const
		if ( isIntegral!T && T.sizeof <= impl.sizeof )
	{
		return index(mixin("lhs "~op~" this.impl"));
	}
+/
	/// This operator overload implements the index type's boolean equivalency
	/// in boolean expression contexts.
	bool opCast(T : bool)() const
	{
		return (this.impl != ptrdiff_t.min);
	}

	/// Explicit orderly conversion to supported integer types.
	size_t to(T : size_t)() const
		{ return cast(size_t)this.impl; }

	/// ditto
	ptrdiff_t to(T : ptrdiff_t)() const
		{ return cast(ptrdiff_t)this.impl; }

	/// ditto
	bool to(T : bool)() const
		{ return this.opCast!bool; }
}




unittest
{
	index  find_first(string text_body, dchar to_find)
	{
		foreach( i, ch; text_body )
			if ( ch == to_find )
				return index(i);

		return index(false);  // <--  o.O
		// (And yes, that will actually do The Right Thing.)
	}

	index  idx;

	// This case is just like when `size_t` or `ptrdiff_t` is used
	// as a return value: the 0th element is 'h', and the 1st is 'a',
	// so find_first returns 1 to indicate it matched 'a' at position 1.
	idx = find_first("haystack", 'a');
	assert(idx == 1);

	// We usually want to be able to return the 0th element too.
	// That's pretty normal. But...
	idx = find_first("haystack", 'h');
	assert(idx == 0);

	// What about the negative case?
	idx = find_first("haystack", 'z');

	// `idx` can't be 0. That would indicate that there's a 'z' at the
	// start of "haystack", and there clearly is not.
	assert(idx != 0);

	// But we want it to have `false` equivalence. Does it?
	assert(idx == false); // You bet it does.

	// Let's use it a little differently and see if it's still OK.
	// This is like that earlier example with `find_first_v0`.
	// The `assert(0)` should never be reached, because the expression
	// `find_first("haystack", 'z')` should evaluate as boolean `false`.
	if ( find_first("haystack", 'z') )
		assert(0); // Passes. It's still OK!

	// Let's revisit the 0th match case.
	// Earlier we asserted that this returns 0.
	// But does it also return `true`?
	assert(find_first("haystack", 'h')); // Yes. Yes it does.

	// The good doctrine of "Full Power! Total Destruction!" dictates
	// that we have to attempt destroying our index in at least one
	// more way: have it represent negative values.
	// This happens in a number of situations, including when we need
	// a natural way to represent how far an element is from the end of
	// a string or an array, or when we need to calculate the offset
	// between two substrings in a larger string.

	// So let's try negatives:
	idx = find_first("haystack", 'y');
	idx = idx - "haystack".length;

	// Is it negative?
	assert(idx < 0); // Good.
	assert(idx == -6); // This should also be true.

	// Since `-6` is a valid offset, we don't expect it to be `false`.
	assert(idx == true);
	assert(idx != false);

	// This is consistent with constructs such as
	//   python's slicing operator:
	//
	string pyslice(string s, ptrdiff_t i1, ptrdiff_t i2)
	{
		index _i1 = i1;
		index _i2 = i2;
		if ( _i1 < 0 )
			_i1 += s.length;
		if ( _i2 < 0 )
			_i2 += s.length;
		return s[_i1 .. _i2];
	}

	assert(pyslice("haystack", -8,-5) == "hay");
	assert(pyslice("haystack", -6,-1) == "ystac");

	// So those should be true:
	assert(index(-8));
	assert(index(-6));
	assert(index(-5));
	assert(index(-1));
	// ... and they are.

	// There's still another thing to check.

	// An obvious, but suboptimal, way to implement the `index` type would be
	// to make it a struct with a `ptrdiff_t` in it, but also with another
	// boolean in it to convey its existential nature. This is suboptimal
	// because every `index` variable in the program will now occupy more
	// memory, possibly even twice as much in cases where struct and variable
	// padding/alignment dictate that a lone `bool` occupy an entire 4 or 8 bytes.
	// That waste is then multiplied by the number of times such variables appear.
	// It's not something that everyone cares about, but it would just feel
	// *wrong*, OK?

	// Well, let's exclude that possibility.
	assert(ptrdiff_t.sizeof == index.sizeof); // No waste!

	// That `false` value is hiding somewhere in the `index` type's
	// number line, but we'll get to that later. More importantly, we've
	// already seen that it's likely to be somewhere far away from the ranges
	// of values that we care about using for valid indices into arrays.
}
