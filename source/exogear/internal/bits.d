module exogear.bits;

import std.traits;
import exogear.types;

/// Collection of useful constants for an integer type.
template IntegerProperties(IntT)
	if ( isIntegral!IntT )
{
public:
	enum totalBits    = IntT.sizeof * 8;     /// The number of bits in the integer.
	enum halfBits     = totalBits / 2;       /// The number of bits in the integer, divided by 2.
	enum quarterBits  = totalBits / 4;       /// The number of bits in the integer, divided by 4.
	enum eighthBits   = totalBits / 8;       /// The number of bits in the integer, divided by 8.
	enum mask00001111 = (1UL << halfBits)-1; /// Lower half of bits are set (1), upper half are clear (0).
	alias loMask      = mask00001111;        /// ditto
	enum mask11110000 = ~mask00001111;       /// Lower half of bits are clear (0), upper half are set (1).
	alias hiMask      = mask11110000;        /// ditto
	enum mask00000011 = (1UL << quarterBits)-1; /// Lower quarter of bits are set (1), upper three quarters are clear (0).
	enum mask00110011 =  mask00000011 | (mask00000011 << halfBits); // Every quarter of bits are alternately clear and set.
	enum mask11111100 = ~mask00000011;       /// Lower quarter of bits are clear (0), upper three quarters are set (1).
	enum mask11001100 = ~mask00110011;       /// Every quarter of bits are alternately set and clear.
	enum mask00000001 = (1UL << eighthBits)-1; ///
	enum mask00000101 =  mask00000001 | (mask00000001 << quarterBits); ///
	enum mask00010101 =  mask00000101 | (mask00000101 << quarterBits); ///
	enum mask01010101 =  mask00000101 | (mask00000101 << halfBits); ///
	enum mask11111110 = ~mask00000001; ///
	enum mask11111010 = ~mask00000101; ///
	enum mask11101010 = ~mask00010101; ///
	enum mask10101010 = ~mask01010101; ///
}

enum largerIntTypeExists(IntT) =
	is(IntT == int8)  || is(IntT == int16)  || is(IntT == int32) ||
	is(IntT == uint8) || is(IntT == uint16) || is(IntT == uint32);

template DoubleIntType(IntT)
	if ( largerIntTypeExists!IntT )
{
	static if ( is( IntT == uint32 ) )
		alias DoubleIntType = uint64;
	else
	static if ( is( IntT == int32 ) )
		alias DoubleIntType = int64;
	else
	static if ( is( IntT == uint16 ) )
		alias DoubleIntType = uint32;
	else
	static if ( is( IntT == int16 ) )
		alias DoubleIntType = int32;
	else
	static if ( is( IntT == uint8 ) )
		alias DoubleIntType = uint16;
	else
	static if ( is( IntT == int8 ) )
		alias DoubleIntType = int16;
	else
		static assert(0, "Unexpected integer type: "~IntT.stringof);
}

template DoubleOrGreaterIntType(IntT)
	if ( largerIntTypeExists!IntT )
{
	static if ( isSigned!IntT )
	{
		static if ( IntT.sizeof * 2 <= ptrdiff_t.sizeof )
			alias DoubleOrGreaterIntType = ptrdiff_t;
		else
			alias DoubleOrGreaterIntType = int64;
	}
	else
	{
		static if ( IntT.sizeof * 2 <= size_t.sizeof )
			alias DoubleOrGreaterIntType = size_t;
		else
			alias DoubleOrGreaterIntType = uint64;
	}
}

/// Holds the result of adding or subtracting two of the given integer type,
/// along with a carry bit to indicate underflow or overflow.
struct AddOrSubResult(UIntT)
{
	UIntT sum;
	bool  carry;
}

///
pure @nogc nothrow AddOrSubResult!(CommonType!(IntA, IntB))
	addWithCarry(IntA, IntB)(IntA a, IntB b, bool carryIn)
	if ( isIntegral!IntA && isIntegral!IntB )
{
	alias IntT = CommonType!(IntA, IntB);
	AddOrSubResult!IntT  result;

	static if ( largerIntTypeExists!IntT )
	{
		// Delegate the problem to the larger int type.
		alias WideIntT = DoubleOrGreaterIntType!IntT;
		WideIntT wideA = a;
		WideIntT wideB = b;
		WideIntT wideC = carry;
		WideIntT wideResult = wideA + wideB + wideC;

		enum intSizeInBits = IntT.sizeof * 8;
		result.sum   = cast(IntT)wideResult; // Discard overflow bit.
		result.carry = cast(bool)(wideResult >> intSizeInBits); // Single out the overflow bit.
	}
	else
	{
		// Largest int type in the language: this forces us to explicitly look
		// for overflow conditions.
		with(result)
		{
			sum = a;
			carry = false;

			carry |= sum > IntT.max - carryIn; 
			sum += carryIn;

			carry |= sum > IntT.max - b;
			sum += b;
		}
	}
	return result;
}

///
pure @nogc nothrow AddOrSubResult!(CommonType!(IntA, IntB))
	addWithCarryOut(IntA, IntB)(IntA a, IntB b)
	if ( isIntegral!IntA && isIntegral!IntB )
{
	alias IntT = CommonType!(IntA, IntB);

	AddOrSubResult!IntT  result;
	with(result)
	{
		carry = a > IntT.max - b;
		sum = a + b;
	}
	return result;
}

///
pure @nogc nothrow CommonType!(IntA, IntB)
	addWithCarryIn(IntA, IntB)(IntA a, IntB b, bool carryIn)
	if ( isIntegral!IntA && isIntegral!IntB )
{
	return a + b + cast(IntB)carryIn;
}

/+

struct uint128
{
	alias ThisT = typeof(this);
	uint64 lo;
	uint64 hi;

	pure @nogc nothrow
	ThisT opOpAssign(string op : "+", OtherT : ThisT)(OtherT other)
	{
		bool carry = this.lo > uint64.max - other.lo;
		this.lo += other.lo;
		this.hi += other.hi;
		this.hi += cast(uint64)carry;
		return this;
	}

	pure @nogc nothrow
	ThisT opOpAssign(string op : "-", OtherT : ThisT)(OtherT other)
	{
		bool borrow = this.lo < other.lo; // uint64's are guaranteed to be >= 0.
		this.lo -= other.lo;
		this.hi -= other.hi;
		this.hi -= cast(uint64)borrow;
		return this;
	}

	pure const @nogc nothrow
	ThisT opBinary(string op : "+", OtherT : ThisT)(OtherT other)
	{
		ucent newb;
		bool carry = this.lo > uint64.max - other.lo;
		newb.lo = this.lo + other.lo;
		newb.hi = this.hi + other.hi;
		newb.hi += cast(uint64)carry;
		return newb;
	}

	pure const @nogc nothrow
	ThisT opBinary(string op : "-", OtherT : ThisT)(OtherT other)
	{
		ucent newb;
		bool borrow = this.lo < other.lo; // uint64's are guaranteed to be >= 0.
		newb.lo = this.lo - other.lo;
		newb.hi = this.hi - other.hi;
		newb.hi -= cast(uint64)borrow;
		return newb;
	}
}

// And then for the signed version, just cast the operands to uint128's and
// perform the adds and subtracts, then cast back. The result will be the
// same according to two's complement arithmetic.
// It might even be worthwhile to just implement int128 as two uint64's, then
// do sign bit checking where appropriate (thankfully, only the hi integer has
// a sign bit).

+/


/// Holds the results of long-multiplication routines.
struct MultiplicationResult(UIntT)
{
	UIntT lo; /// The lower half of the multiplication result.
	UIntT hi; /// The upper half of the multiplication result.
}

/// Multiplies the two numbers 'a' and 'b', storing the result into a wide
/// struct (MultiplicationResult) that is capable of holding the entire result,
/// e.g. without truncating its most-significant bits.
///
/// This comes in handy whenever some code handling generic integers might
/// produce a multiplication result that exceeds the size of the integers,
/// and the full result is important.
///
/// If the multiplication result cannot fit into the language's largest integer
/// type, then grade-school multiplication is performed (see gsMultiply).
/// If the multiplication result can fit into the language's largest integer
/// type, then that integer type may be used internally to perform the
/// calculation more rapidly.
///
pure @nogc nothrow MultiplicationResult!(CommonType!(UIntA, UIntB))
	multiply(UIntA, UIntB)(UIntA a, UIntB b)
	if ( isIntegral!UIntA && !isSigned!UIntA
	&&   isIntegral!UIntB && !isSigned!UIntB )
{
	alias UIntT = CommonType!(UIntA, UIntB);

	static if ( largerIntTypeExists!UIntT )
	{
		// Attempt to use the most comfortable (for the CPU/compiler)
		// integer type available for the calculation.
		alias WideUIntT = DoubleOrGreaterIntType!UIntT;

		WideUIntT wideA = a;
		WideUIntT wideB = b;
		WideUIntT wideResult = wideA * wideB;

		// Use DoubleUIntT instead of WideIntT to guarantee that the result
		// is split at the correct location; e.g. the bit count of `loMask` must
		// equal the bit count of a `UIntT`.
		alias DoubleUIntT = DoubleIntType!UIntT;
		mixin IntegerProperties!(DoubleUIntT);

		MultiplicationResult!UIntT  result;
		result.lo = cast(UIntT)(wideResult & loMask);
		result.hi = cast(UIntT)(wideResult >> halfBits);
		return result;
	}
	else
	{
		return schoolMultiply(a,b);
	}
}

/// Performs schoolbook multiplication of the two numbers 'a' and 'b', storing the result into
/// a wide struct (MultiplicationResult) that is capable of holding the entire result, e.g. without
/// truncating its most-significant bits.
///
/// For most situations, you will want to use the `multiply` function instead.
/// That function will use the underlying multiplication method that is most
/// likely to be the fastest (within this module), which may or may not be
/// `schoolMultiply`.
///
/// This comes in handy whenever you need to know the result of multiplying two of the language's
/// largest integer types.
///
/// Other integer types are supported, but it will likely be more efficient to
/// perform the multiplication by casting the operands into the next larger
/// integer type and then doing a single multiplication, hence the recommendation
/// to use the `multiply` function instead of this one for most cases.
///
pure @nogc nothrow MultiplicationResult!(CommonType!(UIntA, UIntB))
	schoolMultiply(UIntA, UIntB)(UIntA a, UIntB b)
	if ( isIntegral!UIntA && !isSigned!UIntA
	&&   isIntegral!UIntB && !isSigned!UIntB )
{
	alias UIntT = CommonType!(UIntA, UIntB);

	alias IntProps = IntegerProperties!(UIntT);
	alias shift  = IntProps.halfShift;
	alias loMask = IntProps.loMask;
	alias hiMask = IntProps.hiMask;

	UIntT halfMult(UIntT a, UIntT b)
	{
		UIntT result = a*b;
		//carry += !!(result & hiMask);
		return result;
	}

	// This implements schoolbook multiplication of two numbers, like so:
	//
	//               a1   a2
	//            x  b1   b2
	// ----------------------
	//              H22  L22
	//         H12  L12
	//         H21  L21
	// +  H11  L11
	// ----------------------
	//     R3   R2   R1   R0
	//
	//  result = R3R2R1R0
	//    result.lo = R1R0
	//    result.hi = R3R2

	UIntT  a1, a2;
	UIntT  b1, b2;

	a1 = a >> shift;
	a2 = a & loMask;
	b1 = b >> shift;
	b2 = b & loMask;

	UIntT m22, H22, L22;
	UIntT m12, H12, L12;
	UIntT m21, H21, L21;
	UIntT m11, H11, L11;

	m22 = a2 * b2;
	m12 = a1 * b2;
	m21 = a2 * b1;
	m11 = a1 * b1;

	H22 = m22 >> shift;
	L22 = m22 & loMask;
	H12 = m12 >> shift;
	L12 = m12 & loMask;
	H21 = m21 >> shift;
	L21 = m21 & loMask;
	H11 = m11 >> shift;
	L11 = m11 & loMask;

	UIntT R0   = L22;
	UIntT R1wC = H22 + L12 + L21;
	UIntT R1   = R1wC & loMask;
	UIntT R2wC = H12 + H21 + L11 + (R1wC >> shift);
	UIntT R2   = R2wC & loMask;
	UIntT R3   = H11 + (R2wC >> shift);

	// This should not be mathematically possible, because ((2^n)-1)*((2^n)-1) < (2^n)*(2^n).
	assert(0 == (R3 & hiMask));

	MultiplicationResult!UIntT  result;
	result.lo = R0 | (R1 << shift);
	result.hi = R2 | (R3 << shift);
	return result;
}
