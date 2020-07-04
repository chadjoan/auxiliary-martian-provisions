module amp.bits;

import std.traits;
import amp.types;

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
@nogc nothrow MultiplicationResult!(CommonType!(UIntA, UIntB))
	multiply(UIntA, UIntB)(UIntA a, UIntB b)
	if ( isIntegral!UIntA && !isSigned!UIntA
	&&   isIntegral!UIntB && !isSigned!UIntB )
{
	alias UIntT = CommonType!(UIntA, UIntB);

	static if (
		is( UIntT == uint8  ) ||
		is( UIntT == uint16 ) ||
		is( UIntT == uint32 ) )
	{
		mixin IntegerProperties!(UIntT);

		uint64 wideA = a;
		uint64 wideB = b;
		uint64 longResult = wideA * wideB;

		MultiplicationResult!UIntT  result;
		result.lo = cast(UIntT)(longResult & loMask);
		result.hi = cast(UIntT)(longResult >> halfBits);
		return result;
	}
	else
	{
		return gsMultiply(a,b);
	}
}

/// Performs grade-school multiplication of the two numbers 'a' and 'b', storing the result into
/// a wide struct (MultiplicationResult) that is capable of holding the entire result, e.g. without
/// truncating its most-significant bits.
///
/// This comes in handy whenever you need to know the result of multiplying two of the language's
/// largest integer types.
///
@nogc nothrow MultiplicationResult!(CommonType!(UIntA, UIntB))
	gsMultiply(UIntA, UIntB)(UIntA a, UIntB b)
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

	// This implements grade-school multiplication of two numbers, like so:
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
