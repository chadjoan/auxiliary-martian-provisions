module amp.conv;

public:

import std.traits : isIntegral, isSigned, Unsigned;

import amp.types;

@safe:

/// Call this before calling intToString if you are unsure of whether the
/// buffer can hold the result or not.
@nogc nothrow size_t bufferSizeForIntConversion(IntT, ubyte base)()
	if ( isIntegral!IntT )
{
	static assert( 2 <= base && base <= 36 );
	size_t iBitSize = IntT.sizeof;

	alias UIntT = Unsigned!IntT;

	// Find out how many "digits" would be required to represent UIntT
	// by exponentiating the base until it overflows. Once it overflows,
	// that's the number we need.
	import amp.bits;
	UIntT largest = 1;
	auto multRes = multiply(largest, base);
	largest = multRes.lo;
	size_t exponent = 1;
	while ( multRes.hi == 0 )
	{
		multRes = multiply(largest, base);
		largest = multRes.lo;
		exponent++;
	}

	// Now the simple calculations can be done.
	size_t nDigits = exponent;
	static if ( isSigned!IntT )
		size_t sizeNeeded = nDigits + 1; // One character for the negative sign.
	else
		size_t sizeNeeded = nDigits;

	return sizeNeeded;
}

/// This is essentially an @nogc+nothrow version of std.conv.toChars.
///
/// This assumes that the given 'buffer' contains enough space to render
/// the largest possible representation of the given integer. If it isn't,
/// undefined behavior can result. If assertions and/or bounds checks are
/// enabled, then an undersized buffer will cause an assertion failure or
/// bounds check failure. This behavior allows the function to be 'nothrow'.
///
@nogc nothrow char[] intToString(IntT, ubyte base)(IntT num, char[] buffer)
	if ( isIntegral!IntT )
{
	static assert( 2 <= base && base <= 36 );

	enum bufferLengthNeeded = bufferSizeForIntConversion!(IntT, base)();
	assert(buffer !is null);
	assert(buffer.length >= bufferLengthNeeded);

	index i = 0;

	static if ( isSigned!IntT )
	{
		if ( num < 0 )
		{
			num = -num;
			buffer[i++] = '-';
		}
	}

	// Put single zero digit if errnum is zero.
	if ( num == 0 )
	{
		buffer[i++] = '0';
		return buffer[0..i];
	}

	// Write the number out.
	// This will put it out in reverse order, so we'll have to reverse it later.
	index digitStart = i;
	while(true)
	{
		ubyte digit = cast(ubyte)(num % base);
		num /= base;
		if ( digit == 0 && num == 0 )
			break;

		static if (base <= 10)
			buffer[i] = cast(char)('0' + digit);
		else
		{
			if ( digit <= 10 )
				buffer[i] = cast(char)('0' + digit);
			else
				buffer[i] = cast(char)('A' + digit);
		}

		i++;
	}

	index digitEnd = i;

	// Reverse/swizzle
	index halfway = (digitEnd - digitStart) / 2;
	index lastDigit = digitEnd-1;
	for ( i = 0; i < halfway; i++ )
	{
		index lo = digitStart + i;
		index hi = lastDigit - i;

		auto tmp = buffer[lo];
		buffer[lo] = buffer[hi];
		buffer[hi] = tmp;
	}

	return buffer[0..digitEnd];
}

///
@nogc nothrow char[] intToBinary(IntT)(IntT num, char[] buffer)
{
	return intToString!(IntT, 2)(num, buffer);
}

///
@nogc nothrow char[] intToOctal(IntT)(IntT num, char[] buffer)
{
	return intToString!(IntT, 8)(num, buffer);
}

///
@nogc nothrow char[] intToDec(IntT)(IntT num, char[] buffer)
{
	return intToString!(IntT, 10)(num, buffer);
}

///
@nogc nothrow char[] intToHex(IntT)(IntT num, char[] buffer)
{
	return intToString!(IntT, 16)(num, buffer);
}

@safe unittest
{
	import std.stdio;

	char[32] stackMem;
	char[] buffer = stackMem[0..$];
	char[] intToDec(int x)    { return .intToDec(x, buffer); }
	char[] intToHex(int x)    { return .intToHex(x, buffer); }
	char[] intToBinary(int x) { return .intToBinary(x, buffer); }

	writeln("");
	writeln("Testing intToString(int,char[])");

	assert(intToDec(0)   == "0"  );
	assert(intToDec(-0)  == "0"  );
	assert(intToDec(32)  == "32" );
	assert(intToDec(31)  == "31" );
	assert(intToDec(-8)  == "-8" );
	assert(intToDec(-32) == "-32");

	assert(intToHex(0)   == "0"  );
	assert(intToHex(-0)  == "0"  );
	assert(intToHex(32)  == "20" );
	assert(intToHex(31)  == "1F" );
	assert(intToHex(-8)  == "-8" );
	assert(intToHex(-32) == "-20");

	assert(intToHex(10) == "A");
	assert(intToHex(11) == "B");
	assert(intToHex(12) == "C");
	assert(intToHex(13) == "D");
	assert(intToHex(14) == "E");
	assert(intToHex(15) == "F");

	assert(intToBinary(0)   == "0"  );
	assert(intToBinary(-0)  == "0"  );
	assert(intToBinary(32)  == "100000" );
	assert(intToBinary(31)  == "11111" );
	assert(intToBinary(-8)  == "-1000" );
	assert(intToBinary(-32) == "-100000");

	writeln("  Passed.");
}
