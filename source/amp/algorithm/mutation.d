module amp.algorithm.mutation;

import std.algorithm.mutation;

public @nogc nothrow
size_t truncatingCopy
	(SourceArrayT, DestArrayT)
	(SourceArrayT src, DestArrayT dst)
	if( areCopyCompatibleArrays!(SourceArrayT,DestArrayT) )
{
	// Although this is a copying function, the first thing to do is to
	// attempt to complete the copy operation without copying anything.
	// That is the fastest possible outcome.
	if ( src is null || src.length == 0
	||   dst is null || dst.length == 0 )
	{
		// The simplest case: no copying or slice management needed.
		// Just leave the destination array unchanged and signal zero truncations.
		return 0;
	}

	// Ensure that we don't overflow; calculate the copy size.
	size_t nToCopy = src.length;
	ptrdiff_t nTruncated = nToCopy - dst.length;
	if ( nTruncated > 0 )
		nToCopy = dst.length;
	else
		nTruncated = 0;

	// Another attempt at getting out of work.
	if ( dst.ptr is src.ptr )
	{
		// The almost-simplest case. Maybe the caller was using slices to
		// manage memory, and just expanded the original buffer's slice to
		// include a larger portion of its enclosing memory chunk.
		// Whatever the case, we have now proven that the contents
		// *already exist* at the destination. So don't do extra work.
		return nTruncated;
	}

	// Forward the rest of the work to the private (internal) copy implementation.
	copy(src, dst, nToCopy);

	// Return number of truncated elements.
	return nTruncated;
}

public @nogc nothrow
DestArrayT copy
	(SourceArrayT, DestArrayT)
	(SourceArrayT src, DestArrayT dst)
	if( areCopyCompatibleArrays!(SourceArrayT,DestArrayT) )
{
	// Although this is a copying function, the first thing to do is to
	// attempt to complete the copy operation without copying anything.
	// That is the fastest possible outcome.
	if ( src is null || src.length == 0 )
	{
		// The simplest case: no copying or slice management needed.
		// Just leave the destination array unchanged.
		return dst;
	}

	assert(dst !is null,
		"Cannot copy a non-empty source range into an empty target range.");

	assert(dst.length >= src.length,
		"Cannot copy a source range into a smaller target range.");

	size_t nToCopy = src.length;

	if ( dst.ptr is src.ptr )
	{
		// The almost-simplest case. Maybe the caller was using slices to
		// manage memory, and just expanded the original buffer's slice to
		// include a larger portion of its enclosing memory chunk.
		// Whatever the case, we have now proven that the contents
		// *already exist* at the destination. So don't do extra work.
		return dst[nToCopy .. $];
	}

	// Now that we've tried to get out of it, we've learned that we must copy.
	// Actually, really, for sure. Now do it.
	return copy(src, dst, nToCopy);
}

private @nogc nothrow
DestArrayT copy
	(SourceArrayT, DestArrayT)
	(SourceArrayT src, DestArrayT dst, size_t nToCopy)
{
	// The porcelain functions should ensure that these constraints are met:
	assert(src !is null);
	assert(dst !is null);
	assert(src.length > 0);
	assert(dst.length > 0);
	assert(src.ptr !is dst.ptr);

	// The non-copying cases have now been excluded.
	// Everything that follows involves copying of some kind.

	// Perform overlapping copies if needed. Otherwise, just do
	// a normal D array blit.
	//
	// The overlapping cases are broken out into separate functions due to
	// suspicion that they may contain enough instructions to affect icaching.
	// This gives the compiler the ability to avoid inlining that code,
	// which should at least make the non-overlapping (common) case slightly faster.
	// And it's more tidy.
	if ( src.ptr < dst.ptr && dst.ptr < src.ptr + nToCopy )
		copyUp(src, dst, nToCopy);
	else
	if ( dst.ptr < src.ptr && src.ptr < dst.ptr + nToCopy )
		copyDown(src, dst, nToCopy);
	else
	{
		// Non-overlapping copy.
		// This uses optimized memory copying routines under the hood and is
		// about 10-20x faster than element-by-element copying.
		dst[0..nToCopy] = src[0..nToCopy];
	}

	return dst[nToCopy .. $];
}

private @nogc nothrow
void copyUp
	(SourceArrayT, DestArrayT)
	(SourceArrayT src, DestArrayT dst, size_t nToCopy)
{
	// Shifting to higher addresses: copy the tail first and work
	// towards the head.

	// Optimization: attempt to batch the copying into array
	// slice copies. This may provide the compiler/runtime some
	// opportunities to optimize, since these sub-slices are
	// guaranteed to be NOT overlapping.
	//
	// In case it's not obvious how this is possible, examples are provided
	// below. In these examples, the square brackets represent the source
	// array, and the curly braces represent the destination array.
	//
	// Example scenario: copy [1,2,3,4] into {3,4,X,X}
	//
	// Unoptimized example:
	// [1,2,{3,4],X,X} -> [1,2,{3,X],X,4} -> [1,2,{X,X],3,4}
	// -> [1,X,{X,2],3,4} -> [X,X,{1,2],3,4}
	// (Takes 4 operations.)
	//
	// Optimized example:
	// [1,2,{3,4],X,X} -> [1,2,{X,X],3,4} -> [X,X,{1,2],3,4}
	// (Takes 2 operations.)
	//
	// This optimization should work best on mostly-not-overlapping
	// copies.
	ptrdiff_t copyIncr = (dst.ptr /* + nToCopy */) - (src.ptr /* + nToCopy */);
	assert(copyIncr > 0);

	ptrdiff_t copyLower = nToCopy - copyIncr;
	ptrdiff_t copyUpper = nToCopy;
	while(copyLower >= 0)
	{
		dst[copyLower .. copyUpper] = src[copyLower .. copyUpper];
		copyLower -= copyIncr;
		copyUpper -= copyIncr;
	}

	// It will likely overshoot on the last chunk. Clip that chunk and finish it.
	if ( copyUpper > 0 )
		dst[0 .. copyUpper] = src[0 .. copyUpper];
}

private @nogc nothrow
void copyDown
	(SourceArrayT, DestArrayT)
	(SourceArrayT src, DestArrayT dst, size_t nToCopy)
{
	// Shifting to lower addresses: copy the head first and work
	// towards the tail.
	//
	// This is probably what the D array blit would do, but the
	// spec forbids overlapping copy of any kind, so we'll do it
	// ourselves.

	// Optimization: attempt to batch the copying into array
	// slice copies. This may provide the compiler/runtime some
	// opportunities to optimize, since these sub-slices are
	// guaranteed to be NOT overlapping.
	//
	// In case it's not obvious how this is possible, examples are provided
	// below. In these examples, the square brackets represent the source
	// array, and the curly braces represent the destination array.
	//
	// Example scenario: copy [1,2,3,4] into {X,X,1,2}
	//
	// Unoptimized example:
	// {X,X,[1,2},3,4] -> {1,X,[X,2},3,4] -> {1,2,[X,X},3,4]
	// -> {1,2,[3,X},X,4] -> {1,2,[3,4},X,X]
	// (Takes 4 operations.)
	//
	// Optimized example:
	// {X,X,[1,2},3,4] -> {1,2,[X,X},3,4] -> {1,2,[3,4},X,X]
	// (Takes 2 operations.)
	//
	// This optimization should work best on mostly-not-overlapping
	// copies.
	ptrdiff_t copyIncr = (src.ptr /* + nToCopy */) - (dst.ptr /* + nToCopy */);
	assert(copyIncr > 0);

	size_t copyLower = 0;
	size_t copyUpper = copyIncr;
	while(copyUpper <= nToCopy)
	{
		dst[copyLower .. copyUpper] = src[copyLower .. copyUpper];
		copyLower += copyIncr;
		copyUpper += copyIncr;
	}

	// It will likely overshoot on the last chunk. Clip that chunk and finish it.
	if ( copyLower < nToCopy )
		dst[copyLower .. nToCopy] = src[copyLower .. nToCopy];
}

///
@safe unittest
{
    int[] a = [ 1, 5 ];
    int[] b = [ 9, 8 ];
    int[] buf = new int[](a.length + b.length + 10);
    auto rem = a.copy(buf);    // copy a into buf
    rem = b.copy(rem);         // copy b into remainder of buf
    assert(buf[0 .. a.length + b.length] == [1, 5, 9, 8]);
    assert(rem.length == 10);   // unused slots in buf
}

/**
As long as the target range elements support assignment from source
range elements, different types of ranges are accepted:
*/
@safe unittest
{
    float[] src = [ 1.0f, 5 ];
    double[] dest = new double[src.length];
    src.copy(dest);
}

/+ TODO: ranges n shit
/**
To _copy at most `n` elements from a range, you may want to use
$(REF take, std,range):
*/
@safe unittest
{
    import std.range;
    int[] src = [ 1, 5, 8, 9, 10 ];
    auto dest = new int[](3);
    src.take(dest.length).copy(dest);
    assert(dest == [ 1, 5, 8 ]);
}

/**
To _copy just those elements from a range that satisfy a predicate,
use $(LREF filter):
*/
@safe unittest
{
    import std.algorithm.iteration : filter;
    int[] src = [ 1, 5, 8, 9, 10, 1, 2, 0 ];
    auto dest = new int[src.length];
    auto rem = src
        .filter!(a => (a & 1) == 1)
        .copy(dest);
    assert(dest[0 .. $ - rem.length] == [ 1, 5, 9, 1 ]);
}

/**
$(REF retro, std,range) can be used to achieve behavior similar to
$(LINK2 http://en.cppreference.com/w/cpp/algorithm/copy_backward, STL's `copy_backward`'):
*/
@safe unittest
{
    import std.algorithm, std.range;
    int[] src = [1, 2, 4];
    int[] dest = [0, 0, 0, 0, 0];
    src.retro.copy(dest.retro);
    assert(dest == [0, 0, 1, 2, 4]);
}
+/

// Test CTFE copy.
@safe unittest
{
    enum c = copy([1,2,3], [4,5,6,7]);
    assert(c == [7]);
}


@safe unittest
{
    import std.algorithm.iteration : filter;
/+ TODO: ranges
    {
        int[] a = [ 1, 5 ];
        int[] b = [ 9, 8 ];
        auto e = copy(filter!("a > 1")(a), b);
        assert(b[0] == 5 && e.length == 1);
    }
+/
    {
        int[] a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        copy(a[5 .. 10], a[4 .. 9]);
        assert(a[4 .. 9] == [6, 7, 8, 9, 10]);
    }

    // https://issues.dlang.org/show_bug.cgi?id=7898
    {
        enum v =
        {
            import std.algorithm;
            int[] arr1 = [10, 20, 30, 40, 50];
            int[] arr2 = arr1.dup;
            copy(arr1, arr2);
            return 35;
        }();
        assert(v == 35);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=13650
@safe unittest
{
    import std.meta : AliasSeq;
    static foreach (Char; AliasSeq!(char, wchar, dchar))
    {{
        Char[3] a1 = "123";
        Char[6] a2 = "456789";
        assert(copy(a1[], a2[]) is a2[3..$]);
        assert(a1[] == "123");
        assert(a2[] == "123789");
    }}
}

/+ TODO: ranges
// https://issues.dlang.org/show_bug.cgi?id=18804
@safe unittest
{
    static struct NullSink
    {
        void put(E)(E) {}
    }
    int line = 0;
    struct R
    {
        int front;
        @property bool empty() { return line == 1; }
        void popFront() { line = 1; }
    }
    R r;
    copy(r, NullSink());
    assert(line == 1);
}
+/
