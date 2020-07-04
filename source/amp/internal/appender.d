module amp.internal.appender;

import std.traits : hasIndirections;
/// Determines if the given buffer type (BufferT) is appropriate for storing
/// the given element type (ElemT).
///
/// The logic goes like so:
/// If ElemT contains pointers (directly or indirectly), then this must be
/// of type void[]. If ElemT does not contain pointers, this can be of type
/// ubyte[] or byte[].
///
/// These constraints avoid situations that might fool the compiler or GC and
/// cause unpredictable or undefined behavior. For example, ubyte[] memory is
/// assumed by the GC to be free of pointers (an optimization to avoid
/// excessive scanning), but if it were to actually contain pointers, then
/// the GC wouldn't see them and might prematurely free the memory that those
/// pointers point to, thus causing later operations to dereference invalid
/// memory (which leads to mysterious crashing and heisenbugs). Hence, this
/// template is used by buffer-accepting functions to encourage calling code
/// to avoid poor buffer type choices.
///
/// This does allow non-referential types to be stored in void[] buffers,
/// which can lead to suboptimal code. This situation may cause unnecessary
/// GC activity. This does not result in incorrect code, so the caller is
/// allowed to make this decision for themselves.
enum isValidBufferTypeFor(BufferT, ElemT) =
	is(BufferT == void[]) ||
		( !hasIndirections!ElemT &&
			(  is(BufferT == byte[])
			|| is(BufferT == ubyte[])
			)
		);

/// This is the underlying implementation for amp.appender.
/// It does not provide a 'put' function and is thus not an OutputRange.
/// Instead, it provides 'attemptPut' and 'tryPut' functions that are nothrow
/// and instead use return values to indicate success or failure. This is
/// a poor practice for everyday error handling, but it is an important
/// building block for the more friendly functions with better error handling.
/// If the caller really needs a 'put' function, use a FeedbackBufferedAppender
/// instead. That appender is derived from this one, but offers a callback
/// mechanism to handle overflow.
/// This appender can also be used in modules that the amp.appender
/// module depends on, thus breaking dependency cycles. The original motivator
/// was to prevent a dependency cycle between amp.exception and
/// amp.appender.
struct BasicBufferedAppender(T)
{
private:

	// The buffer used to store the intermediate result.
	T[] buffer;
	size_t cursor;

public:

	///
	@disable this();

	/// Construct an Appender using the given buffer.
	///
	/// The buffer must be non-null and have a non-zero length.
	///
	/// If T contains pointers (directly or indirectly), then this must be
	/// of type void[] or T[]. If T does not contain pointers, this can be of
	/// type ubyte[], byte[], void[], or T[].
	@nogc this(BufferT)(BufferT buffer)
		if ( isValidBufferTypeFor!(BufferT, T) )
	{
		this(cast(T[])buffer);
		assert( this.buffer.length == (buffer.length / T.sizeof) );
	}

	/// ditto
	@nogc this(T[] buffer)
	{
		assert(buffer !is null, );
		assert(buffer.length > 0);
		this.buffer = buffer;
	}

	/// Attempts to place the given element into the appender's buffer.
	/// Returns: 0 on success, 1 (the number of truncations) on failure (overflow).
	@nogc nothrow size_t attemptPut(T elem)
	{
		size_t end = cursor + 1;
		if ( end > this.buffer.length )
			return 1;
		this.buffer[this.cursor] = elem;
		this.cursor = end;
		return 0;
	}

	/// Attempts to place the given elements into the appender's buffer.
	/// Returns: 0 on success or the number of truncations on failure (overflow).
	@nogc nothrow size_t attemptPut(T[] elems)
	{
		size_t end = cursor + elems.length;
		size_t nToCopy;
		ptrdiff_t nTruncated = end - this.buffer.length;
		if ( nTruncated > 0 )
		{
			end = this.buffer.length;
			nToCopy = elems.length - nTruncated;
		}
		else
			nToCopy = elems.length;

		this.buffer[cursor .. end] = elems[0 .. nToCopy];
		this.cursor = end;

		if ( nTruncated > 0 )
			return nTruncated;
		else
			return 0;
	}

	/// Attempts to place the given element into the appender's buffer.
	/// Returns: true on success, false on failure (overflow).
	@nogc nothrow bool tryPut(T elem)
	{
		return !attemptPut(elem);
	}

	/// Attempts to place the given elements into the appender's buffer.
	/// Returns: true on success, false on failure (overflow).
	@nogc nothrow bool tryPut(T[] elems)
	{
		return !attemptPut(elems);
	}

	/// Returns: The number of elements that have been appended.
	@nogc nothrow @property size_t length() const
	{
		return this.cursor;
	}

	/// Returns: The number of elements the appender can hold, including
	///   already-appended elements.
	@nogc nothrow @property size_t capacity() const
	{
		return this.buffer.length;
	}

	/// Sets the capacity for the appender. This will likely cause a heap
	/// allocation and is thus not an @nogc operation.
	///
	/// This is useful for best-effort situations where capacity demand
	/// increases are very unlikely. However, if you really need to avoid
	/// heap/gc activity, consider using the expand function instead.
	@property size_t capacity(size_t newSize)
	{
		size_t result = this.buffer.length = newSize;
		if ( this.cursor > result )
			this.cursor = result;
		return result;
	}

	/// Changes the appender's buffer to the one given by 'newBuffer'.
	/// The current contents of the buffer will be copied into the new buffer,
	/// unless 'newBuffer' has the same .ptr as the existing buffer.
	/// If 'newBuffer' and the existing buffer overlap, a properly ordered
	/// overlapping copy is performed.
	///
	/// The new buffer must be non-null and have a non-zero length.
	///
	/// Returns: 0 if the resize caused no truncation or the number of
	///   truncated elements if truncation occurred.
	@nogc nothrow size_t resize(T[] newBuffer)
	{
		assert(newBuffer !is null);
		assert(newBuffer.length > 0);

		if ( this.cursor == 0 )
		{
			// The simplest case: no copying, buffer management, or cursor
			// adjustments are needed.
			// Just change the buffer and return.
			this.buffer = newBuffer;
			return 0;
		}
		else
		if ( newBuffer.ptr is this.buffer.ptr )
		{
			// The almost-simplest case. Maybe the caller was using slices to
			// manage memory, and just expanded the original buffer's slice to
			// include a larger portion of its enclosing memory chunk.
			this.buffer = newBuffer;
		}
		else
		{
			// The copying case.
			import amp.algorithm.mutation : truncatingCopy;

			// Calculate the copy size.
			// This will become very useful for multiple reasons.
			size_t nToCopy = this.cursor;
			if ( nToCopy > newBuffer.length )
				nToCopy = newBuffer.length;

			// Calculate overlapping based on populated and non-truncated
			// contents only. This might avoid an overlapping copy if
			// the appender is empty enough.
			// Of course, this is also a way to simply avoid copying
			// unpopulated or truncated portions of the buffers, even
			// when the overlap is inevitable.
			T[] copySrc = this.buffer[0 .. nToCopy];

			// Hand the mess off to a function dedicated to fast overlapping
			// copies that might truncate.
			truncatingCopy(copySrc, newBuffer);
		}

		// If truncation occured, adjust the cursor and return the
		// truncation count.
		ptrdiff_t nTruncated = this.cursor - this.buffer.length;
		if ( nTruncated > 0 )
		{
			this.cursor = this.buffer.length;
			return nTruncated;
		}

		return 0;
	}

}

/// Similar to BasicBufferedAppender, but implements the 'put' function using
/// a callback to indicate overflow conditions.
struct FeedbackBufferedAppender(T)
{
private:
	BasicBufferedAppender!T  impl;
	@nogc void delegate(size_t,size_t,size_t) onOverflow;

	@nogc void overflow(size_t nElems, size_t amountFilled, size_t capacity_)
	{
		this.onOverflow(nElems, amountFilled, capacity_);
	}

public:
	alias impl this;

	///
	@disable this();

	/// This appender requires a callback, so the normal 1-argument
	/// constructor used by buffering appenders is forbidden.
	@disable this(BufferT)(BufferT buffer);

	import std.traits : hasIndirections;
	/// Construct an Appender using the given buffer.
	/// If T contains pointers (directly or indirectly), then this must be
	/// of type void[]. If T does not contain pointers, this can be of type
	/// ubyte[] or byte[].
	@nogc this(BufferT)(BufferT buffer, void delegate(size_t,size_t,size_t) onOverflow)
		if ( isValidBufferTypeFor!(BufferT, T) )
	{
		this.impl = BasicBufferedAppender!T(buffer);

		// Ugly cast needed because
		// "@nogc attribute for function parameter is not supported" (in dmd 2.085.0)
		this.onOverflow = cast(typeof(this.onOverflow))onOverflow;
	}

	@nogc void put(T elem)
	{
		size_t nTruncated = impl.attemptPut(elem);
		if ( nTruncated )
			overflow(1, impl.length, impl.capacity);
	}

	@nogc void put(T[] elems)
	{
		size_t nTruncated = impl.attemptPut(elems);
		if ( nTruncated )
			overflow(elems.length, impl.length, impl.capacity);
	}
}
