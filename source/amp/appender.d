module amp.appender;

/// This is similar to std.array.Appender, but with an API and implementation
/// that allows it to be @nogc.
struct StrictBufferedAppender(T)
{
private:
	import amp.internal.appender;
	FeedbackBufferedAppender!T  impl;

public:
	alias impl this;

	@nogc void put(T elem)
	{
		if ( !impl.tryPut(elem) )
			throw errorfmt!RangeError(
				"Attempt to append one element to already-full appender with capacity ",
				impl.capacity,".");
	}

	@nogc void put(T elems)
	{
		size_t startLength = impl.length;
		size_t nTruncated = impl.attemptPut(elems);
		if ( nTruncated )
			throw errorfmt!RangeError(
				"Attempt to append ", elems.length, " elements into appender with ",
					startLength," of ",impl.capacity," elements already occupied.");
	}
}

/+ TODO:
/// Similar to StructBufferedAppender, but uses a caller-provided allocator
/// to expand whenever an overflow scenario occurs.
struct BestEffortBufferedAppender(T)
{
	
}
}+/
