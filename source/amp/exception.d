module amp.exception;

import amp.internal.appender;

private:
// Memory preallocated for exceptions that need to be thrown from code that
// is forbidden from making heap allocations.
void[512] exceptionMemory;
size_t ememCursor = 0;

@nogc void onOverflow(E)()
{
	import std.traits : isInstanceOf;
	static if ( isInstanceOf!(ExceptionFailedNogcAllocation, E) )
		throw error!E();
	else
		throw error!(ExceptionFailedNogcAllocation!E)();
}

@nogc nothrow void resetEmem()
{
	ememCursor = 0;
}

@nogc void[] allocateEmem(E)(size_t nBytes)
{
	size_t newUsage = ememCursor + nBytes;
	if ( newUsage > exceptionMemory.length )
		onOverflow!E();
	void[] result = exceptionMemory[ememCursor .. newUsage];
	ememCursor = newUsage;
	return result;
}

public:

/// Calls std.format.formattedWrite using pre-allocated thread-local memory.
/// The type E should be the exception type that the string is intended to
/// be used in.
@nogc string formatError(E = Exception, A...)(string fmtstr, A fmtArgs)
{
	import std.format : formattedWrite;

	void onOverflow(size_t nElems, size_t amountFilled, size_t capacity)
	{
		.onOverflow!E();
	}

	auto writer = FeedbackBufferedAppender!char(
		exceptionMemory[ememCursor .. $], &onOverflow);

	string result = formattedWrite(writer, fmtstr, fmtArgs);
	
	ememCursor += writer.length;
	return result;
}

/// This exception is thrown whenever the size of an exception exceeded
/// the current thread's memory that was pre-allocated for exception handling.
class ExceptionFailedNogcAllocation(E) : Exception
{
	this()
	{
		string msg = formatError!(typeof(this))(
			"%1$s exception occurred. The proper error message for this exception "~
			"could not be generated because the %1$s object required more memory than "~
			"what is available in pre-allocated exception memory.",
			E.classinfo.name);
		super(msg);
	}
}

/// Creates a new exception of type E using pre-allocated thread-local memory.
/// This is useful for writing @nogc code that needs to throw exceptions,
/// such as when a caller-allocated buffer isn't large enough to hold the
/// result. This can also make stack unwinding less expensive.
@nogc E error(E, A...)(A args)
{
	resetEmem();
	return error_!E(args);
}

// Internal version that doesn't call resetEmem. It's important not to call
// resetEmem more than once per exception, as it will lead to clobbering of
// already-written data.
private @nogc E error_(E, A...)(A args)
{
	import std.conv : emplace;
	size_t memNeeded = __traits(classInstanceSize, E);
	void[] emem = allocateEmem!E(memNeeded);
	static if ( A.length == 0 )
	{
		// .../dmd.2.085.0/linux/bin64/../../src/phobos/std/conv.d(4512,9):
		//   Error: static assert:  "Don't know how to initialize an object of
		//       type ExceptionFailedNogcAllocation!(Exception) with arguments ()" 
		// So it seems that we can't use 0-arg ctors with std.conv.emplace. Damn.
		// Probably a phobos bug.
		// Let's work around it.
		emem[] = typeid(E).initializer[];
		E inst = cast(E)emem.ptr;
		inst.__ctor();
		return inst;
	}
	else
		return emplace!E(emem, args);
}

/// Creates a new exception of type E using pre-allocated thread-local memory.
/// Unlike the 'error' function, this requires the exception object to accept
/// only one argument in its constructor: a string. This function will then
/// accept a format string and any format arguments, which it will use to
/// create the string that is passed to the exception's (E's) constructor.
/// The formatting is performed by the 'formatError' function, which uses
/// the same pre-allocated thread-local memory that is used to allocate the
/// exception object.
@nogc E errorfmt(E, A...)(string fmtstr, A fmtArgs)
	if ( __traits(compiles, new E("test")) )
{
	resetEmem();
	string msg = formatError!E(fmtstr, fmtArgs);
	return error_!E(msg);
}

/// Creates a normal Exception object using pre-allocated thread-local memory.
/// The exception's message is the result of formatting the given fmtstr.
/// The formatting is performed by the 'formatError' function, which uses the
/// same pre-allocated thread-local memory that is used to allocate the
/// exception object.
@nogc Exception error(A...)(string fmtstr, A fmtArgs)
{
	return errorfmt!Exception(fmtstr, fmtArgs);
}

/// Creates a normal Exception object using pre-allocated thread-local memory
/// and passes the given 'msg' to its constructor.
/// No string formatting is performed.
@nogc Exception vagueError(string msg)
{
	return error!Exception(msg);
}


