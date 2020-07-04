module amp.posix.exception;

class PosixException : Exception
{
	//private int _errno;
	//protected @property int errno(int errno) { return _errno = errno; }
	//public @property int errno() const { return _errno; }

	public static PosixException whenCalling(string posixFunctionCalled, int errnum,
		string file = __FILE__, size_t line = __LINE__)
	{
		string msg =
			"Call to "~posixFunctionCalled~" failed. Error is as follows: \n";

		return new PosixException(msg, errnum, file, line);
	}

	this(string msg, int errnum, string file = __FILE__, size_t line = __LINE__)
	{
		super(msg ~ getPosixError(errnum), file, line);
	}

	private string getPosixError(int errnum)
	{
		// This buffer is not 'shared', so it should be thread-safe to
		// reuse it betweeen function calls.
		static char[] lazilyAllocatedHeapBuffer;
		if ( lazilyAllocatedHeapBuffer is null )
			lazilyAllocatedHeapBuffer = new char[1024];

		return getPosixError(errnum, lazilyAllocatedHeapBuffer);
	}

	private string getPosixError(int errnum, char[] buffer)
	{
		import core.stdc.string;
		import std.string : fromStringz;
		import std.exception : assumeUnique;

		const(char)* s;
		version (CRuntime_Glibc)
		{
			while(true)
			{
				s = core.stdc.string.strerror_r(errnum, buffer.ptr, buffer.length);
				if ( s is null )
					return unknownError(errnum, buffer).assumeUnique();

				auto dstr = s[0..strlen(s)];
				if ( dstr.length < buffer.length )
					break;

				if ( buffer.length < 64 )
					buffer.length = 64;
				else
					buffer.length = (buffer.length * 3) / 2;
			}
		}
		else
		{
			s = core.stdc.string.strerror(errnum);
		}
		return s.fromStringz().assumeUnique();
	}

	private string unknownError(int errnum, char[] buffer)
	{
		import std.exception : assumeUnique;
		import amp.conv;

		string prefix = "Unknown error";
		size_t pos = 0;
		if ( prefix.length >= buffer.length )
			return "Unknown error"; // Super small buffer == return static string

		pos += prefix.length;
		buffer[0 .. pos] = prefix[];

		char[] numStr = intToDec(errnum, buffer);
		if ( numStr is null || numStr.length == 0 )
			return "Unknown error"; // buffer still too small == return static string
		pos += numStr.length;

		if ( pos >= buffer.length )
			return "Unknown error"; // Can't fit terminating \0

		buffer[pos] = '\0';
		pos++;

		return buffer[0..pos-1].assumeUnique();
	}
}

