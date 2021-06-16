

/// The module uses template instantiation to know all possible Exception types
/// that might be thrown. Also, when the caller uses a compile-time string
/// formatter (the preferred method of error message generation), the module
/// will be able to use those template instantiations to estimate the size
/// of the largest possible (Exception-object + error-message) combination.
/// This largest-possible-allocation information is used to configure an
/// allocator at start-time that will pre-allocate memory for the
/// largest-possible-case, as well as for smaller cases. The count of smaller
/// pre-allocations shall not exceed log2(S) per every largest-possible
/// pre-allocation, where S is the size of the largest possible preallocation.
/// This means that the smaller allocations may be slightly over-allocated,
/// as there could be many more types of exceptions than there are
/// pre-allocations. 
///
/// Each size of pre-allocation will have its own `SharedFreelist` allocator.
/// If there are `n` active threads, as measured by incrementing a counter
/// during `static this()` and decrementing during a `static ~this()`, then
/// each SharedFreelist will keep a minimum of `sqrt(n)` blocks reserved
/// in the SharedFreelist.
///
module exogear.awareness.package;




TODO:
- Compile-time formatter object that can provide almost-exact size requirements
	to the module's static constructor
- Also support building error reports!
- Algorithm that calculates allocation size before allocating
- Segregator(n x SharedList) allocator that handles largest statically-known exception type
- Are Segregator and SharedList @nogc?


enum Severity
{
	info,
	warning,
	error
}



report(enum tag, string name, formatArgs...)


template error(string fmtstr, A...)
{
	static if (

	@nogc nothrow error()
	{
	}
}


/// Describes what timestamp format should be used when printing
/// info/warning/error reports (ex: to stdout and in log files).
enum TimeStampReportingFormat
{
	/// Uses the system's locale settings to determine the timestamp format
	/// placed on report printouts.
	/// Here is example output for a certain US locale:
	///   12/31/2020 12:31 AM  Error:   An exception was thrown. Good luck!
	///    12/4/2020  1:15 PM  Warning: There might be a problem.
	systemLocale,

	/// Uses a variant of ISO8601 date-time format:
	///   YYYY-MM-DD HH:MM:SS
	/// Examples:
	///   2020-12-31 12:31:21  Error:   An exception was thrown. Good luck!
	///   2020-12-04 13:15:56  Warning: There might be a problem.
	human8601,

	/// Uses strict ISO8601 date-time formatting, which takes this form:
	///   YYYY-MM-DDTHH:MM:SS
	/// Examples:
	///   2020-12-31T12:31:21  Error:   An exception was thrown. Good luck!
	///   2020-12-04T13:15:56  Warning: There might be a problem.
	iso8601,
}

// So I ran through some ordinary everyday scenarios that I've had to deal
// with in my programming career:

scenario A
The fusion reactor went critical because there were too many spoods in the
brigglebam. We should also give the poor sap a percentage criticality.

int32  spoods = 57;
double criticality;

throw error!ReactorException.report!("Too many spoods in the brigglebam!")
	.details!("There were too many spoods in the brigglebam.")
	.details!("The exhalted beauracracy will be very displeased.")
	// A note about param!(x) :
	// If we pass state as a variable, we know it's value.
	// BUT. If we pass it as a template alias, we know it's value AND its name.
	// No more redundant typing of "someLongName = ${someLongName}" type stuff for us!
	.param!(spoods)
	.param!(criticality / 100.0).format!("%s%%")
	.advice!(
		"Flood the sloosharbor with coolant and run like hell."~
		"Upon reaching a safe distance, prepare CYA paperwork.")
	.advice!(
		"Consider using sadgets to increase criticality next time, "~
		"They are less likely to agitate the spood breeding inductors.");

scenario B
An ordinary twitch-spasm occured in one of the auxiliary mecha-tentacles.
Nothing to throw about, but we should definitely note this somewhere.

enum TentacleClass
{
	...
}

class Tentacle
{
	uint64    id;
	string    modelNumber;
	string    vendorId;
	string    vendorName;
	Datetime  lastService;
}

bool describe(Report)(Report r, Tentacle t)
	if ( instanceOf!(ReportContext, Report) )
{
	
}

writeln(warning.report!"Twitch-spasm incident"
	.details!("A twitch-spasm occurred in one of the mecha-tentacles.")
	.details!(
		"It's probably nothing to worry about, but if you are a "~
		"mechanical neurotechnician, you may want to know about this.")
	.param!(TentacleClass.auxiliary)
	.param!(tentacle.id)
	.section
		.details!("Full tentacle description:")

		// For user-defined types like structs and classes, the param!(...)
		// processor will try using describe(<report context>, tentacle) first,
		// then tentacle.describe(<report context>), then std.conv.to!string
		// after that.
		//
		// When matching `describe` implementations, the free-function form
		// is prioritized due to its ability to handle cases where classes
		// are `null`.
		.param!(tentacle)

	.end
	.advice!(
		"If there have been incidents of tungsten leechworms in the vacinity "~
		"of tentacle %d, consider checking the convex joint plate for leaks "~
		"and inspect the synapse module's packing oil.", tentacle.id)
	.advice!(
		"Otherwise, it is should be safe to ignore this.");


scenario C
One of the yam harvesting operations completed. There are a thousand like it,
but this one is ours.



scenario D
There was a security breach and there are intruders in the research facility.
We need to alert the responders, but we don't want to tell them the nature
of the overcylinder contents unless the attackers are near that section
and the crucible is retracted.

throw error!SecurityBreach.report!("Intruders detected")
	.details!("Intruders have been detected in the research facility.")
	.details!("All Violent Incident Response (VIR) staff should report to ")
		.cont!("the given location promptly and assume the position.")
	.param!(intruder.locationId)
	.when(
		(  intruder.locationId == overcylinder.locationId
		|| intruder.locationId == protonicLab.locationId
		|| intruder.locationId == darkPlaza.locationId
		)
		&& researchFacility.crucible.retracted
	) // lazy parameter
	.section
		.format
		.details!("Beware! The intruders are approaching the overcylinder.")
		.details!("It contains %s and constitutes a risk of global depopulation ")
			.cont!("and would result in a PR disaster.")
			.args(overcylinder.contents)
	.end
	.



interface ReportIterator
{
	@property bool empty() const;
	@property ReportSegment front() const;
	@property ReportSegment back() const;
	@property ReportIterator save() const;
	void popFront();
	void popBack();
	ReportSegment opIndex(size_t index_) const;
	ReportSegment opIndexAssign(size_t index_, ReportSegment val);
	int opApply(scope int delegate(ref ReportSegment) dg);
}

class ArrayBackedReportIterator : ReportIterator
{
	import std.range.primitives;

	private ReportSegment[] contents_;

public:
	this(ReportSegment[] contents)
	{
		this.contents_ = contents;
	}

	@property bool empty() const { return contents_.empty; }
	@property ReportSegment front() const { return contents_.front; }
	@property ReportSegment back() const { return contents_.back; }
	@property ArrayBackedReportIterator save() const {
		return new ArrayBackedReportIterator(this.contents_);
	}
	void popFront() { contents_.popFront(); }
	void popBack() { contents_popBack(); }
	ReportSegment opIndex(size_t index_) const { return contents_[index_]; }
	ReportSegment opIndexAssign(size_t index_, ReportSegment val) {
		return contents_[index_] = val;
	}

	int opApply(scope int delegate(ref ReportSegment) dg)
	{
		int result = 0;

		for (size_t i = 0; i < contents_.length; i++)
		{
			result = dg(contents_[i]);
			if (result)
				break;
		}
		return result;
    }
}

final class EmptyReportIterator : ReportIterator
{
public:
	immutable EmptyReportIterator instance = new EmptyReportIterator();

final:
	@property bool empty() const { return true; }
	@property ReportSegment front() const { assert(0, "Accessed 'front' of an EmptyReportIterator."); }
	@property ReportSegment back() const { assert(0, "Accessed 'back' of an EmptyReportIterator."); }
	@property ArrayBackedReportIterator save() const { return this; }
	void popFront() {}
	void popBack() {}
	ReportSegment opIndex(size_t index_) { assert(0, "Attempted to do indexed read from an EmptyReportIterator."); }
	ReportSegment opIndexAssign(size_t index_, ReportSegment val) {
		assert(0, "Attempted to do indexed write into an EmptyReportIterator.");
	}
	int opApply(scope int delegate(ref ReportSegment) dg){ return 0; }
}


abstract class ReportSegment
{
protected:
	import std.traits : isArray, isFloatingPoint;

	enum estimateTextSizeOf(T : ubyte)  =  3; // "0" - "255"
	enum estimateTextSizeOf(T : byte)   =  4; // "-128" - "127"
	enum estimateTextSizeOf(T : ushort) =  5; // "0" - "65535"
	enum estimateTextSizeOf(T : short)  =  6; // "-32768" - "32767"
	enum estimateTextSizeOf(T : uint)   = 10; // "0" - "4294967295"
	enum estimateTextSizeOf(T : int)    = 11; // "-2147483648" - "2147483647"
	enum estimateTextSizeOf(T : ulong)  = 20; // "0" - "18446744073709551615"
	enum estimateTextSizeOf(T : long)   = 20; // "-9223372036854775808" - "9223372036854775807"

	template estimateTextSizeOf(T, uint digitsOfPrecision)
		if ( isFloatingPoint!T )
	{
		// Largest conversion possible: "-N.ddd[...]dddE−16382"  (quad-floating-point)
		// So "-N." contributes 3, and "E-16382" contributes 7.
		// It could also be "nan", "NAN", "inf", "INF", "infinity", "INFINITY",
		// "-inf", "-INF", "-infinity", or "-INFINITY", so if this ever gets
		// modified, make sure nothing lower than that ever gets returned.
		// ("-infinity".length == 9, so 3+0+7 == 10 should be plenty.)
		enum estimateTextSizeOf = 3 + digitsOfPrecision + 7;
	}

	template estimateTextSizeOf(T)
		if( is(T == enum) )
	{
		import std.conv;
		import std.meta : staticMap, staticSort;
		import std.traits : EnumMembers;

		enum stringSize(alias enumMember) = (enumMember.to!string).length;
		enum cmp(size_t a, size_t b) = a < b;
		enum sizeOfLongestMemberName =
			staticSort!(cmp, staticMap!(stringSize, EnumMembers!T))[$-1];

		enum estimateTextSizeOf =
			T.stringof.length +
			".".length +
			sizeOfLongestMemberName;
	}

	enum TestEnum01
	{
		foobar,
		foo,
		foobarbaz, // "foobarbaz".length == 9
		qux,
		quuuuux,
	}

	enum TestEnum02
	{
		foo
	}

	static assert(estimateTextSizeOf!TestEnum01 == (10+1+9)); // "TestEnum01.foobarbaz".length
	static assert(estimateTextSizeOf!TestEnum02 == (10+1+3));

	template estimateTextSizeOf(T)
		if( isArray!T )
	{
		import std.range.primitives : ElementType;
		import std.traits : isDynamicArray, isStaticArray;
		alias E = ElementType!T;
	}

	template estimateTextSizeOf(T, size_t maxNumberOfElementsDisplayed)
		if( isDynamicArray!T )
	{
		import std.range.primitives : ElementType;
		alias E = ElementType!T;
	}



public:
	@property size_t sizeEstimate();
	@property children() { return EmptyReportIterator.instance; }
}

class ReportStringSegment : ReportSegment
{
	private string formatString_;
public:
	@disable this();

	this(string formatStr)
	{
		this.formatString_ = formatStr;
	}

	@property string formatString() const { return formatString_; }
	@property string formatString(string val) { return formatString_ = val; }

	@property size_t formatArgCount() const { return 0; }

	@property size_t sizeEstimate() const {
		return formatString_.length;
	}
}

class ReportFormatString(Args...) : ReportStringSegment
{
	override @property size_t formatArgCount() const { return Args.length; }

	override @property size_t sizeEstimate() const {
		
	}
}

class ReportHeader(Args...) : ReportFormatString(Args...)
{
}

class ReportDetails(Args...) : ReportFormatString(Args...)
{
}

class ReportParameter(T, T val, uint digitsOfPrecision)
	if ( isFloatingPoint!T )
{
}

class ReportParameter(T, T val)
{
}

class FormatSection(Args...) : ReportStringSegment
{
	private ReportSegment[] contents_;

	@override @property children() {
		return new ArrayBackedReportIterator(this.contents_);
	}
}

struct FormattedStringFragment
{
	/// `literalValue` will be one of two things:
	/// (1) Unformatted text, with formatstring escape sequences unescaped*.
	/// (2) A format spec, with leading '%' character included.
	///
	///
	/// * In cases where modification of the original string is not possible,
	///     this may be accomplished by splitting the string around escape
	///     sequences and having the second slice exclude the first escape
	///     sequence character. So "abc%%def" would become ["abc","%def"],
	///     with each of those strings having their own
	///     `FormattedStringFragment` object.
	string literalValue;

	///
	FormatChar formatChar;

	/// If `literalValue` represents a format spec, then this will be the
	/// number of bytes required to store the longest possible result of
	/// formatting an arbitrary (but valid) argument. Of course, some
	/// arguments (for example: strings and arrays) can be arbitrarily large,
	/// hence this is merely an "estimate". Most likely, in the case of
	/// arguments with arbitrary size, this will indicate some amount of
	/// memory that strikes a balance between handling the majority of
	/// use-cases while avoiding the (pre)allocation of large regions of memory.
	///
	/// Be aware that this may also change between formatters (ex: two "%s"
	/// might not return the same estimated max size), because it is likely
	/// that large format objects will only appear once in a string, with other
	/// objects of the same class being smaller. Hence a pre-allocator should
	/// have one (or few) larger pre-allocations to handle the occasional
	/// wall-of-text, with a number of smaller pre-allocations to handle
	/// the more common smaller stuff (names, symbols, identifiers,
	/// grammatical elements, etc).
	///
	/// If the format argument is a user-defined type that has a matching
	/// `stringizer` function or that has a `stringizer` property, then
	/// the returned `Stringizer` (TODO: implement/define) struct will be used
	/// to obtain this value.
	///
	/// If `literalValue` represents an unformatted slice of a string literal,
	/// then its `.length` value will match this field's value. This is merely
	/// done for consistency. Do not rely on this to distinguish between
	/// format specs and unformatted text, because this could happen
	/// coincidentally in the second case.
	size_t estimatedMaxBytesNeeded;

	/// This is set to true when `estimatedMaxBytesNeeded` is actually known
	/// precisely (exactly) due to the nature of the format argument or
	/// format spec.
	///
	/// Examples of situations where this occurs (or could possibly be
	/// implemented) are as follows:
	/// - Formatting numeric built-ins (integers, floats, chars).
	/// - Formatting fixed-sized arrays of other elements with precisely-known formatted output size.
	/// - Format specs with width limits. Truncation establishes bounds on memory usage.
	/// - User-defined types with a `stringizer` implementation or property that has precise output size. (TODO: define this)
	/// - User-defined types that don't define `toString` or `stringizer` and
	///     consist entirely of fields with precisely known formatted output size. (unimplemented)
	bool estimatedMaxBytesNeededIsPrecise_;
	@property bool estimatedMaxBytesNeededIsPrecise() const {
		return estimatedMaxBytesNeededIsPrecise_;
	}
}

enum FormatChar
{
	/// Used to indicate that a format string fragment does not represent
	/// a formatting sequence, but is instead just a normal slice of a
	/// string literal.
	none,

	s, /// Ex: "%s"; Format argument is a string, or automatically determined.
	c, /// Ex: "%c"; Format argument must be a character (char, wchar, dchar).
	b, /// Ex: "%b"; Integer in binary notation
	d, /// Ex: "%d"; Integer in decimal notation
	o, /// Ex: "%o"; Integer in octal notation
	x, /// Ex: "%x" and "%X"; Integer or pointer formatted as hexadecimal string.
	e, /// Ex: "%e" and "%E"; Floating point in (scientific) E notation. (N.NNNeNN)
	f, /// Ex: "%f" and "%F"; Floating point in decimal notation. (N.NNNNNNNN)
	g, /// Ex: "%g" and "%G"; Floating point in either notation depending on magnitude of exponent.
	a, /// Ex: "%a" and "%A"; Floating point in hexadecimal notation: 0xh.hhhhhhp±d

	arrayOpen,  /// Ex: "%("
	arrayClose, /// Ex: "%)"

	/// Corresponds to '%|', which specifies where the delimiter begins in
	/// an array formatter.
	pipe,
}

struct FormatStringAnalysis
{
	FormattedStringFragment[]  fragments;
	size_t                     argumentCount;
}

struct ReportSegment
{
	ReportSegmentType     type;
	FormatStringAnalysis  formatStringAnalysis;
}

struct Report
{
	ReportSegment   title_;
	ReportSegment[] contents_;

	void title(string fmtstr)
	{
		contents_ ~= new ReportHeader(fmtstr);
	}

	void details(string fmtstr)
	{
		contents_ ~= new ReportDetails(fmtstr);
	}

	void 
}









TODO:

keep template invocations lean: probably just append text to each invocation,
along with some serialized metadata about that segment (length of fmtstr,
est max size required for each format arg, purpose of segment (detail, param,
advise), tree level). This avoids having template instantiations filled with
long type names repeated over and over (ex: the FQN of a ReportSegment
container struct or w/e). Once at the end of the chain, process the
compile time information to generate an immutable or enum Report struct.
If Exception is used, it all must be formatted at constructor invocation.
If AlternativeException (name?) is used, then the formatting can be delayed
until the catch clause deals with it using a range interface (well, the range
might be internal, or the interface would just output chunks of formatted
text and the caller only needs to dump them somewhere, like stdout/stderr).
This might prevent large allocations entirely: the static text never needs
to be copied into a large buffer, it is just pushed directly onto whatever
I/O or handler awaits it on the other side. Formatted values still need to
have memory preallocated for them, but in many cases this will probably be
dwarfed by the size of the static strings, so this could be big savings
(exception: if the formatted value is a large array or something, then it
might be much bigger than the format string parts surrounding it).

The OOP interface I was creating is probably unnecessary. Just put all
needed features into one struct. The struct should not be templated.
Any compile time information entering the struct should be normalized
into string form or some other common attribute.

TODO: How to make Handler implementations aware of the compile-time information?
The Handler will be important for determining the size required for all of
the formatting and stuff. (Or maybe not. It could be that the Report will
handle formatting of all individual strings and format-x-args sections, but
that it won't do any compile-time interpretation of the segment meanings. That
gets delayed until runtime. This might work alright, as the Handler would
not be operating in the context where an exception is being thrown, and
it a lot of its content might be static strings that can, like the others
available at compile-time, just be dump into a stream or output range like
stdout.

.param members should be turned into formatstrings by the Handler before any
size estimation is concluded. This means the Handler may require at least
two passes: one format string generation pass, then another pass to actually
produce a range of string fragments for consumption by streams/logs/stdout/etc.







