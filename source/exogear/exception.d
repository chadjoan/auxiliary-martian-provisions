/// This module implements @nogc exception handling. In most cases, it will
/// be possible to throw an exception without allocating new memory at all.
/// This is accomplished by pre-allocating a small-but-reasonable amount of
/// memory at thread creation (including the main thread) using malloc
/// or (for non-main threads) a configurable allocator. This should handle
/// the needs of most non-chained exceptions. If an exception allocation
/// exceeds the thread's preallocation, it is expanded using the thread's
/// current exception memory allocator (by default, this is the Mallocator,
/// which uses malloc). In total, this means that small exceptions cause
/// no allocation when constructed and thrown, while larger exceptions
/// cause non-gc heap activity.
///
/// Limitations:
///
/// * When an exception is created by this module, it can only be guaranteed
///     that the exception object itself, as well as any error message
///     copied or formatted by this module, will be emplaced in pre-allocated
///     memory. In other words, `exogear.exception` can only comprehensively
///     allocate memory for Exceptions that are shallow in nature.
///     It is possible that an object inherited from Exception will allocate
///     other things (e.g. in its constructor), and the `exogear.exceptions`
///     module currently has no way to know about, fulfill, or anticipate such
///     allocations. It is the caller's responsibility to manage any such
///     memory. If the caller needs to prevent such allocations from happening
///     while a problematic Exception class cannot be modified, then this
///     would need to be handled by implementing an equivalent Exception
///     class that does not perform out-of-band GC allocations. `exogear.exception`
///     would then be able to instantiate the replacement class without
///     issue.
///
/// Future directions:
///
/// * Use wrapper structs to move exceptions into GC memory
/// if they are persisted after being caught (before the exception memory is
/// reset by more exceptions being thrown).
///
/// * Have some way to forbid or more elegantly handle situations where caller
/// code might have pointers into exception memory. This creates a hazard where
/// moving that memory can cause dangling pointers (e.g. if the memory is moved
/// into GC memory as above, or moved during an expanding reallocation of
/// the thread's pre-allocated exception memory).
///
/// * More considerations for chained exceptions. They might work as-is, but
/// haven't been tested at this point. It is more likely that chaining an
/// exception will require a second exception to be allocated after one has
/// already been thrown, which will cause the exception memory for the first
/// exception to be deallocated and reallocated to the second exception, thus
/// deleting the first exception and either aliasing its references to the
/// second exception or creating dangling pointers. So this module probably
/// won't work for this use-case right now. This is a bug. Any code needing
/// to add exception(s) to an existing exception should be able to either
/// cause a non-gc exception memory expansion or allow existing
/// non-gc'd exceptions to be moved into gc memory.
///
module exogear.exception;

import std.experimental.allocator : IAllocator;

import exogear.internal.appender;

public:
/// The exception memory preallocation size is how much memory is allocated at
/// the start of each thread's execution for storing and handling exceptions.
/// This is its default value.
enum defaultExceptionMemoryPreallocationSize = 512;

alias AllocatorFactory = IAllocator delegate();

private:

// This module preallocates some amount of memory to allow exceptions to be
// allocated and thrown from functions marked as @nogc. If more memory is
// needed, then it is allocated from the specified allocator and not from
// the GC.

// Constant defaults.
shared AllocatorFactory  defaultAllocatorFactory;

shared static this()
{
	import std.experimental.allocator : CAllocatorImpl;
	import std.experimental.allocator.mallocator;
	defaultAllocatorFactory = () => new CAllocatorImpl!Mallocator;
}

// Runtime Settings
// These are, in some sense, the default values during thread creation.
// However, to avoid confusion with the constant default values (above),
// these will not be referred to as "defaults" but as "runtime settings".
shared size_t            exceptionMemoryPreallocationSize_;
shared AllocatorFactory  exceptionMemoryAllocatorFactory_;
shared bool              isNewThreadExceptionMemoryScannedByGC_ = true;

shared static this()
{
	exceptionMemoryPreallocationSize_ = defaultExceptionMemoryPreallocationSize;
	exceptionMemoryAllocatorFactory_  = defaultAllocatorFactory;
}

// Thread-local Settings
IAllocator  exceptionMemoryAllocator_;
bool        isExceptionMemoryScannedByGC_;

// State
// (As of this writing, there is no global/shared state, just global settings,
// and all state is thread-local. This is good for avoiding thread contention
// and synchronization pains, so long as we can pull it off.)
void[]      exceptionMemory;   // The preallocated memory used for exception handling.
size_t      ememCursor = 0;
bool        ememCurrentlyScannedByGC = false;

// The allocator used to obtain the preallocated memory.
// This may or may not be the current allocator, becuase the current allocator
// can change after the allocation.
IAllocator  ememAllocatorUsed;

static this()
{
	exceptionMemoryAllocator_ = exceptionMemoryAllocatorFactory_();
	isExceptionMemoryScannedByGC_ = isNewThreadExceptionMemoryScannedByGC_;

	ememAllocatorUsed = exceptionMemoryAllocator;
	exceptionMemory = exceptionMemoryAllocator.allocate(exceptionMemoryPreallocationSize);
}

// This clears any allocations made from the exception memory.
// It does not "free" the memory in the sense of returning it to the original
// allocator. The exception memory will remain allocated. What this function
// does is render all of that exception memory available. Anything previously
// used for exception handling will be made available again and could be
// overwritten by subsequent exception handling operations (which is fine,
// as long as that memory is no longer used and isn't referenced anywhere).
@nogc nothrow void resetEmem()
{
	import core.memory : GC;

	.ememCursor = 0;
	if ( .ememCurrentlyScannedByGC )
		GC.removeRange(.exceptionMemory.ptr);

	.ememCurrentlyScannedByGC = false;
}

// TODO: Is this nothrow? In principle we can run out of memory. That might
//   not be a consideration for the 'nothrow' keyword, but one of the advantages
//   of reducing or eliminating heap allocations during exception handling is
//   that OOM conditions become non-fatal (assuming the program has a
//   non-allocating way to free non-essential memory, such as cache memory,
//   and continue operating).
@nogc void[] allocateFromEmem(E)(size_t nBytes)
{
	import core.memory : GC;

	size_t oldUsage = .ememCursor;
	size_t newUsage = .ememCursor + nBytes;
	if ( newUsage > .exceptionMemory.length )
		reallocateExceptionMemory(newUsage * 2);
	void[] result = .exceptionMemory[ememCursor .. newUsage];
	.ememCursor = newUsage;

	if ( !.isExceptionMemoryScannedByGC_ )
		return result;

	// TODO:
	// This range management operation should be atomic with respect to the GC.
	//
	// So it'd be nice if there were some way to tell the GC to NOT do any
	// collections during this time. This isn't the same as "disabling" it,
	// because the intent isn't to turn off the gc, just to make it wait
	// briefly while the atomic operation finishes. (Note that it's also
	// possible that calling code is using his module because it is avoiding
	// the GC entirely and already turned it off. That mostly shouldn't matter,
	// but this code needs to be careful that it doesn't lose program state,
	// such as the enabled/disabled status of the GC.)
	if ( .ememCurrentlyScannedByGC )
		GC.removeRange(.exceptionMemory.ptr);

	if ( .isExceptionMemoryScannedByGC )
	{
		GC.addRange(.exceptionMemory.ptr, newUsage);
		.ememCurrentlyScannedByGC = true;
	}

	return result;
}

public:

/// This propery allows the caller to change the allocator used when new thread
/// creation causes this module to pre-allocate memory for exceptions to use
/// later on.
///
/// Normally, during program startup, this module will have already allocated
/// the exception memory for the main thread before the the main thread's
/// static constructors can affect this. However, changing the allocator
/// factory can still be useful for affecting how exception memory is
/// preallocated for other threads.
///
/// It is possible to change how the memory is allocated for an existing
/// thread by setting the .exceptionMemoryAllocator property from that thread
/// and then calling .reallocateExceptionMemory from that thread.
///
/// Giving any thread the gc_allocator for managing its exception memory pool
/// is not recommended. This would invalidate the @nogc guarantee on any
/// functions that throw exceptions using memory from these memory pools.
///
@nogc nothrow @property  AllocatorFactory  exceptionMemoryAllocatorFactory() {
	return .exceptionMemoryAllocatorFactory_;
}

/// ditto
@nogc nothrow @property  AllocatorFactory  exceptionMemoryAllocatorFactory(AllocatorFactory  factory) {
	return .exceptionMemoryAllocatorFactory_ = factory;
}

/// The allocator that was used by the current thread to pre-allocate memory
/// for exception handling, and that will be used if that memory is freed
/// or its size changed by the .reallocateExceptionMemory function.
///
/// Changing this allocator to the gc_allocator is not recommended. This would
/// invalidate the @nogc guarantee on any functions that throw exceptions
/// using memory from these memory pools.
///
@nogc nothrow @property IAllocator  exceptionMemoryAllocator() {
	return .exceptionMemoryAllocator_;
}

/// ditto
@nogc nothrow @property IAllocator  exceptionMemoryAllocator(IAllocator  allocator) {
	return .exceptionMemoryAllocator_ = allocator;
}

/// The exception memory preallocation size is how much memory is allocated at
/// the start of each thread's execution for storing and handling exceptions.
///
/// Altering this value will not cause existing preallocations to be
/// reallocated. This property is specifically for changing the
/// preallocation made during thread creation. If an existing thread
/// needs to change its exception memory allocation, this can be accomplished
/// by calling the 'reallocateExceptionMemory' function from that thread.
@nogc nothrow @property size_t  exceptionMemoryPreallocationSize() {
	return .exceptionMemoryPreallocationSize_;
}

/// ditto
@nogc nothrow @property size_t  exceptionMemoryPreallocationSize(size_t  newSize) {
	return .exceptionMemoryPreallocationSize_ = newSize;
}

/// This setting determines whether exception memory is added to the GC's
/// scanning ranges whenever an exception uses it.
///
/// The default isNewThreadExceptionMemoryScannedByGC setting would set this
/// to true. This might slow GC collection (likely by a very small,
/// unnoticable, amount), but prevents premature GC memory freeing if any
/// exceptions use GC memory pointers. Such usage would place GC memory
/// pointers into the exception handling memory pool once those exceptions
/// are emplaced.
///
@nogc nothrow @property bool isExceptionMemoryScannedByGC() {
	return .isExceptionMemoryScannedByGC_;
}

/// ditto
@nogc nothrow @property bool isExceptionMemoryScannedByGC(bool newBehavior) {
	return .isExceptionMemoryScannedByGC_ = newBehavior;
}


/// This is a global setting that determines the initial value of the
/// thread-local isExceptionMemoryScannedByGC setting.
///
/// Altering this setting will not change the thread-local setting for
/// already-existing threads.
@nogc nothrow @property bool isNewThreadExceptionMemoryScannedByGC() {
	return .isNewThreadExceptionMemoryScannedByGC_;
}

/// ditto
@nogc nothrow @property bool isNewThreadExceptionMemoryScannedByGC(bool newBehavior) {
	return .isNewThreadExceptionMemoryScannedByGC_ = newBehavior;
}


/// In the simplest case, this will simply call the current
/// exceptionMemoryAllocator's .reallocate method to change the current
/// thread's exception memory pool to 'newSize'.
///
/// If the exceptionMemoryAllocator has changed since the previous allocation,
/// or if that allocator's .reallocate method returns false, then the below
/// behaviors will happen:
/// 
/// If the exception memory is not in use, then this function will free the
/// calling thread's previously pre-allocated exception memory, and then
/// allocate a new block of exception memory using the current
/// exceptionMemoryAllocator.
///
/// If the exception memory is in use, then the order of this operation is
/// reversed so that the new block of memory is allocated before the old is
/// released. The used portion of the memory is copied from the old to the new
/// during the overlap.
///
/// Other remarks:
///
/// This function is used as a last-ditch effort to ensure that exceptions
/// can be allocated and handled when a thread's preallocated exception
/// memory is exhausted. If any code holds pointers into this memory
/// during the reallocation (ex: by holding references to exceptions during
/// this reallocation), then those pointers may become invalid.
///
/// This function can also be used to adjust the pre-allocation size for
/// existing threads.
///
/// It is strongly recommended to avoid shrinking the exception memory pool
/// when it is in use or could be in use (that is, after any Exception
/// objects have been allocated or errors reported from this module).
/// If the reallocation would shrink the memory pool to a size smaller than
/// what is currently in use, then the reallocation not be attempted and
/// this function will return false. If the use is not known ahead of time,
/// then the .exceptionMemoryInUse property can be used to determine if
/// the memory is in use or if a new pool size would cause reallocation to abort.
///
/// Currently this cannot be marked @nogc because the IAllocator interface
/// is (reasonably) not marked @nogc, and because (unreasonably) D doesn't
/// allow @nogc status to be positive-but-conditional upon callbacks or
/// polymorphic implementations (not the interfaces) being @nogc. Basically,
/// @nogc should enforce a function's contents to not use the garbage collector,
/// but it shouldn't force any callback mechanisms to be @nogc because the
/// caller is responsible for selecting those, and the caller's function
/// should only be allowed to be @nogc if the implementation of any callbacks
/// originating there are @nogc implementations. As of this writing, D just
/// doesn't work that way.
///
/// Returns: true under most circumstances. If the reallocation is a shrinking
///   operation and 'newSize' cannot hold all of the existing exception data,
///   then the reallocation will not be attempted and false is returned.
@nogc nothrow bool reallocateExceptionMemory(size_t newSize)
{
	import core.memory;

	if ( .exceptionMemoryInUse > newSize )
		return false;

	if ( .ememCurrentlyScannedByGC )
	{
		GC.removeRange(.exceptionMemory.ptr);
		.ememCurrentlyScannedByGC = false;
	}

	reallocateExceptionMemoryWithoutGcRangeAwareness(newSize);

	if ( .isExceptionMemoryScannedByGC && .ememCursor > 0 )
	{
		GC.addRange(.exceptionMemory.ptr, .ememCursor);
		.ememCurrentlyScannedByGC = true;
	}

	return true;
}

// Internal function used to decouple reallocation logic from gc range-tracking logic.
private @nogc nothrow void reallocateExceptionMemoryWithoutGcRangeAwareness(size_t newSize)
{
	bool done = false;

	// Ugly casts to get around @nogc's inability to handle callbacks and
	// caller responsibility.
	nothrow void[] allocate_interface(IAllocator allocator, size_t sz) {
		return allocator.allocate(sz);
	}

	nothrow bool reallocate_interface(IAllocator allocator, ref void[] mem, size_t newSize) {
		return allocator.reallocate(mem, newSize);
	}

	nothrow void deallocate_interface(IAllocator allocator, ref void[] mem) {
		allocator.deallocate(mem);
	}

	auto allocate =
		cast(void[] delegate(IAllocator, size_t) @nogc nothrow)
			&allocate_interface;

	auto reallocate =
		cast(bool delegate(IAllocator, ref void[], size_t) @nogc nothrow)
			&reallocate_interface;

	auto deallocate =
		cast(void delegate(IAllocator, ref void[]) @nogc nothrow)
			&deallocate_interface;
	// End ugly casts.

	if ( .ememAllocatorUsed is .exceptionMemoryAllocator_ )
	{
		// Simple case: just let the reallocator handle everything.
		done = reallocate(.exceptionMemoryAllocator_, .exceptionMemory, newSize);
	}

	if ( done )
		return;

	if ( .exceptionMemoryInUse > 0 )
	{
		import exogear.algorithm.mutation : copy;
		void[] newMemory = allocate(.exceptionMemoryAllocator_, newSize);

		copy(.exceptionMemory[0 .. ememCursor], newMemory);

		deallocate(.ememAllocatorUsed, .exceptionMemory);
		.ememAllocatorUsed = .exceptionMemoryAllocator_;
		.exceptionMemory = newMemory;
	}
	else
	{
		deallocate(.ememAllocatorUsed, .exceptionMemory);

		.ememAllocatorUsed = .exceptionMemoryAllocator_;
		.exceptionMemory = allocate(.exceptionMemoryAllocator_, newSize);
	}
}

/// Returns the amount of the calling thread's pre-allocated exception memory
/// that is currently in use, in bytes.
@nogc nothrow @property size_t exceptionMemoryInUse() {
	return .ememCursor;
}

/// Ensures that the calling thread's `exogear.exceptions` module is in a 'ready'
/// state. This means that it is able to create exceptions at any time without
/// performing a memory allocation (e.g. it is able to avoid using the malloc
/// heap, or whatever allocator has been assigned with
/// `exogear.exceptions.exceptionMemoryAllocator`).
///
/// More specifically, this function causes the calling thread's exception
/// pre-allocator to check for whether or not it currently possesses any unused
/// (ready) memory. If there is no such memory, then it will immediately
/// allocate a new block of memory to ensure that there will be memory ready
/// in advance of the next creation of an (`exogear.exception`) exception.
///
/// Any new thread (including the main thread) will always begin in a ready
/// state, so it is not (normally) necessary to call this from within
/// initialization code. Rather, this function only needs to be called if
/// an Exception (of any type) thrown by this module will be persisted
/// by the caller beyond the use of another (`exogear.exception` allocated)
/// Exception object, AND the persisted Exception did not have its allocation
/// firmed by either calling `exogear.exception.allocate` or `exogear.exception.claim`.
///
/// In other words, as long as the following conditions are met:
/// - An Exception was created by calling functions in `exogear.exceptions`
/// - That exception was thrown
/// - The `exogear.exception.allocate(exception)` and `exogear.exception.claim(exception)`
///     functions were NOT (and will not be) called.
/// - That exception could have a long lifespan, meaning that it may still
///     be residing in the `exogear.exception` module's memory when another
///     exception is created, and `exogear.exception.discard(exception)` might
///     not be called until after that.
/// ... then `ensureReadiness` should be called.
///
/// Notably, because this can allocate, there could be situations where the
/// caller doesn't want to call this right after an Exception is caught
/// (ex: if the Exception is caught in a region of code where heap or heap-like
/// memory allocations are forbidden for any reason). It is the caller's
/// responsibility to 

Instead, this should be called at any time after
/// catching an Exception (of any specific type) and before 
nothrow @property void ensureReadiness()
{
}

class EphemeralException(ExceptionT) : ExceptionT
{
	ExceptionT allocate() { ... } // use theAllocator?

	/// Calling this function signals that the caller is claiming ownership of
	/// the block of preallocated memory that the given exception resides
	/// within (it may also hold some referenced data, such as any error
	/// messages formatted in the `errorfmt` function). The returned
	/// MemoryBlock is exactly that block of memory being claimed.
	///
	/// Upon calling this, the thread's exception pre-allocator will
	/// allocate a different block of memory to hold any future exceptions,
	/// but only if it hasn't done so already.
	///
	MemoryBlock claim() { ... }
	void discard() { ... }
}

TODO: Compile-time formatter that can provide static size-needed estimation.

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
	void[] emem = allocateFromEmem!E(memNeeded);
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


