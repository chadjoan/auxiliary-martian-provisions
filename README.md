# EXOGEAR

WARNING: This library is very work-in-progress. At any given commit, it is more
than likely broken and won't build. It's not ready for any serious use yet.
There might be useful bits here and there, if you're curious.

### Summary

A library providing useful constructs intended to complement and build upon
what is offered in Phobos.

Some functionality may nearly duplicate what is available in Phobos, but
must follow a not-backwards-compatible API in order to achieve the strict
(but very caller-friendly) memory management, purity, idempotency, and
error handling ideals that this library strives to meet.

### @nogc-friendliness

Exogear has an emphasis on providing fine-grained control over memory buffering
and allocation strategies. Every operation should have an `@nogc` version unless
the garbage collector is necessary for the operation by definition. `nothrow`
versions of functions are also provided whenever it isn't entirely unwieldy.

### @nogc Exceptions

This library also features a work-in-progress attempt at @nogc exceptions.
Depending on the current state of druntime+phobos, this may require
druntime+phobos modification to be truly @nogc (for incidental but potentially
thorny reasons).

The `@nogc` exception implementation hinges upon these techniques:
* Preallocation of space for Exception objects and any other classes/structures.
* Preallocation of format-string destination buffers based on template information
	(using compile-time format strings as template parameters allows Exogear to
	make usually-reasonable guesses about the largest contiguous chunk of
	memory that might be required to represent the most verbose exception,
	thus enabling precise preallocation).
* When possible, error message formatting and substitution is deferred until
	it needs to be printed. This lazy calculation of compositions of data is
	in line with D's use of Ranges: when values are calculated at the last
	possible moment, it is often possible to skip buffering steps entirely
	and do zero memory allocation. This isn't a silver bullet, and any chunk
	of memory held by a stack frame that is being exited (due to an Exception
	being thrown and unwinding the stack) will need to be copied before it
	gets lost or free'd. Nonetheless, saving memory wherever we can will make
	the worst-case scenarios much less likely.
* Failover to a (caller-replaceable) mallocator if preallocation comes up short.
	(This can happen if multiple worst-case exceptions are thrown or the error
	messages involve dynamic data, such as runtime strings, that don't have
	compile-time or start-time predictable sizes.)
* Handoff of preallocated and mallocated exception memory whenever an exception
	is caught. Memory handoff is a potentially complicated topic
	with several possibilities for user-choice (caller-choice) being
	highly desirable, as the caller may wish to deal with handoff differently
	depending on how they are managing memory resources. At the same time,
	it is important to preserve reasonably efficient and safe defaults for
	code that does not have strict memory management requirements.
	(Ex: most likely, caught exceptions that aren't merely printed or rethrown
	should automatically give themselves GC-allocated memory if they were
	caught in GC-enabled functions and the caller doesn't preempt this choice).

Assuming a thrown exception doesn't require highly dynamic data in its
error reporting (and can thus fit in preallocated exception space),
and assuming your allocator doesn't shoot you in the back, then this strategy
should even allow for smooth recovery from out-of-memory situations.
And while recovery might still be impossible for programs that have hard
memory requirements beyond what the OOM error allows, there are also plenty
of programs whose memory will end up invested in "optional" things like caches,
old connections (or other stale state), low priority sound/image data, and
so on. Such programs could use strategies such as early GC collection and
trimming of discretionary allocations to reduce memory use (or pressure)
before retrying the failed operation.

### The "index" type

Although a small consideration, the `index` type has shown great promise
and thus deserves mention.

Every now and then a nasty corner-case will lead one to consider peeling
back some fundamental assumptions in the quest for happy generality. And
every now and then, that process of destruction and creation delivers
something that feels incredibly *right*.

The `index` type is one such thing.

Before the `index` type, there was ambiguity:

```D
ptrdiff_t  find_first_v0( string text_body, dchar to_find )
{
	foreach( i, ch; text_body )
		if ( ch == to_find )
			return i;

	return ??; // What to return to indicate "not found"?
}
```

What should the above function return in its `ptrdiff_t` to indicate that
no value was found?  `-1`?  `ptrdiff_t.max`?  `text_body.length`?

Unfortunately, all of those have a boolean equivalence of `true`,
which is an odd thing to receive for a negative result!

Indeed, we end up with nonesense like this:
```D
unittest
{
	if ( find_first_v0("haystack", 'z') )
	{
		// Intuitively, we expect the above `find_first_v0` to
		// return `false` because there is no 'z' character in "haystack".
		// But it returns `-1` and...
		assert(0); // Error! This triggers!
	}
}
```

If we want to return something with `false` equivalence when the search
fails, then we'd be compelled to return 0. That is a bad idea, however,
because 0 would also indicate that the first character in `text_body`
was equivalent to `to_find`, a very different conclusion from
"nothing matched".

That horn of the dichotomy leads down another dark alley:
```D
unittest
{
	if ( find_first_v0("haystack", 'h') )
	{
		// Now we expect a return value to evaluate to `true`,
		// because there's definitely an 'h' at the beginning
		// of "haystack". IT'S RIGHT THERE!
		// But it returns `0` and...
		assert(0); // We expect this to trigger, but it never does.
	}
}
```

This type of function will typically return one of those earlier sentinel
values (`-1`, `ptrdiff_t.max`, `text_body.length`) to indicate the "not found"
result, even if it means the function's return value will do unintuitive
things when evaluated in a boolean context (such as the conditional
expression of an if-statement, as in the above examples).

Hence we end up with something like this:
```D
ptrdiff_t  find_first_v0( string text_body, dchar to_find )
{
	foreach( i, ch; text_body )
		if ( ch == to_find )
			return i;

	return -1;
}
```

At least it's possible to return all possibly outcomes.

It just sucks and might cause errors.

There HAS to be a better way.

Behold, the index type:
```D
import exogear.types : index;

index  find_first(string text_body, dchar to_find)
{
	foreach( i, ch; text_body )
		if ( ch == to_find )
			return index(i);

	return index(false);  // <--  o.O
	// (And yes, that will actually do The Right Thing.)
}

unittest
{
	index  idx;

	// This case is just like when `size_t` or `ptrdiff_t` is used
	// as a return value: the 0th element is 'h', and the 1st is 'a',
	// so find_first returns 1 to indicate it matched 'a' at position 1.
	idx = find_first("haystack", 'a');
	assert(idx == 1);

	// We usually want to be able to return the 0th element too.
	// That's pretty normal. But...
	idx = find_first("haystack", 'h');
	assert(idx == 0);

	// What about the negative case?
	idx = find_first("haystack", 'z');

	// `idx` can't be 0. That would indicate that there's a 'z' at the
	// start of "haystack", and there clearly is not.
	assert(idx != 0);

	// But we want it to have `false` equivalence. Does it?
	assert(idx == false); // You bet it does.

	// Let's use it a little differently and see if it's still OK.
	// This is like that earlier example with `find_first_v0`.
	// The `assert(0)` should never be reached, because the expression
	// `find_first("haystack", 'z')` should evaluate as boolean `false`.
	if ( find_first("haystack", 'z') )
		assert(0); // Passes. It's still OK!

	// Let's revisit the 0th match case.
	// Earlier we asserted that this returns 0.
	// But does it also return `true`?
	assert(find_first("haystack", 'h')); // Yes. Yes it does.

	// The good doctrine of "Full Power! Total Destruction!" dictates
	// that we have to attempt destroying our index in at least one
	// more way: have it represent negative values.
	// This happens in a number of situations, including when we need
	// a natural way to represent how far an element is from the end of
	// a string or an array, or when we need to calculate the offset
	// between two substrings in a larger string.

	// So let's try negatives:
	idx = find_first("haystack", 'y');
	idx = idx - "haystack".length;

	// Is it negative?
	assert(idx < 0); // Good.
	assert(idx == -6); // This should also be true.

	// Since `-6` is a valid offset, we don't expect it to be `false`.
	assert(idx == true);
	assert(idx != false);

	// This is consistent with constructs such as
	//   python's slicing operator:
	//
	string pyslice(string s, ptrdiff_t i1, ptrdiff_t i2)
	{
		index _i1 = i1;
		index _i2 = i2;
		if ( _i1 < 0 )
			_i1 += s.length;
		if ( _i2 < 0 )
			_i2 += s.length;
		return s[_i1 .. _i2];
	}

	assert(pyslice("haystack", -8,-5) == "hay");
	assert(pyslice("haystack", -6,-1) == "ystac");

	// So those should be true:
	assert(index(-8));
	assert(index(-6));
	assert(index(-5));
	assert(index(-1));
	// ... and they are.

	// There's still another thing to check.

	// An obvious, but suboptimal, way to implement the `index` type would be
	// to make it a struct with a `ptrdiff_t` in it, but also with another
	// boolean in it to convey its existential nature. This is suboptimal
	// because every `index` variable in the program will now occupy more
	// memory, possibly even twice as much in cases where struct and variable
	// padding/alignment dictate that a lone `bool` occupy an entire 4 or 8 bytes.
	// That waste is then multiplied by the number of times such variables appear.
	// It's not something that everyone cares about, but it would just feel
	// *wrong*, OK?

	// Well, let's exclude that possibility.
	assert(ptrdiff_t.sizeof == index.sizeof); // No waste!

	// That `false` value is hiding somewhere in the `index` type's
	// number line, but we'll get to that later. More importantly, we've
	// already seen that it's likely to be somewhere far away from the ranges
	// of values that we care about using for valid indices into arrays.
}
```

The `index` type flies in the face of integer-boolean-equivalence traditionally
found in C-family languages, including D. That is, integer types are regarded
as `false` when they equal zero, and as `true` when they equal any other value.

This is clearly not the case with the `index` type.

And that's why it's good at its job.

The insight behind the `index` type is that C-family integer-boolean-equivalence,
as exhibited by the behavior of `ptrdiff_t` earlier, is objectively suboptimal
for 0-based array indices. Both `0` and `1` can plausibly be within the bounds
of an array, and yet traditional boolean equivalence has these two values behave
very differently in boolean contexts!

It would make sense, then, to choose our `true` and `false` to correlate
with valid and invalid indices, respectively. And that is exactly what
the `index` type does.

This truth table illustrates the boolean equivalence for both `index` and
for all other native D (C-style) integers:

|  type       |       `true` values             | `false` values |
|-------------|---------------------------------|----------------|
| `index`     |  `index.min+1 .. index.max`     |   `index.min`  |
| `ptrdiff_t` | All `i` s.t. `i < 0 \|\| i > 0` |       `0`      |
| `size_t`    |     All `i` s.t. `i > 0`        |       `0`      |
| `int`       | All `i` s.t. `i < 0 \|\| i > 0` |       `0`      |

Now we see that the `index` type has simply moved its `false` equivalence
from `0` to `index.min`. Given that `index` is a signed integer obeying
twos-complement arithmetic, `index.min` is the farthest possible value
from `0` in either direction. It's a polar opposite.

It's also extremely unlikely to coincide with any valid array indices.
Even on 32-bit machines, it will take about 2 billion array elements for
that to become a problem, *and* it would need to be negatively indexed.
As we've established, that *can* happen, but it's definitely less common
than positive indexing.

Of course, normal C-style integers might even work out well enough for
"1-based" arrays, but we promise never to mix "0-based" and "1-based" arrays
in our programs, lest [E.W. Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra)
haunt us with [visions of (Xerox PARC) Mesa programmers struggling to manage
4 different indexing modes](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html)!

### Naming Conventions

Exogear diverges from the traditional D style guide in terms of variable,
constant, function, and template naming by employing *snake_case* instead
of CamelCase when naming these entities.

Also, acronyms, abbreviations, and initialisms are treated as full words
when CamelCasing type names. Thus, when writing an acronym in a type name,
always capitalize the first letter of the acronym, then lowercase all others.

The below example provides some names that highlight or exaggerate differences
in the traditional D style convention (as found in Phobos and DRuntime) and
the Exogear D style convention.

Example:
```D
// --------------------
// Traditional D style:
class XMLNode(T)
{
	XMLNode[] xmlChildNodes;

	auto appendValue(T)(T parameterName) { ... }

	static XMLNode htmlDOMToXML(HTMLDOMXML htmlDOMXML) { ... }
}

struct WrapperForXML { ... }

// --------------
// Exogear style:
struct XmlNode(T)
{
	XmlNode* xml_child_nodes;

	auto append_value(T)(T parameter_name) { ... }

	static XmlNode html_dom_to_xml(HtmlDomXml html_dom_xml) { ... }
}

struct WrapperForXml { ... }
```

This represents a balanced strategy: *snake_case* ends up used for the majority
of identifiers, which makes it easy and painless to deal with names that
contain acronyms, abbreviations, and initialisms (as these are very common
in software technology, among other contexts). On the other hand, CamelCase
is still used for type names because it provides good visual contrast when
type identifiers appear next to function or variable identifiers, a situation
that occurs *very* frequently!

If not for the helpfulness of visual contrast, *snake_case* or *SNAKE_CASE*
might have been chosen for type names as well. However, having type names and
function or variable names both be *snake_case* tends to result in some
very undesirable runs of text, like so:
```D
scuba_diver_but_better driver_for_butter_scoops()
{
	type_thats_useful useless_variable;
	type_stats usable_analysis_strategy;
	dont_blame_me it_only_gets_better_from_here;
	// ... and_so on ...
}
```

Given that space and underscore are not very distinguishable in such
situations, the CamelCase type names allow us to ensure that there can
be at most one space or underscore at the ends of *snake_case* identifiers,
as they are otherwise bordered by D's punctuation (special characters
like parentheses or semicolons). Thus the above mess becomes a little bit
more managable:
```D
ScubaDiverButBetter driver_for_butter_scoops()
{
	TypeThatsUseful useless_variable;
	TypeStats usable_analysis_strategy;
	DontBlameMe it_only_gets_better_from_here;
	// ... AndSo on ...
}
```

This does mean that acronyms in type names can still give us headaches.
However, since type names always begin with an uppercase letter, we can
make identical acronyms be also visually identical, and at all times
(within CamelCase). In type names, an acronym's first letter is *always*
capitalized and its remaining letters are *always* lowercase. This decision
is also important for making it easier to tell where acronyms begin and end
when they are placed adjacent to each other in a type name. This leads to
the contrast between `HTMLDOMXML` (traditional) and `HtmlDomXml` (Exogear)
seen in the first style difference example.


