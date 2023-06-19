# Errors, or lack of them

Dr, it really hurts when I pass an `nums` array to a routine that expects a
`fixeds` array... you all know the answer to that !

I want to talk about ChrysaLisp error checking policy and philosophy.

"First rule of error club is we don't pass errors."

## Philosophy

DON'T coddle ! Don't waste time checking things that should never happen ! It's
debatable if I should have any of those low level rect `region` class width and
height `<= 0` checks ! Because of the question, "Can it even legally happen ?"

I think the best thing to say up front is that the responsibility for error
checking inputs should be pushed as far up the call tree (towards the root) as
possible. And, even then, once you have tested that your runtime validation of
inputs is correct, you should be able to turn it all off to gain better
performance and footprint.

You have to distinguish between erroneous inputs and sometimes `null` values.
It might be totally legit to test that you got passed a 0 length vector to the
`'path :transform` VP function. That's probably not an errorcase but a
legitimate result you should care about coping with.

But if a VP level function is supposed to be passed a `list` and you pass it an
`array` then we aren't going to waste time type checking that at the VP level,
nor do we define an error return type you should check for !

At the lowest level, VP code core functions, there is no forgiveness. They
assume you pass correct input and always provide a valid output. YOU are in the
wrong if you pass crap to them and it's perfectly valid to crash if you do so.

## Lisp throws

We should try to type check everything we can at the Lisp interpreter level, IF
running in debug mode. Front load all the type checking and stupid input
checking on the Lisp binding `lisp_xxx` functions to test for types and just
plane crap input, and provide some almost sensible error throws. But wrap all
of that in the errorcase macros so we can strip it out when doing a release
mode build.

## Defensive coding

Another point to make is about defensive coding.... DON'T do it ! Absolutely
don't do it. Don't engage with stupid, "They will drag you down to their level
and beat you with experience..."

All those times you code a loop to break on `iter >= iter_end` !!! Stop doing
that ! You're introducing a test coverage analysis leak. If your inputs and
algorithm say you should hit an `=` end stop, then say so in the coding, don't
wimp out and think you're helping anyone by saying `>=`, you're not helping !
You're perpetuating the problems. What would happen on a CPU that has some
weird addressing system that uses negative arranged addresses ? Or you cross
the memory limit boundary and wrap back to zero... but that can't happen ? No
it did in the past, maybe not so much now, but maybe `The Mill CPU` will come
up with a good way to exploit that idea...

The further down to the leaf code you introduce an error check, the worse you
make things in terms of performance. Maybe you can get your compiler to prove
certain things can't happen and optimize them away, but you're living in dream
land if you really think they actually can in all but the most boring
situations. All those extra bits of code for type checking and error throwing
end up slowing down the compilation and build system, clog up the CPU cache,
evict the instructions that you actually need to execute, in favour of some
that never get executed in a correct program !

## Signed stuff

"Testing for < 0 when it is an unsigned integer is worthless anyway, <= 0 only
catches zero."

Well that brings up other issues !

At the VP level you're not dealing with signed or unsigned values, you're
dealing with registers and YOU treat them as signed or unsigned. You're not
getting a compiler to manipulate them with `vp-asr-cr` vs `vp-shr-cr` based on
the type you declared ! The CScript level compiler will do this, but at the VP
level it's baked in by YOU !

Can you even check for the type of a register input ? NO, it's not possible. At
this level it's just some bits and you perform some defined actions on them.
