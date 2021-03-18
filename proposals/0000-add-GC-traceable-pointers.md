---
author: Dong
date-accepted: ""
ticket-url: ""
implemented: ""
---

This proposal is [discussed at this pull request](https://github.com/ghc-proposals/ghc-proposals/pull/0>).
**After creating the pull request, edit this file again, update the number in
the link, and delete this bold sentence.**

# Add GC traceable pointers backed by a threaded slab allocator

Currently, we have a messy story on standard byte array and slice types:

+ The standard byte slice type in GHC is `ByteString`, which is backed by `ForeignPtr`.
+ `ForeignPtr` is actually implemented with `MutableByteArray#` if it's allocated on GHC heap.
+ `ForeignPtr` uses `Addr#`, which uses `mutableByteArrayContent`, and in turn requires `touch#` to be safe.
+ The vector package use `ByteArray#` directly. 
+ Small `ByteArray#` could be unpinned, thus can't be passed in safe FFI.
+ If we pass `ByteArray#` to FFI, the slicing offset has to be passed separately.

Oh my!

## Motivation

+ We want a unified, easy to be used with FFI, memory, and CPU cost-effective pointer type to represent byte arrays. 

## Proposed Change Specification

We propose adding a new primitive type `GAddr#`, which is:

+ Allocated from a threaded local slab allocator, here slab means memory pool classified by size(16, 32, 64...).
+ It's GC traceable.
+ It can be added or substracted with offset, the result is still a `GAddr#`, which is traceable.
+ It can be passed to FFI like a pointer type, such as `char*` or `int16_t*` depend on types.

## Effect and Interactions

Now we can have a unified slice and array type:

```
data PrimArray a = PrimArray GAddr# Int#
```

We don't need another slice type: the slicing operation can be implemented like:

```
take :: Int -> PrimArray Word8 -> PrimArray Word8
take (PrimArray gaddr len) (I# n) | n <=# len = PrimArray (gaddr +# n) (len -# n)
                                  | otherwise = error "..."
```

The new slice type could:

+ Save an info table word.
+ Save a closure length word.
+ Save an offset word.

Which would be `3*8 = 16` bytes a 64bit machine!

For FFI code, we should be able write:

```
memcpy :: GAddr# -> GAddr# -> Int# -> IO ()
```

## Costs and Drawbacks

It seems we already have a slab allocator for the older generation in low pause GC work, so the extra work is adding new primitive types mainly.

A drawback of `GAddr#` is that the allocation cost compared with `ByteArray#` may be higher, we have to find a suitable slab, and do some bit twiddling work.

## Alternatives

Don't do it, and continue to use `ByteArray#` as the main byte array type.

## Unresolved Questions

I have none.

## Implementation Plan

I hope the Haskell foundation could help.
 
