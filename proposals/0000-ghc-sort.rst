.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull requst <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::

Proposal title
==============

This offers a new implemention for Data.List.sort and Data.List.sortBy, which has better performance characteristics and is more laziness-friendly.


Motivation
------------

Data.List.sort is a very important functionality in Haskell. I believe that the proposed implementation is:
- significantly faster than the current implementation on unsorted lists, typically 14% to 27% faster
- more laziness-firendly, i.e.: `take 3 | sort l` will require less comparisons than the current implementation

Proposed Change Specification
-----------------------------

::

    sort :: (Ord a) => [a] -> [a]
    sort =  sortBy compare
    
    sortBy cmp [] = []
    sortBy cmp xs = head $ until (null.tail) reduce (pair xs)
      where
        pair (x:y:t) | x `cmp` y == GT  = [y, x] : pair t
                     | otherwise        = [x, y] : pair t
        pair [x] = [[x]]
        pair []  = []
    
        reduce (v:w:x:y:t) = merge v' x' : reduce t
                             where v' = merge v w
                                   x' = merge x y
                             
        reduce (x:y:t) = merge x y : reduce t
        reduce xs      = xs
    
        merge xs []           = xs
        merge []  ys          = ys
        merge xs@(x:xs') ys@(y:ys') 
             | x `cmp` y == GT  = y : merge xs  ys'
             | otherwise        = x : merge xs' ys


Effect and Interactions
-----------------------

I have a stack project with a criterion test for this new implementation, available at https://github.com/greg7mdp/ghc-sort. 
I ran the tests on an Ubuntu 14.0.2 VM and GHC 8.0.2, and had the following results:

- sorting of random lists of integers is 27% faster
- sorting of random lists of strings is 14% faster
- sorting of already sorted lists is significantly slower, but still much faster than sorting random lists
- proposed version is more laziness friendly. For example this version of sortBy requires 11 comparisons to find the smallest element of a 15 element list, while the default Data.List.sortBy requires 15 comparisons.

Criterion output (descending/ascending results are for already sorted lists). I barely understand what Criterion does, and I am puzzled with the various "T" output - maybe there is a bug in my bench code:

::

    vagrant@vagrant-ubuntu-trusty-64:/vagrant$ stack exec ghc-sort
    benchmarking ascending ints/ghc
    TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTtime                 160.6 ms   (153.4 ms .. 167.8 ms)
                         0.997 R²   (0.986 R² .. 1.000 R²)
    mean                 161.7 ms   (158.3 ms .. 165.9 ms)
    std dev              5.210 ms   (3.193 ms .. 7.006 ms)
    variance introduced by outliers: 12% (moderately inflated)
    
    benchmarking ascending ints/greg
    TTTTTTTTTTTTTTTTtime                 473.8 ms   (398.6 ms .. 554.9 ms)
                         0.996 R²   (0.987 R² .. 1.000 R²)
    mean                 466.2 ms   (449.0 ms .. 475.0 ms)
    std dev              14.94 ms   (0.0 s .. 15.29 ms)
    variance introduced by outliers: 19% (moderately inflated)
    
    benchmarking descending ints/ghc
    TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTtime                 165.1 ms   (148.2 ms .. 178.2 ms)
                         0.991 R²   (0.957 R² .. 1.000 R²)
    mean                 158.7 ms   (154.0 ms .. 164.3 ms)
    std dev              7.075 ms   (4.152 ms .. 9.903 ms)
    variance introduced by outliers: 12% (moderately inflated)
    
    benchmarking descending ints/greg
    TTTTTTTTTTTTTTTTtime                 471.7 ms   (419.8 ms .. 508.3 ms)
                         0.999 R²   (0.995 R² .. 1.000 R²)
    mean                 476.0 ms   (467.5 ms .. 480.0 ms)
    std dev              7.447 ms   (67.99 as .. 7.865 ms)
    variance introduced by outliers: 19% (moderately inflated)
    
    benchmarking random ints/ghc
    TTTTTTTTTTTTTTTTtime                 2.852 s    (2.564 s .. 3.019 s)
                         0.999 R²   (0.997 R² .. 1.000 R²)
    mean                 2.812 s    (2.785 s .. 2.838 s)
    std dev              44.06 ms   (543.9 as .. 44.97 ms)
    variance introduced by outliers: 19% (moderately inflated)
    
    benchmarking random ints/greg
    TTTTTTTTTTTTTTTTtime                 2.032 s    (1.993 s .. 2.076 s)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 2.028 s    (2.019 s .. 2.033 s)
    std dev              7.832 ms   (0.0 s .. 8.178 ms)
    variance introduced by outliers: 19% (moderately inflated)
    
    benchmarking shakespeare/ghc
    TTTTTTTTTTTTTTTTtime                 6.504 s    (6.391 s .. 6.694 s)
                         1.000 R²   (1.000 R² .. 1.000 R²)
    mean                 6.499 s    (6.468 s .. 6.518 s)
    std dev              28.85 ms   (0.0 s .. 32.62 ms)
    variance introduced by outliers: 19% (moderately inflated)
    
    benchmarking shakespeare/greg
    TTTTTTTTTTTTTTTTtime                 5.560 s    (5.307 s .. 5.763 s)
                         1.000 R²   (0.999 R² .. 1.000 R²)
    mean                 5.582 s    (5.537 s .. 5.607 s)
    std dev              39.30 ms   (0.0 s .. 43.49 ms)
    variance introduced by outliers: 19% (moderately inflated)



Costs and Drawbacks
-------------------

The only cost I see is the reduced performance when sorting already sorted lists. However, since this remains quite efficient, indeed over 4 times faster than sorting unsorted lists, I think it is an acceptable tradeoff.


Alternatives
------------

Keep the current implementation. I think it would be a shame though :-)


Unresolved questions
--------------------


Implementation Plan
-------------------

