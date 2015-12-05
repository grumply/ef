{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.System.Random.Shuffle
    ( shuffle
    ) where

import Ef.Core

import Data.Function
    ( fix )

import System.Random
    ( RandomGen
    , randomR
    , newStdGen
    )



data Tree a
  where

    Leaf
        :: !a
        -> Tree a

    Node
        :: !Int
        -> !(Tree a)
        -> !(Tree a)
        -> Tree a

  deriving Show



shuffle
    :: ( Lift IO parent
       , Monad parent
       )
    => [a]
    -> Pattern scope parent [a]

shuffle xs =
  do
    rng <- lift newStdGen
    let
      lngth =
          length xs

    return (shuffle'_ xs lngth rng)



buildTree
    :: [a]
    -> Tree a

buildTree =
    (fix growLevel) . (map Leaf)
  where

    growLevel _ [node] =
        node

    growLevel slf l =
        slf (inner l)



    inner [] =
        []

    inner [e] =
        [e]

    inner (e1 : e2 : es) =
        e1 `seq` e2 `seq` (join e1 e2) : inner es



    join l@(Leaf _) r@(Leaf _) =
        Node 2 l r

    join l@(Node ct _ _) r@(Leaf _) =
        Node (ct + 1) l r

    join l@(Leaf _) r@(Node ct _ _) =
        Node (ct + 1) l r

    join l@(Node ctl _ _) r@(Node ctr _ _) =
        Node (ctl + ctr) l r



shuffle_
    :: [a]
    -> [Int]
    -> [a]

shuffle_ elements =
    shuffleTree (buildTree elements)
  where

    shuffleTree (Leaf e) [] =
        [e]

    shuffleTree tree (r : rs) =
        let
          (b, rest) =
              extractTree r tree

        in
          b : (shuffleTree rest rs)

    shuffleTree _ _ =
        error "[shuffle] called with lists of different lengths"



    extractTree 0 (Node _ (Leaf e) r) =
        (e, r)

    extractTree 1 (Node 2 (Leaf l) (Leaf r)) =
        (r, Leaf l)

    extractTree n (Node c (Leaf l) r) =
      let
        (e, r') =
            extractTree (n - 1) r

      in
        (e, Node (c - 1) (Leaf l) r')

    extractTree n (Node n' l (Leaf e))
        | n + 1 == n' = (e, l)

    extractTree n (Node c l@(Node cl _ _) r)
        | n < cl =
              let
                (e, l') =
                    extractTree n l

              in
                (e, Node (c - 1) l' r)

        | otherwise =
              let
                (e, r') =
                    extractTree (n - cl) r

              in
                (e, Node (c - 1) l r')

    extractTree _ _ = error "[extractTree] impossible"



shuffle'_
    :: RandomGen gen
    => [a]
    -> Int
    -> gen
    -> [a]

shuffle'_ elements len =
    shuffle_ elements . rseq len
  where

    rseq
        :: RandomGen gen
        => Int
        -> gen
        -> [Int]

    rseq n =
        fst . unzip . rseq' (n - 1)
      where

        rseq'
            :: RandomGen gen
            => Int
            -> gen
            -> [(Int, gen)]

        rseq' 0 _ =
            []

        rseq' i gen =
            (j, gen) : rseq' (i - 1) gen'
          where

            (j, gen') =
                randomR (0, i) gen
