{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Implementation based on https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree
module Service.MerkleTree where

import Ext.PlutusTx.List (drop, replicate)
import Ext.PlutusTx.Numeric ((^))
import GHC.Generics (Generic)
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

-- | A type for representing hash digests.
type Hash = BuiltinByteString

-- | Counter of leaves in a tree, should start from 0 for empty tree
type NextInsertionCounter = Integer

-- | From root to leaf, False - go left, True - go right
type MerkleProofPath = [Bool]

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
hash :: BuiltinByteString -> Hash
hash = sha2_256
{-# INLINEABLE hash #-}

-- | Combines two hashes digest into a new one. This is effectively a new hash
-- digest of the same length.
combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

checkHashLength :: Hash -> Bool
checkHashLength = (== 32) . lengthOfByteString
{-# INLINEABLE checkHashLength #-}

data MerkleTreeConfig = MerkleTreeConfig
  { zeroRoot :: Hash, -- zero root digest
    zeroLeaf :: Hash,
    height :: !Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MerkleTreeConfig

PlutusTx.makeIsDataIndexed
  ''MerkleTreeConfig
  [('MerkleTreeConfig, 0)]

-- | A MerkleTree representation, suitable for on-chain manipulation.
data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash !MerkleTree !MerkleTree
  | MerkleLeaf Hash
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

instance Eq MerkleTree where
  {-# INLINEABLE (==) #-}
  (MerkleLeaf h0) == (MerkleLeaf h1) = h0 == h1
  (MerkleNode h0 l1 r1) == (MerkleNode h1 l2 r2) =
    if h0 == h1
      then (if l1 == l2 then r1 == r2 else False)
      else False
  MerkleEmpty == MerkleEmpty = True
  _ == _ = False

PlutusTx.makeLift ''MerkleTree

PlutusTx.makeIsDataIndexed
  ''MerkleTree
  [('MerkleEmpty, 0), ('MerkleNode, 1), ('MerkleLeaf, 2)]

data MerkleTreeState = MerkleTreeState
  { nextLeaf :: !NextInsertionCounter,
    tree :: !MerkleTree
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

instance Eq MerkleTreeState where
  {-# INLINEABLE (==) #-}
  MerkleTreeState nl1 t1 == MerkleTreeState nl2 t2 = nl1 == nl2 && t1 == t2

PlutusTx.makeLift ''MerkleTreeState

PlutusTx.makeIsDataIndexed
  ''MerkleTreeState
  [('MerkleTreeState, 0)]

-- | Make empty Merkle Tree, filling leaves with MerkleEmpty
-- and other nodes with zeros - list of zero digests starting from root, excluding zero leaf
mkEmptyMT :: Integer -> Hash -> MerkleTree
mkEmptyMT height zeroLeaf = goCreate zeros
  where
    goCreate (h : hs) = MerkleNode h (goCreate hs) (goCreate hs)
    goCreate [] = MerkleEmpty
    zeros = reverse $ go height zeroLeaf
    go 0 _ = []
    go i h =
      let new = combineHash h h
       in new : go (i - 1) new
{-# INLINEABLE mkEmptyMT #-}

-- | Get zero root for tree of specific height and zero leaf
calculateZeroRoot :: Integer -> Hash -> Hash
calculateZeroRoot h zeroHash
  | h == 0 = zeroHash
  | otherwise =
    let new = combineHash zeroHash zeroHash
     in calculateZeroRoot (h - 1) new
{-# INLINEABLE calculateZeroRoot #-}

-- | Counter of next inserted item is converted to Merkle Path.
-- A number in binary could be considered as path in a binary tree,
-- e.g. for a tree of height 4: 8th insertion in binary is [True,False,False,False],
-- which is interpreted as "to find where to insert new leaf go to [right,left,left,left]"
counterToPath :: Integer -> NextInsertionCounter -> MerkleProofPath
counterToPath !h !n =
  if n == 0
    then zeroArr
    else
      ( if n > 2 ^ h
          then traceError "Merkle tree is full"
          else drop (length binaryN) zeroArr <> binaryN
      )
  where
    !zeroArr = replicate h False
    !binaryN = reverse $ go n
    go !k =
      if k == 0
        then []
        else
          let (d, m) = divMod k 2
           in if m == 0
                then False : go d
                else True : go d
{-# INLINEABLE counterToPath #-}

-- | Traverse a tree according to Merkle Path saving subtrees, which are complementary to the path
splitByPathMT :: MerkleProofPath -> MerkleTree -> [MerkleTree]
splitByPathMT !path !tree = subtrees
  where
    (_, !subtrees) = goSplit (tree, []) path
    goSplit res [] = res
    goSplit (tr, acc) (p : ps) = case tr of
      MerkleNode _ l r ->
        if p
          then goSplit (r, l : acc) ps
          else goSplit (l, r : acc) ps
      t -> goSplit (t, acc) ps
{-# INLINEABLE splitByPathMT #-}

-- | Starting from inserted leaf compose new Merkle Tree from Merkle Path subtrees,
-- and rehash all path elements
composeByPathMT :: Hash -> MerkleTree -> [(Bool, MerkleTree)] -> MerkleTree
composeByPathMT _ !tree [] = tree
composeByPathMT zl !tree (e : es) = composeByPathMT zl (mkCompose tree e) es
  where
    mkCompose :: MerkleTree -> (Bool, MerkleTree) -> MerkleTree
    mkCompose l@(MerkleLeaf h) (_, MerkleEmpty) =
      MerkleNode (combineHash h zl) l MerkleEmpty
    mkCompose r@(MerkleLeaf hr) (_, l@(MerkleLeaf hl)) =
      MerkleNode (combineHash hl hr) l r
    mkCompose acc@(MerkleNode hacc _ _) (p, el@(MerkleNode hel _ _)) =
      if p
        then MerkleNode (combineHash hel hacc) el acc
        else MerkleNode (combineHash hacc hel) acc el
    mkCompose _ _ = traceError "Not consistent Merkle Tree composition"
{-# INLINEABLE composeByPathMT #-}

-- | insert is done off-chain first, it returns new MerkleTree (it should be a part of contract state)
-- it is then checked on-chain: newMerkleTree == insert depositedCommitment oldMerkleTree
insert :: MerkleTreeConfig -> Hash -> MerkleTreeState -> MerkleTreeState
insert config commitment !inputState = MerkleTreeState outputCounter outputTree
  where
    !h = height config
    zl = zeroLeaf config
    !next = nextLeaf inputState
    !path = counterToPath h next
    !inputTree = tree inputState
    !subTrees = splitByPathMT path inputTree
    !newLeaf = MerkleLeaf commitment
    !pathAndSubtrees = zip (reverse path) subTrees
    !outputTree = composeByPathMT zl newLeaf pathAndSubtrees
    !outputCounter = succ next
{-# INLINEABLE insert #-}

-- | Returns current root if Merkle Tree is not empty
getRoot :: MerkleTreeConfig -> MerkleTree -> Maybe Hash
getRoot conf tree =
  let zr = zeroRoot conf
   in case tree of
        MerkleNode root _ _ ->
          if root == zr then Nothing else Just root
        _ -> Nothing
{-# INLINEABLE getRoot #-}

nonEmptyLeafs :: MerkleTree -> [Hash]
nonEmptyLeafs (MerkleNode _ l r) = nonEmptyLeafs l <> nonEmptyLeafs r
nonEmptyLeafs (MerkleLeaf h) = [h]
nonEmptyLeafs MerkleEmpty = []
{-# INLINEABLE nonEmptyLeafs #-}
