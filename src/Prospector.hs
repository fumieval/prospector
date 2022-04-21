{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Prospector (plugin) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.Char
import Data.Data

{-
import GHC.Data.IOEnv (IOEnv)
import GHC.Hs
import GHC.Plugins hiding ((<>), getHscEnv, TcPlugin)
import GHC.Tc.Types (TcPlugin(..), TcPluginResult(..))
import Language.Haskell.Syntax.Decls
-}

import SrcLoc (GenLocated(..))
import Outputable
import OccName
import RdrName
import GHC.Hs
import Plugins 
import HscTypes

tyVars :: HsType GhcPs -> [String]
tyVars ty = getConst
    $ gfoldl (\(Const xs) d -> Const $ case d of
        (cast -> Just (L _ rdr :: LIdP GhcPs)) -> [occNameString $ rdrNameOcc rdr]
        (cast -> Just (L _ t :: LHsType GhcPs)) -> tyVars t
        (cast -> Just t) -> tyVars t
        (cast -> Just ts) -> ts >>= tyVars
        (cast -> Just ts) -> ts >>= \(L _ t :: LHsType GhcPs) -> tyVars t
        _ -> []
        ++ xs)
    (const $ Const []) ty

knownTypes :: Set.Set String
knownTypes = Set.fromList $ words
    "Bool Maybe Either Ordering Char String Eq Ord\
    \ Enum Bounded Int Integer Float Double Rational Word Num Real Integral\
    \ Fractional Floating RealFrac RealFloat Semigroup Monoid Functor\
    \ Applicative Monad Foldable Traversable [] Show Read IO\
    \ Set Map HashMap Text ByteString"

isCombinatorSig :: HsType GhcPs -> Bool
isCombinatorSig body = length vars > 1 -- not constant
    && all isKnownType vars
    where
        vars = tyVars body

isKnownType :: String -> Bool 
isKnownType name = all isLower (take 1 name) -- type variable
    || name `Set.member` knownTypes

bindsId :: IdP GhcPs -> HsBind GhcPs -> Bool
bindsId name FunBind{fun_id = L _ x} = name == x
bindsId name VarBind{var_id = x} = name == x
bindsId _ _ = False

plugin :: Plugin
plugin = defaultPlugin
    { parsedResultAction = \args _ hpm -> do
        let outputPath = case args of
                [] -> "prospector.hs"
                x : _ -> x
        let L _ hsm = hpm_module hpm
        let decls = hsmodDecls hsm
        let sigs = [(loc, name, decl)
                | decl@(L loc (SigD _ (TypeSig _ names hswc))) <- decls
                , let HsWC _ (HsIB _ (L _ body)) = hswc
                -- , let HsWC _ (L _ (HsSig _ _ (L _ body))) = hswc
                , isCombinatorSig body
                , L _ name <- names
                , occNameString (rdrNameOcc name) /= "main"
                ]
        unless (null sigs) $ liftIO $ appendFile outputPath
            $ showSDocUnsafe $ vcat
            [ vcat [ppr loc, ppr sig, ppr decl, text ""]
            | (loc, name, sig) <- sigs
            , decl@(L _ (ValD _ bind)) <- decls
            , bindsId name bind]
        pure hpm
    , pluginRecompile = purePlugin
    }