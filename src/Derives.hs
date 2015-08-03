{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Derives where
-- split out to improve recompilation times

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Language.Haskell.Exts as HSE

import Data.Typeable

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version

deriving instance Read TH.Loc
deriving instance Read TH.Module
deriving instance Read TH.PkgName
deriving instance Read TH.ModName

deriving instance Read HSE.Module
deriving instance Read HSE.Name
deriving instance Read HSE.Type
deriving instance Read HSE.Decl
deriving instance Read HSE.ModuleName
deriving instance Read HSE.Splice
deriving instance Read HSE.Promoted
deriving instance Read HSE.BangType
deriving instance Read HSE.Boxed
deriving instance Read HSE.TypeEqn
deriving instance Read HSE.TyVarBind
deriving instance Read HSE.Exp
deriving instance Read HSE.QName
deriving instance Read HSE.Safety
deriving instance Read HSE.Kind
deriving instance Read HSE.XName
deriving instance Read HSE.Rule
deriving instance Read HSE.XAttr
deriving instance Read HSE.SpecialCon
deriving instance Read HSE.Rhs
deriving instance Read HSE.Stmt
deriving instance Read HSE.RuleVar
deriving instance Read HSE.QualConDecl
deriving instance Read HSE.QualStmt
deriving instance Read HSE.Overlap
deriving instance Read HSE.QOp
deriving instance Read HSE.Activation
deriving instance Read HSE.Op
deriving instance Read HSE.Literal
deriving instance Read HSE.GuardedRhs
deriving instance Read HSE.Match
deriving instance Read HSE.IPName
deriving instance Read HSE.Pat
deriving instance Read HSE.InstDecl
deriving instance Read HSE.FieldUpdate
deriving instance Read HSE.ConDecl
deriving instance Read HSE.FunDep
deriving instance Read HSE.Bracket
deriving instance Read HSE.Asst
deriving instance Read HSE.ClassDecl
deriving instance Read HSE.Alt
deriving instance Read HSE.Binds
deriving instance Read HSE.CallConv
deriving instance Read HSE.Sign
deriving instance Read HSE.GadtDecl
deriving instance Read HSE.BooleanFormula
deriving instance Read HSE.RPat
deriving instance Read HSE.DataOrNew
deriving instance Read HSE.Assoc
deriving instance Read HSE.PatField
deriving instance Read HSE.IPBind
deriving instance Read HSE.Annotation
deriving instance Read HSE.PXAttr
deriving instance Read HSE.RPatOp
deriving instance Read HSE.SrcLoc
deriving instance Read HSE.WarningText
deriving instance Read HSE.ModulePragma
deriving instance Read HSE.Tool
deriving instance Read HSE.ImportDecl
deriving instance Read HSE.ImportSpec
deriving instance Read HSE.Namespace
deriving instance Read HSE.CName
deriving instance Read HSE.ExportSpec
deriving instance (Read a) => Read (Condition a)
deriving instance Read ConfVar
deriving instance (Read a,Read b,Read c) => Read (CondTree a b c)
deriving instance Read Flag
deriving instance Read GenericPackageDescription

deriving instance Typeable HSE.Decl
