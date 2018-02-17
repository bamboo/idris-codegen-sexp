{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Codegen.SExp
  ( idrisCodegenInfoFor
  , renderSExp
  , evalIdris
  , runIO
  , runMain
  ) where

import           IRTS.CodegenCommon
import           IRTS.Compiler
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.AbsSyntax hiding (prettyName, showName)
import           Idris.Core.CaseTree (CaseType())
import           Idris.Core.TT hiding (Impossible)
import           Idris.ElabDecls (elabPrims, elabMain)
import           Idris.Main
import           Idris.Options (IRFormat(IBCFormat), Codegen(Via))

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad.Writer

import qualified Data.DList as DList
import           Data.Graph
import qualified Data.List as List
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text

import           GHC.Generics (Generic)
import           Text.PrettyPrint.GenericPretty (pretty)
import qualified Text.PrettyPrint.GenericPretty as GenericPretty
import qualified Text.PrettyPrint.Leijen.Text as PP

idrisCodegenInfoFor :: [FilePath] -> FilePath -> Idris CodegenInfo
idrisCodegenInfoFor inputs output = do
  elabPrims
  _ <- loadInputs inputs Nothing
  mainProg <- elabMain
  compile (Via IBCFormat "sexp") output (Just mainProg)

renderSExp :: CodegenInfo -> Text
renderSExp ci = renderPretty doc where
  doc = PP.vcat (List.intersperse PP.line prettyDecls)
  prettyDecls = pretty <$> reachableDecls
  reachableDecls = removeUnreachable decls
  decls          = snd <$> simpleDecls ci

renderPretty :: PP.Doc -> Text
renderPretty = PP.displayTStrict . PP.renderPretty 0.4 120

evalIdris :: Idris a -> IO (Either Err a)
evalIdris prog = runExceptT (evalStateT prog idrisInit)

removeUnreachable :: [SDecl] -> [SDecl]
removeUnreachable decls = decl . fromVertex <$> reachable graph entryPoint where
  decl (d, _, _) = d
  (graph, fromVertex, toVertex) = callGraph decls
  (Just entryPoint)             = toVertex entryPointName

entryPointName :: Name
entryPointName = MN 0 "runMain"

callGraph :: [SDecl] -> (Graph, Vertex -> (SDecl, Name, [Name]), Name -> Maybe Vertex)
callGraph methods = graphFromEdges (edge <$> methods) where
  edge :: SDecl -> (SDecl, Name, [Name])
  edge decl@(SFun name _ _ body) = (decl, name, callees body)

callees :: SExp -> [Name]
callees = DList.toList . execWriter . go where
  go :: SExp -> CalleeWriter
  go (SApp _ name _) = callee name
  go (SLet _ val body) = go val *> go body
  go (SUpdate _ e) = go e
  go (SCase _ _ salts) = forM_ salts salt
  go (SChkCase _ salts) = forM_ salts salt
  go _ = pure ()

  salt :: SAlt -> CalleeWriter
  salt (SConCase _ _ _ _ e) = go e
  salt (SConstCase _ e) = go e
  salt (SDefaultCase e) = go e

  callee :: Name -> CalleeWriter
  callee = tell . DList.singleton

type CalleeWriter
  = Writer (DList.DList Name) ()

{-
module IRTS.Simplified where
...

data SExp = SV LVar
          | SApp Bool Name [LVar]
          | SLet LVar SExp SExp
          | SUpdate LVar SExp
          | SCon (Maybe LVar) -- location to reallocate, if available
                 Int Name [LVar]
          | SCase CaseType LVar [SAlt]
          | SChkCase LVar [SAlt]
          | SProj LVar Int
          | SConst Const
          -- Keep DExps for describing foreign things, because they get
          -- translated differently
          | SForeign FDesc FDesc [(FDesc, LVar)]
          | SOp PrimFn [LVar]
          | SNothing -- erased value, will never be inspected
          | SError String
  deriving Show

data SAlt = SConCase Int Int Name [Name] SExp
          | SConstCase Const SExp
          | SDefaultCase SExp
  deriving Show

data SDecl = SFun Name [Name] Int SExp
  deriving Show

-}

deriving instance (Generic SDecl)
deriving instance (Generic FDesc)
deriving instance (Generic SExp)
deriving instance (Generic SAlt)
deriving instance (Generic LVar)

instance GenericPretty.Pretty SDecl
instance GenericPretty.Pretty SExp
instance GenericPretty.Pretty SAlt
instance GenericPretty.Pretty LVar
instance GenericPretty.Pretty Const
instance GenericPretty.Pretty PrimFn
instance GenericPretty.Pretty FC
instance GenericPretty.Pretty FDesc
instance GenericPretty.Pretty ArithTy
instance GenericPretty.Pretty IntTy
instance GenericPretty.Pretty NativeTy
instance GenericPretty.Pretty CaseType
instance GenericPretty.Pretty Name        where pretty = prettyName
instance GenericPretty.Pretty SpecialName where pretty = prettyShow

prettyName :: Name -> PP.Doc
prettyName = PP.stringStrict . quoted . showName

showName :: Name -> Text
showName (NS n ns) = Text.intercalate "." . reverse $ showName n : ns
showName (UN t)    = t
showName (MN i t)  = Text.concat [t, Text.pack $ show i]
showName (SN sn)   = Text.pack $ show sn
showName e = error $ "Unsupported name `" <> show e <> "'"

quoted :: Text -> Text
quoted name = quote <> name <> quote where
  quote = Text.singleton '"'

prettyShow :: Show a => a -> PP.Doc
prettyShow = PP.stringStrict . Text.pack . show
