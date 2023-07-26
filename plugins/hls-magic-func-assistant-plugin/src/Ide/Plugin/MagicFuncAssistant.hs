{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ide.Plugin.MagicFuncAssistant
  ( descriptor,
    descriptorForModules,
    sentenceSorting,
    within,
    Log (..),
  )
where

import Control.DeepSeq
--高倉がhidingした

import Control.Exception (handle)
import Control.Monad.IO.Class
import Data.Aeson
  ( ToJSON (toJSON),
    Value (Null),
  )
import Data.Aeson.Types (FromJSON)
import qualified Data.Char as C
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    isJust,
    listToMaybe,
  )
import Data.String (fromString)
import Data.String.ToString (ToString (toString))
import qualified Data.String.ToString as ST
import qualified Data.Text as T
import Development.IDE hiding
  ( pluginHandlers,
    pluginRules,
  )
import qualified Development.IDE as TcModuleParsed
import qualified Development.IDE as TypeCheck
import Development.IDE.Core.PositionMapping
import qualified Development.IDE.Core.Shake as Shake
import Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat as ParsedModule
import qualified Development.IDE.GHC.Compat as TypeCheck
import Development.IDE.Graph.Classes
import Development.IDE.Types.Logger as Logger (Pretty (pretty))
import GHC.Generics (Generic)
import Ide.PluginUtils (mkLspCommand)
import Ide.Types hiding (Config)
import Language.LSP.Server hiding (defaultConfig)
import Language.LSP.Types
import Network
import System.IO
import System.IO.Error (catchIOError)
import Text.HTML.TagSoup

--import           MagicHaskeller.ExpToHtml
--import Language.Haskell.Parser
--import Language.Haskell.Syntax

mtCommandId :: CommandId
mtCommandId = "MagicTesterCommand"

newtype Log
  = LogShake Shake.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

-- | The "main" function of a plugin
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder =
  -- (almost) no one wants to see an explicit import list for Prelude
  descriptorForModules recorder

descriptorForModules ::
  Recorder (WithPriority Log) ->
  PluginId ->
  PluginDescriptor IdeState
descriptorForModules recorder plId =
  (defaultPluginDescriptor plId)
    { -- This plugin provides a command handler
      pluginCommands = [importLensCommand],
      -- This plugin defines a new rule
      pluginRules = rule recorder,
      pluginHandlers =
        mconcat
          [ -- This plugin provides code lenses
            mkPluginHandler STextDocumentCodeLens lensProvider
            -- This plugin provides code actions
            --, mkPluginHandler STextDocumentCodeAction $ codeActionProvider
          ]
    }

-- | The command descriptor
importLensCommand :: PluginCommand IdeState
importLensCommand =
  PluginCommand mtCommandId "Magic tester command" runImportCommand

-- | The type of the parameters accepted by our command
newtype MTCommandParams = MTCommandParams WorkspaceEdit
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runImportCommand :: CommandFunction IdeState MTCommandParams
runImportCommand _state (MTCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right Null)

-- | For every implicit import statement, return a code lens of the corresponding explicit import
-- Example - for the module below:
--
-- > import Data.List
-- >
-- > f = intercalate " " . sortBy length
--
-- the provider should produce one code lens associated to the import statement:
--
-- > import Data.List (intercalate, sortBy)
lensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider
  state -- ghcide state, used to retrieve typechecking artifacts
  pId -- plugin Id
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
      do
        mbMinfuncs <- runAction "Minimalfuncs" state $ useWithStale Minimalfuncs nfp
        case mbMinfuncs of
          -- Implement the provider logic:
          -- for every import, if it's lacking a explicit list, generate a code lens
          Just (MinimalfuncsResult results, posMapping) -> do
            results2 <-
              sequence
                [ mktuple hsdec valtext
                  | (hsdec, Just valtext) <- results
                ]
            --hMap <- catchIOError readfile (\e -> return "")
            hMap <- catchIOError (readFile hashlogfile) (\e -> return "")
            let hashlog = stringTohashmap hMap
            (edits, reloadhMap) <- mhEdits posMapping results2 hashlog
            --writefile (show reloadhMap)
            writeFile hashlogfile (show reloadhMap)
            commands <- mapM (generateLens pId _uri) (concat $ catMaybes edits)
            return $ Right (List $ catMaybes commands)
          _ ->
            return $ Right (List [])
    | otherwise =
      return $ Right (List [])

data Minimalfuncs = Minimalfuncs
  deriving (Show, Generic, Eq, Ord)

instance Hashable Minimalfuncs

instance NFData Minimalfuncs

type instance RuleResult Minimalfuncs = MinimalfuncsResult

newtype MinimalfuncsResult = MinimalfuncsResult
  {getMinimalfuncsResult :: [(LHsDecl GhcPs, Maybe T.Text)]}

instance Show MinimalfuncsResult where show _ = "<minimalfuncsResult>"

instance NFData MinimalfuncsResult where rnf = rwhnf

rule :: Recorder (WithPriority Log) -> Rules ()
rule recorder = define (cmapWithPrio LogShake recorder) $ \Minimalfuncs nfp -> do
  -- Get the typechecking artifacts from the module
  tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- use GhcSessionDeps nfp
  -- Use the GHC api to extract the "minimal" imports
  (funcs, magicHmatch) <- liftIO $ sentenceSorting hsc tmr

  let importsMap =
        Map.fromList
          [ (realSrcSpanStart l, printOutputable i)
            | L (locA -> RealSrcSpan l _) i <- fromMaybe [] magicHmatch
            --, not (isImplicitPrelude i)
          ]
      res =
        [ (i, Map.lookup (realSrcSpanStart l) importsMap)
          | i <- funcs,
            RealSrcSpan l _ <- [getLoc i]
        ]

  return ([], MinimalfuncsResult res <$ magicHmatch)

--------------------------------------------------------------------------------

-- | Use the ghc api to extract a minimal, explicit set of imports for this module
sentenceSorting ::
  Maybe HscEnvEq ->
  Maybe TcModuleResult ->
  IO ([LHsDecl GhcPs], Maybe [LHsDecl GhcPs])
sentenceSorting (Just hsc) (Just TcModuleResult {..}) = do
  -- extract the original imports and the typechecking environment
  let tcEnv = tmrTypechecked
      ParsedModule {pm_parsed_source = L loc hsm} = tmrParsed
      funcs = hsmodDecls hsm
      span = fromMaybe (error "expected real") $ realSpan loc

  let magicHmatch = mHmatch funcs

  return (funcs, magicHmatch)
sentenceSorting _ _ = return ([], Nothing)

mktuple :: LHsDecl GhcPs -> T.Text -> IO (LHsDecl GhcPs, T.Text)
mktuple hsdec valtext = return (hsdec, valtext)

mhEdits :: PositionMapping -> [(LHsDecl GhcPs, T.Text)] -> HashMap.HashMap String String -> IO ([Maybe [TextEdit]], HashMap.HashMap String String)
mhEdits posMapping [] m = return ([], m)
mhEdits posMapping (x : xs) m = do
  (y, m') <- mhEdit posMapping x m
  (ys, m'') <- mhEdits posMapping xs m'
  return (y : ys, m'')

mhEdit :: PositionMapping -> (LHsDecl GhcPs, T.Text) -> HashMap.HashMap String String -> IO (Maybe [TextEdit], HashMap.HashMap String String)
mhEdit posMapping (L (locA -> src) _, body) hMap
  | checkBool $ ST.toString body,
    RealSrcSpan l _ <- src,
    Just rng <- toCurrentRange posMapping $ realSrcSpanToRange l =
    do
      let msg = ST.toString body
          funcname = head $ words msg
          ckmsg = checkMsg msg
      (resulttext, newhMap) <- magichaskeller ckmsg hMap
      return (mkRcvToEdit rng funcname resulttext, newhMap)
  | otherwise = return (Nothing, hMap)

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens :: PluginId -> Uri -> TextEdit -> IO (Maybe CodeLens)
generateLens pId uri funcEdit@TextEdit {_range, _newText} = do
  --p <- runAction "Minimalfuncs" state $ use TypeCheck nfp
  let title = abbreviateTitle _newText
      -- the code lens has no extra data
      _xdata = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing Nothing
      editsMap = HashMap.fromList [(uri, List [funcEdit])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ MTCommandParams edit]
      -- create the command
      _command = Just $ mkLspCommand pId mtCommandId title _arguments
  -- create and return the code lens
  return $ Just CodeLens {..}

-- This number is somewhat arbitrarily chosen. Ideally the protocol would tell us these things,
-- but at the moment I don't believe we know it.
-- 80 columns is traditional, but Haskellers tend to use longer lines (citation needed) and it's
-- probably not too bad if the lens is a *bit* longer than normal lines.
-- でも一旦80にする。
maxColumns :: Int
maxColumns = 80

-- we don't want to create a really massive code lens (and the decl can be extremely large!).
--ExplicitImport.hsのabbreviateTitleを参考にした
--------------------------------------------------------------------------------
abbreviateTitle :: T.Text -> T.Text
abbreviateTitle input =
  let -- For starters, we only want one line in the title
      oneLineText = T.unwords $ T.lines input
      -- Now, split at the max columns, leaving space for the summary text we're going to add
      -- (conservatively assuming we won't need to print a number larger than 100)
      (prefix, suffix) = T.splitAt (maxColumns - T.length (summaryText 100)) oneLineText
      -- We also want to truncate the last item so we get a "clean" break, rather than half way through
      -- something. The conditional here is just because 'breakOnEnd' doesn't give us quite the right thing
      -- if there are actually no commas.
      (actualPrefix, extraSuffix) = if T.count "," prefix > 0 then T.breakOnEnd "," prefix else (prefix, "")
      actualSuffix = extraSuffix <> suffix
      -- We also want it to look sensible if we end up splitting in the module name itself,
      summaryText n = " ... (" <> fromString (show n) <> " items)"
      title =
        -- If the original text fits, just use it
        if T.length oneLineText <= maxColumns
          then oneLineText
          else actualPrefix
   in title

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde = runAction "importLens"

within :: Range -> SrcSpan -> Bool
within (Range start end) span =
  isInsideSrcSpan start span || isInsideSrcSpan end span

mHmatch :: [LHsDecl GhcPs] -> Maybe [LHsDecl GhcPs]
mHmatch [] = Nothing
mHmatch lhsdecllist = Just (filter mHmatch2 lhsdecllist)

mHmatch2 :: LHsDecl GhcPs -> Bool
mHmatch2 (L _ (ValD _ (FunBind _ _ _ _ _))) = True
mHmatch2 _ = False

-------------------------------------------------------------
--in lensProvider
stringTohashmap :: String -> HashMap.HashMap String String
-- stringTohashmap [] = HashMap.empty
-- stringTohashmap hMap = read hMap
stringTohashmap hMap = case reads hMap of
  (hm, "") : _ -> hm
  [] -> HashMap.empty

hashlogfile = "/home/takakura/hlsversion/hls-1.9/Hashlog.txt"

readfile :: IO String
readfile = do
  handle <- openFile hashlogfile ReadMode
  contents <- hGetContents handle
  hClose handle
  return contents

writefile :: String -> IO ()
writefile contents = do
  handle <- openFile hashlogfile WriteMode
  hPutStr handle contents
  hClose handle

---------------------------------------------------------------

data Config = C
  { portID :: PortID,
    hostname :: String
  }
  deriving (Read)

-- Unfortunately, the Read instance of PortID is not defined in the library!
instance Read PortID where
  readsPrec _ str = case lex str of
#ifdef UNIX
    [("UnixSocket", strrest)] -> [ (UnixSocket str, rest) | (str, rest) <- reads strrest ]
#endif
    [("PortNumber", numrest)] -> [(PortNumber $ fromInteger num, rest) | (num, rest) <- reads numrest]
    _ -> []

defaultConfig :: Config
defaultConfig =
  C
    { portID = PortNumber 55443,
      hostname = "133.54.228.36"
    }

magichaskeller :: String -> HashMap.HashMap String String -> IO (String, HashMap.HashMap String String)
magichaskeller ckmsg hMap
  | Just hmval <- HashMap.lookup ckmsg hMap =
    return (hmval, hMap)
  | otherwise = do
    htmlval <- sendMessage defaultConfig ckmsg
    let mhvalDep = getValueFromHTML htmlval
        mhval = depthToCount mhvalDep
        mHval = unlines (takeParts mhval)
        newhMap = HashMap.insert ckmsg mHval hMap
    return (mHval, newhMap)

sendMessage :: Config -> String -> IO String
sendMessage config msg = do
  handle <- connectTo (hostname config) (portID config)
  hSetBuffering handle LineBuffering
  hPutStrLn handle msg
  hGetContents handle

--Send products
checkBool :: String -> Bool
checkBool = not . any (all C.isSpace . take 1) . lines

checkMsg :: String -> String
checkMsg = let g '\n' = "&&"; g '=' = "~="; g c = [c] in concatMap g . L.dropWhileEnd (== '\n') . unlines . map (\xs -> case lex xs of [(_, dr)] -> 'f' : dr) . filter (not . null) . lines

--Receive products
mkRcvToEdit :: Range -> String -> String -> Maybe [TextEdit]
mkRcvToEdit rng funcname mhval
  | '!' /= head mhval =
    Just $ map (TextEdit rng . (T.pack . appendRcv funcname)) (filter (/= "") (lines mhval))
  | otherwise = Nothing

takeParts :: String -> [String]
takeParts = take 10 . lines

appendRcv :: String -> String -> String
appendRcv fname mhval = fname ++ " = " ++ mhval

-- appendRcv fname mhval = fname ++ argumentStr mhval ++ " =" ++ removeHeadTail mhval

-- HTMLからvalueの値を取得する関数
getValueFromHTML :: String -> String
getValueFromHTML html =
  let tags = parseTags html
      -- candidateTag :: Tag String
      -- candidateTag = TagOpen "input" [("type", "hidden"), ("name", "candidate")]
      valueStr = innerText tags
   in -- valueAttr = fromAttrib "value" $ head $ filter (candidateTag ~==) tags
      valueStr

depthToCount :: String -> String
depthToCount mhvalDep = L.intercalate "\n" $ splitOn "\160 f \160 = \160 " mhvalDep

argumentStr :: String -> String
argumentStr str = ST.toString $ takeWhile (/= '-') $ toList $ drop 2 str

removeHeadTail :: String -> String
removeHeadTail str = ST.toString $ init $ drop 1 $ dropWhile (/= '>') $ toList str

-- --takeWhileがStringに使えない問題の対応処置
-- takeNotEqWhile' :: a -> [a] -> [a]
-- takeNotEqWhile' _ [] = []
-- takeNotEqWhile' p (x : xs)
--   | p /= x = x : takeNotEqWhile' p xs
--   | otherwise = []

-- --dropWhileがStringに使えない問題の対応処置
-- dropWhile' :: (a -> Bool) -> [a] -> [a]
-- dropWhile' _ [] = []
-- dropWhile' p (x : xs)
--   | p x = x : dropWhile' p xs
--   | otherwise = []

takakura 5 = 120

nakane "abc" 2 = "aabbcc"
