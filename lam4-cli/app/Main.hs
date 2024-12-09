{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Base
import           Base.Aeson                    (encodePretty)
import qualified Base.ByteString               as BL
import           Base.File
import           Control.Lens.Regex.ByteString (groups, regex)
import           Data.ByteString               as BS hiding (concat, concatMap,
                                                      map, null, putStr)
import           Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)

import           Cradle
import qualified Lam4.Expr.ConcreteSyntax      as CST (Decl)
import           Lam4.Expr.ExtractProgramInfo
import           Lam4.Expr.Parser              (parseProgramByteStr)
import           Lam4.Expr.ToConcreteEvalAST   (cstProgramToConEvalProgram)
import           Lam4.Expr.ToSimala            ()
import qualified Lam4.Expr.ToSimala            as ToSimala
import           Lam4.Parser.Monad             (evalParserFromScratch)
import           Lam4.Render.Render            (NLGConfig (..), NLGOutput (..))
import qualified Lam4.Render.Render            as Render
import           Options.Applicative           as Options
import           System.Directory
import           System.FilePath               ((</>))
import           System.Environment.Blank      (getEnv)

------------------------------
  -- The key config types
-------------------------------

data FrontendConfig =
  MkFrontendConfig { frontendDir :: FilePath
                   , runner      :: String
                   , args        :: [String]
  }

data OutputConfig =
  MkOutputConfig { simalaOutputConfig :: SimalaOutputConfig
                 , nlgOutputConfig    :: NLGConfig  }

data SimalaOutputConfig =
  MkSimalaOutputConfig {
    outputDir           :: FilePath
  , programFilename     :: FilePath
  , programInfoFilename :: FilePath
}

----------------------------------
  -- Constants / default values
----------------------------------

-- TODO: Most of the following should be put in, and read from, the .env file

lam4FrontendDir :: FilePath
lam4FrontendDir = "lam4-frontend"

frontendConfig :: FrontendConfig
frontendConfig = MkFrontendConfig { runner      = "node"
                                  , frontendDir = lam4FrontendDir
                                  , args        = [lam4FrontendDir </> "bin" </> "cli", "toMinimalAst"] }

defaultSimalaOutputConfig :: SimalaOutputConfig
defaultSimalaOutputConfig = MkSimalaOutputConfig {
    outputDir = "generated" </> "simala"
  , programFilename = "output.simala"
  , programInfoFilename = "program_info.json" }

defaultNlgConfig :: NLGConfig
defaultNlgConfig = MkNLGConfig {
    outputDir = "generated" </> "nlg_en"
  , outputFilename = "nlg_en_output.json"
  , pgfFilename = "Lam4.pgf"
  , concreteSyntaxName = "Lam4Eng"
}

--------------------
  -- CLI Options
--------------------

-- TODO: Think about exposing a tracing option?
data Options =
  MkOptions
    { tracing :: ToSimala.TraceMode
    , files   :: [FilePath]
    , withNlg :: Bool
    }

-- | Copied from Simala
toTracingMode :: String -> ToSimala.TraceMode
toTracingMode "full"    = ToSimala.TraceFull
toTracingMode "results" = ToSimala.TraceResults
toTracingMode "off"     = ToSimala.TraceOff
toTracingMode _         = ToSimala.TraceFull


optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> (toTracingMode <$> strOption (long "tracing" <> help "Tracing, one of \"off\", \"full\" (default), \"results\"") <|> pure ToSimala.TraceResults)
  <*> many (strArgument (metavar ".l4 FILES..."))
  <*> switch (long "with-nlg" <> help "Generate (and serialize to JSON) natural language output")

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header "Lam4 (an experimental variant of the L4 legal DSL)"
    )

-- | Load output configs from environment variables if .env present; use default configs if not
loadConfigsFromEnv :: IO OutputConfig
loadConfigsFromEnv = do
  -- Try to load .env file, warn if not found
  _ <- onMissingFile
    (loadFile defaultConfig)
    (hPutStrLn stderr "Warning: .env file not found, using default configuration")

  mSimalaOutDir   <- getEnv "COMPILED_SIMALA_OUTPUT_DIR"
  mNlgEnOutDir    <- getEnv "NLG_EN_OUTPUT_DIR"
  mGfPgfFilename  <- getEnv "GF_PORTABLE_GRAMMAR_FORMAT_FILENAME"
  mNlgOutFilename <- getEnv "NLG_EN_OUTPUT_FILENAME"

  let simalaOutConfig = MkSimalaOutputConfig {
                                               outputDir = fromMaybe defaultSimalaOutputConfig.outputDir mSimalaOutDir
                                             , programFilename = defaultSimalaOutputConfig.programFilename          -- TODO: move to .env in the future (and update the lsp client accordingly)
                                             , programInfoFilename = defaultSimalaOutputConfig.programInfoFilename -- TODO: move to .env in the future (and update the lsp client accordingly)
                                             }
      nlgOutConfig = MkNLGConfig { outputDir = fromMaybe defaultNlgConfig.outputDir mNlgEnOutDir
                                 , outputFilename = fromMaybe defaultNlgConfig.outputFilename mNlgOutFilename
                                 , pgfFilename = fromMaybe defaultNlgConfig.pgfFilename mGfPgfFilename
                                 , concreteSyntaxName = defaultNlgConfig.concreteSyntaxName -- TODO: Figure out a better way to handle this
                                 }
  pure $ MkOutputConfig { simalaOutputConfig = simalaOutConfig
                        , nlgOutputConfig = nlgOutConfig }

main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files
    then do
      hPutStrLn stderr "Lam4: no input files given; use --help for help"
    else do
      frontendCSTJsons <- getCSTJsonFromFrontend frontendConfig options.files
      let cstProgram       = concatMap parseCSTByteString frontendCSTJsons
          conEvalProgram = cstProgramToConEvalProgram cstProgram
          simalaProgram  = ToSimala.compile conEvalProgram
          programInfo    = extractProgramInfo conEvalProgram

      -- Load output configs
      outConfig <- loadConfigsFromEnv
      let simalaOutConfig = outConfig.simalaOutputConfig
          nlgOutConfig    = outConfig.nlgOutputConfig

      -- Create output directory and write files first
      createDirectoryIfMissing True outConfig.simalaOutputConfig.outputDir
      -- save simala program and program info to disk
      writeFileUtf8 (simalaOutConfig.outputDir </> simalaOutConfig.programFilename) (ToSimala.render simalaProgram)
      writeFileLBS (simalaOutConfig.outputDir </> simalaOutConfig.programInfoFilename) (encodePretty programInfo)

      -- NLG
      -- TODO: Make a ToNLG monad
      when options.withNlg $ do
        nlgEnv <- Render.makeNLGEnv nlgOutConfig
        let nlRendering = Render.renderCstProgramToNL nlgEnv cstProgram
            nlgOutput = MkNLGOutput { sourceFilenames = options.files,
                                      naturalLanguage = nlRendering}
        createDirectoryIfMissing True nlgOutConfig.outputDir
        writeFileLBS (nlgOutConfig.outputDir </> nlgOutConfig.outputFilename) (encodePretty nlgOutput)

      -- Perform evaluation (if needed)
      _ <- ToSimala.doEvalDeclsTracing options.tracing ToSimala.emptyEnv simalaProgram
      pure ()

getCSTJsonFromFrontend :: FrontendConfig -> [FilePath] -> IO [ByteString]
getCSTJsonFromFrontend config files = do
  let argsForFrontendCLI = config.args <> files
  (exitCode :: ExitCode, StdoutRaw rawstdout, StderrRaw err) <- run $ cmd config.runner
    & addArgs argsForFrontendCLI
  case exitCode of
    ExitFailure _ ->
      error ("Frontend parser failed:\n" <> ppShow err)
      -- TODO: Improve the error reporting
    ExitSuccess ->
      pure $ concat $ rawstdout ^.. cstJsonTraversal
        where
          cstJsonTraversal = traversalVL [regex|(?s)<ast_from_frontend>(.*?)</ast_from_frontend>|] % traversalVL groups

parseCSTByteString :: StrictByteString -> [CST.Decl]
parseCSTByteString bs =
  case evalParserFromScratch . parseProgramByteStr . BL.fromStrict $ bs of
    Left err       -> error ("Parse error:\n" <> ppShow err)
    Right cstDecls -> cstDecls


parseProgramFile :: FilePath -> IO [CST.Decl]
parseProgramFile file = do
  bsFile <- BS.readFile file
  pure $ parseCSTByteString bsFile
