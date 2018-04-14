#! /usr/bin/env nix-shell
-- #! nix-shell deps.nix -i "ghci -fdefer-type-errors"
#! nix-shell shell.nix -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall"
#! nix-shell --pure

module Main where

import Development.Shake
import Development.Shake.FilePath
import           Data.List.Split (chunksOf)
import Data.Traversable (for)
import Control.Monad (unless)
import Data.List (intercalate, isPrefixOf)
import Data.Monoid ((<>))

main :: IO ()
main = runShakeBuild

buildDir :: FilePath
buildDir = "result"

myShakeOptions :: ShakeOptions
myShakeOptions = shakeOptions { shakeLint = Just LintBasic
                              , shakeReport = ["report.html", "report.json"]
                              , shakeThreads = 0
                              , shakeColor = True
                              }

runShakeBuild :: IO ()
runShakeBuild = shakeArgs myShakeOptions $ do
  wantTargets
  phonyCommands
  rules

wantTargets :: Rules ()
wantTargets = do
  want [buildDir </> "slides.pdf"]

phonyCommands :: Rules ()
phonyCommands = do
  phony "clean" (removeFilesAfter buildDir ["//*"])

rules :: Rules ()
rules = do
  buildDir </> "slides.pdf" %> \out -> do
    let inp = out -<.> "tex"
        theme = map (buildDir </>) ["beamercolorthemecodecentric.sty"
                                   ,"beamerfontthemecodecentric.sty"
                                   ,"beamerinnerthemecodecentric.sty"
                                   ,"beamerouterthemecodecentric.sty"
                                   ,"beamerthemecodecentric.sty"
                                   ]
    need (inp : (buildDir </> "mindmap.tex") : (buildDir </> "font.tex") : theme)
    latexmk inp

  buildDir </> "font.tex" %> \_ -> dumpFontFile

  buildDir </> "*.tex" %> \out -> do
    copyFileChanged (dropDirectory1 out) out

  buildDir </> "*.sty" %> \out -> do
    copyFileChanged (dropDirectory1 out) out

  buildDir </> "*.code" %> \out -> do
    snip <- extractSnippet (out -<.> "snippet")
    writeFileLines out snip


latexmk :: FilePath -> Action ()
latexmk inp = do
  cmd [Cwd (takeDirectory inp)
      ,WithStdout True
      ,EchoStdout False
      ,EchoStderr False
      ,Stdin ""
      ] bin ["-g", "-shell-escape", "-pdfxe", dropDirectory1 inp]
  where bin = "latexmk" :: String

dumpFontFile :: Action ()
dumpFontFile = do
  putNormal ("dumping file to " ++ (buildDir </> "font.tex"))
  -- Guaranteed to be present via `shell.nix`, although this couples shake and nix...

  Just useCodecentricFont <- getEnv "USE_CC_FONT"
  let filename = if useCodecentricFont == "true" then "font_cc.tex" else "font_non_cc.tex"
      outname = (buildDir </> "font.tex")
  copyFile' filename outname

extractSnippet :: FilePath -> Action [String]
extractSnippet file = do
  snippets <- filter ((==3) . length) . chunksOf 3 <$> readFileLines file
  fmap concat . for snippets $ \ls -> do
    unless (length ls == 3) $
      error ("Error when reading snippet " ++ file
          ++ " need exactly three lines but got " ++ show (length ls) ++ ":\n"
          ++ intercalate "\n"
                         (zipWith (\i line -> show i ++ ": " ++ line) [(1::Int)..] ls))
    let [sourceFile,startString,endString] = ls
    lns <- readFileLines (takeDirectory file </> sourceFile)
    let result = takeWhile (not . (endString `isPrefixOf`) . dropWhile (== ' '))
               . dropWhile (not . (startString `isPrefixOf`) . dropWhile (== ' '))
               $ lns
    if null result
      then error ("Empty snippet for:\n" <> file <> ":0:")
      else return result
