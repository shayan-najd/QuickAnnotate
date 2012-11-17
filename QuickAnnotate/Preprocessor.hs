{-# LANGUAGE PatternGuards #-}
--module QuickAnnotate.Preprocessor  where

import Language.Haskell.Exts
import System.Environment (getArgs)
import Data.List

annotationFunction :: String
annotationFunction = "annotate"

-- | Borrowed from HRX project         
main :: IO ()
main = do args <- getArgs
          case args of
            [origfile, infile, outfile] -> transformFile origfile infile outfile
            [infile, outfile] -> transformFile infile infile outfile
            [infile] -> testFile infile
            _ -> putStrLn usageString

usageString :: String
usageString = "Usage: qapp <infile> [<outfile>]"


transformFile :: String -> String -> String -> IO ()
transformFile origfile infile outfile = do
        f <- readFile infile
        let fm = process origfile f
        writeFile outfile fm

testFile :: String -> IO ()
testFile file = do
        f <- readFile file
        putStrLn $ process file f

process :: FilePath -> String -> String
process fp fc = let  mode = ParseMode {
                       parseFilename =  fp,
                       extensions = [],
                       ignoreLanguagePragmas = False,
                       ignoreLinePragmas = True,
                       fixities = Just preludeFixities
                       }
                     ParseOk s = parseFileContentsWithMode mode fc
                in prettyPrint $ transModule $ s
 
transModule :: Module -> Module
transModule (Module srcLoc moduleName modulePragmas mWarningText mExportSpecs importDecls decs) 
  = Module srcLoc moduleName modulePragmas mWarningText mExportSpecs (transImportDecls importDecls) (transDecl `map` decs)

transImportDecls :: [ImportDecl] -> [ImportDecl]
transImportDecls lst 
  | Nothing <- find (\x-> let (ModuleName n)  =  (importModule x)  in (n == "QuickAnnotate")) lst 
  =  (ImportDecl {importLoc = SrcLoc {srcFilename = "<unknown>.hs", srcLine = 1, srcColumn = 1}, 
                 importModule = ModuleName "QuickAnnotate", 
                 importQualified = False, 
                 importSrc = False, 
                 importPkg = Nothing, 
                 importAs = Nothing, 
                 importSpecs = Nothing}): lst
transImportDecls lst = lst
  
transDecl :: Decl -> Decl         
transDecl (FunBind mtchs) = FunBind  (transMatch`map`mtchs)
transDecl (PatBind srcLoc pat mtype rhs binds) = PatBind srcLoc pat mtype (transRhs srcLoc rhs) binds
transDecl x = x 

transMatch :: Match -> Match
transMatch (Match srcLoc n ps mts rhs binds) =Match srcLoc n ps mts (transRhs srcLoc rhs) binds

transRhs :: SrcLoc -> Rhs -> Rhs
transRhs srcLoc (UnGuardedRhs exp)        = UnGuardedRhs (transExp srcLoc exp)
transRhs _      (GuardedRhss  guardedRhss)= GuardedRhss  (transG `map` guardedRhss)

transG :: GuardedRhs -> GuardedRhs
transG (GuardedRhs gSrcLoc stmts exp) = GuardedRhs gSrcLoc stmts (transExp gSrcLoc exp)

transExp :: SrcLoc -> Exp -> Exp
transExp loc exp = (App (App (Var (UnQual (Ident annotationFunction))) (Lit (String $ show loc))) (Paren exp))