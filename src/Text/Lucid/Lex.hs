module Text.Lucid.Lex where

import ClassyPrelude

import Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Void
import Data.Char

import Text.Lucid.Syntax

reservedPunctuation :: Map Char LToken
reservedPunctuation = M.fromList [ ('%', PercentSign)
                                 , ('@', AtSign)
                                 , ('{', LBrace)
                                 , ('}', RBrace)
                                 , ('[', LBracket)
                                 , (']', RBracket)
                                 , ('\\', BSlash)
                                 , ('$', DollarSign) ]

reservedPunctuationL :: [Char]
reservedPunctuationL = fst <$> (M.toList reservedPunctuation)

reservedPunctuationS :: Set Char
reservedPunctuationS = S.fromList reservedPunctuationL

mkDTokenP :: MonadParsec e s m => m LToken -> m DToken
mkDTokenP p = do
  SourcePos _ r1 c1 <- getPosition
  x <- p
  SourcePos _ r2 c2 <- getPosition
  let di = DebugInfo (unPos r1, unPos c1) (unPos r2, unPos c2)
  return $ DebugToken di x

puncT :: (MonadParsec e s m, Token s ~ Char) => m DToken 
puncT = label "Punctuation" $ 
        mkDTokenP $
        (\c -> fromJust $ lookup c reservedPunctuation) <$> (oneOf reservedPunctuationL)

textT :: (MonadParsec e s m, Token s ~ Char) => m DToken
textT = mkDTokenP $ TextT . fromString <$> (some $ satisfy (\c -> 
                                                              c `notElem` reservedPunctuationS &&
                                                              (not $ isSpace c)))

dToken :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m DToken
dToken = do
  t <- MP.try puncT <|> textT
  spaces
  return t

dTokens :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m LucidTokenStream
dTokens = LucidTokenStream <$> fromList <$> (many dToken)

lineCommentChar :: Char
lineCommentChar = '#'

blockCommentOpenString :: Text
blockCommentOpenString = "-#{"

blockCommentCloseString :: Text
blockCommentCloseString = "}#-"

lineComment :: (MonadParsec e s m, Tokens s ~ Text, Token s ~ Char) => m ()
lineComment = skipLineComment $ fromString $ show lineCommentChar

blockComment :: (MonadParsec e s m, Tokens s ~ Text, Token s ~ Char) => m ()
blockComment = skipBlockCommentNested blockCommentOpenString blockCommentCloseString 

spaces :: (MonadParsec e s m, Tokens s ~ Text, Token s ~ Char) => m ()
spaces = L.space space1 lineComment blockComment

megaMain :: IO ()
megaMain = parseTest (dTokens :: Parsec (ErrorFancy Void) Text LucidTokenStream) ("%{hello there}" :: Text)
