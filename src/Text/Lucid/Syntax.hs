module Text.Lucid.Syntax where

import ClassyPrelude
import Control.Lens

import Data.Proxy
import qualified Data.Vector as V

import Text.Megaparsec.Stream
import Text.Megaparsec.Pos

data LToken = PercentSign
            | AtSign
            | LBrace
            | RBrace
            | LBracket
            | RBracket
            | BSlash
            | DollarSign
            | TextT Text
  deriving (Eq, Ord, Show)

type Row = Int
type Col = Int

newtype LucidTokenStream = LucidTokenStream { unLucidTokenStream :: Vector DToken }
  deriving (Eq, Ord, Show)

instance Stream LucidTokenStream where
  type Token LucidTokenStream = DToken
  type Tokens LucidTokenStream = Vector DToken
  tokensToChunk (Proxy :: Proxy LucidTokenStream) = fromList
  chunkToTokens (Proxy :: Proxy LucidTokenStream) = toList
  chunkLength (Proxy :: Proxy LucidTokenStream) = length
  advance1 (Proxy :: Proxy LucidTokenStream) _ sp t = 
    let info = _dtInfo t in
      SourcePos { sourceName = sourceName sp
                , sourceLine = mkPos $ fst $ _diEnd info
                , sourceColumn = mkPos $ snd $ _diEnd info
                }
  advanceN (pxy :: Proxy LucidTokenStream) p sp ts = foldr (\t acc -> advance1 pxy p acc t) sp ts
  take1_ lts = 
    let vlt = unLucidTokenStream lts in
      if V.null vlt then
        Nothing
      else
        Just (V.unsafeHead vlt, LucidTokenStream $ V.unsafeTail vlt)
  takeN_ n s | n <= 0 = Just (V.empty, s)
             | V.null $ unLucidTokenStream s = Nothing
             | otherwise = 
               let (hd, tl) = V.splitAt n $ unLucidTokenStream s 
               in
                 Just (hd, LucidTokenStream tl)
  takeWhile_ p s = ( V.takeWhile p $ unLucidTokenStream s
                   , LucidTokenStream $ V.dropWhile p $ unLucidTokenStream s )
      
data DebugInfo = DebugInfo { _diStart :: !(Row, Col)
                           , _diEnd :: !(Row, Col)
                           }
  deriving (Eq, Ord, Show)

data DebugToken d = DebugToken { _dtInfo :: d
                               , _dtToken :: LToken
                               }
  deriving (Eq, Ord, Show)

type DToken = DebugToken DebugInfo

data LExp = LGroup (Vector LExp)
          | LList (Vector LExp)
          | LCall Call
          | LID     ID
          | LEnv  AtID
          | LText Text
          | LMDef MDef
          | LVGet VGet
  deriving (Eq, Ord, Show)

data Call = Call { _callMacro :: LExp
                 , _callArgs  :: Vector LExp
                 }
  deriving (Eq, Ord, Show)

data ID = ID { _idQualifiers :: Vector IDQualifier
             , _idName :: IDName
             }
  deriving (Eq, Ord, Show)

data MDef = MDef { _mdefName :: ID
                 , _mdefArgs :: Vector ID
                 , _mdefBody :: LExp
                 }
  deriving (Eq, Ord, Show)

data VGet = VGet { _vgetTarget :: ID
                 , _vgetSource :: LExp
                 }
  deriving (Eq, Ord, Show)

newtype IDQualifier = IDQualifier { unIDQualifier :: Text }
  deriving (Eq, Ord, Show)

newtype IDName = IDName { unIDName :: Text }
  deriving (Eq, Ord, Show)

newtype AtID = AtID { unAtID :: ID }
  deriving (Eq, Ord, Show)

makeLenses ''ID
makeLenses ''DebugInfo
makeLenses ''DebugToken
makeLenses ''Call
makeLenses ''MDef
makeLenses ''VGet
