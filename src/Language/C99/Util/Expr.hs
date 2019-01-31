module Language.C99.Util.Expr where

import Language.C99.AST
import Data.Char (isDigit)

digit :: Int -> Digit
digit i = case i of
  0 -> DZero
  1 -> DOne
  2 -> DTwo
  3 -> DThree
  4 -> DFour
  5 -> DFive
  6 -> DSix
  7 -> DSeven
  8 -> DEight
  9 -> DNine
  _ -> error $ show i ++ " is not a digit"

nonzerodigit :: Int -> NonZeroDigit
nonzerodigit i = case i of
  1 -> NZOne
  2 -> NZTwo
  3 -> NZThree
  4 -> NZFour
  5 -> NZFive
  6 -> NZSix
  7 -> NZSeven
  8 -> NZEight
  9 -> NZNine
  _ -> error $ show i ++ " is not a non-zero digit"

nondigit :: Char -> IdentNonDigit
nondigit c = IdentNonDigit $ case c of
  '_' -> NDUnderscore
  'a' -> NDa ;      'A' -> NDA
  'b' -> NDb ;      'B' -> NDB
  'c' -> NDc ;      'C' -> NDC
  'd' -> NDd ;      'D' -> NDD
  'e' -> NDe ;      'E' -> NDE
  'f' -> NDf ;      'F' -> NDF
  'g' -> NDg ;      'G' -> NDG
  'h' -> NDh ;      'H' -> NDH
  'i' -> NDi ;      'I' -> NDI
  'j' -> NDj ;      'J' -> NDJ
  'k' -> NDk ;      'K' -> NDK
  'l' -> NDl ;      'L' -> NDL
  'm' -> NDm ;      'M' -> NDM
  'n' -> NDn ;      'N' -> NDN
  'o' -> NDo ;      'O' -> NDO
  'p' -> NDp ;      'P' -> NDP
  'q' -> NDq ;      'Q' -> NDQ
  'r' -> NDr ;      'R' -> NDR
  's' -> NDs ;      'S' -> NDS
  't' -> NDt ;      'T' -> NDT
  'u' -> NDu ;      'U' -> NDU
  'v' -> NDv ;      'V' -> NDV
  'w' -> NDw ;      'W' -> NDW
  'x' -> NDx ;      'X' -> NDX
  'y' -> NDy ;      'Y' -> NDY
  'z' -> NDz ;      'Z' -> NDZ
  _   -> error $ show c ++ " is not a nondigit"

digits :: Integer -> [Int]
digits = map (read.return).show

ident :: String -> Ident
ident (c:cs) = foldl char (IdentBase $ nondigit c) cs where
  char cs c | isDigit c = IdentCons         cs (digit (read [c]))
            | otherwise = IdentConsNonDigit cs (nondigit c)

-- TODO
litint :: Integer -> UnaryExpr
litint i | i == 0 = UnaryPostfix $ PostfixPrim $ PrimConst $ ConstInt $ IntOc OcO Nothing
      | i < 0  = UnaryOp UOMin (CastUnary $ litint $ abs i)
      | i > 0  = UnaryPostfix $ PostfixPrim $ PrimConst $ ConstInt $ IntDec (foldl f (DecBase $ nonzerodigit n) ds) postfix where
  (n:ds) = digits i
  f xs x = DecCons xs (digit x)
  postfix | i > 2^63 = Just $ IntSuffixUnsignedLong U Nothing
          | otherwise = Nothing
