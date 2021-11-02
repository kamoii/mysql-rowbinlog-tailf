{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Database.MySQL.DataType where

import RIO hiding (many, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Prelude ()

{- | information_schema.columnsテーブルのCOLUMN_TYPEカラムの値を解析する

COLUMN_TYPEカラムに格納されているのはdata typeの文字列表現である。
詳しくは https://dev.mysql.com/doc/refman/8.0/en/data-types.html を参照。
例えば次のような値を取る。

  varchar(80)
  timestamp
  enum('DROP','PRESERVE')
  tinyint(3) unsigned

COLUMN_TYPEカラムは 'smallint(3) unsigned' のように情報を持つが、
DATA_TYPEカラムは `smallint' のように幅情報や符号情報が削ぎ落されている。
(名前がややこしいが,DATA_TYPEカラムには完全なdata types情報は入っていないということ)。
他のカラムを見ても削ぎ落された情報はないため,
完全なカラム情報を取るには COLUMN_TYPEカラムの値を解析するしかない。

???????

COLUMN_TYPE が tinyint(1) でも NUMERIC_PRECISION: 3 となっていた。
これ如何に？

CHARACTER_SET_NAME は text系のカラムの場合必ず設定されているようだ。基
本的には "utf8" が設定されていることが殆どのはず。COLUMN_TYPE が
"varchar(255)" のように CHARACTER_SET_NAME が現われない場合は恐らくテー
ブルのデフォルトの chracter set が使われる。

利用可能な CHARACTER_SET_NAME は INFORMATION_SCHEMA.CHARACTER_SETS に
格納されている。

> select * from information_schema.character_sets;
+--------------------+----------------------+-----------------------------+--------+
| CHARACTER_SET_NAME | DEFAULT_COLLATE_NAME | DESCRIPTION                 | MAXLEN |
+--------------------+----------------------+-----------------------------+--------+
| big5               | big5_chinese_ci      | Big5 Traditional Chinese    |      2 |
| dec8               | dec8_swedish_ci      | DEC West European           |      1 |
| cp850              | cp850_general_ci     | DOS West European           |      1 |
| hp8                | hp8_english_ci       | HP West European            |      1 |
| koi8r              | koi8r_general_ci     | KOI8-R Relcom Russian       |      1 |
| latin1             | latin1_swedish_ci    | cp1252 West European        |      1 |

-}

data DataType
    = Bit BitNum
    | TinyInt (Maybe DisplayWidth) Sign Fill -- alias BOOL, BOOLEAN = tinyint(1)
    | SmallInt (Maybe DisplayWidth) Sign Fill
    | MediumInt (Maybe DisplayWidth) Sign Fill
    | Int (Maybe DisplayWidth) Sign Fill -- alias Integer
    | BigInt (Maybe DisplayWidth) Sign Fill
    | Decimal (Int, Int) Sign Fill
    | Float (Maybe (Int, Int)) Sign Fill
    | Double (Maybe (Int, Int)) Sign Fill
    | Char Length (Maybe CharacterSet) (Maybe Collate)
    | VarChar Length (Maybe CharacterSet) (Maybe Collate)
    | Text (Maybe Length) (Maybe CharacterSet) (Maybe Collate)
    | Blob (Maybe ByteLength)
    | Date
    | DateTime FractionalSecondsPrecision
    | TimeStamp FractionalSecondsPrecision
    | Time FractionalSecondsPrecision
    | Year
    | Geometry
    deriving (Show)

parseDataType :: Text -> Either (ParseErrorBundle Text Void) DataType
parseDataType = parse (dataTypeParser <* eof) "data type parser"

isTextType :: DataType -> Bool
isTextType (Char _ _ _) = True
isTextType (VarChar _ _ _) = True
isTextType (Text _ _ _) = True
isTextType _ = False

isBlobType :: DataType -> Bool
isBlobType (Blob _) = True
isBlobType _ = False

type Parser = Parsec Void Text

dataTypeParser :: Parser DataType
dataTypeParser =
    choice
        [ bitParser
        , intTypeParser
        , decimalParser
        , floatTypeParser
        , stringTypeParser
        , dateAndTimeParser
        , specialDataParser
        ]

-- "foo\b" 相当
stringB' :: MonadParsec e Text m =>Tokens Text -> m (Tokens Text)
stringB' name = try $ string' name <* notFollowedBy alphaNumChar
-- * Numeric Type

newtype DisplayWidth = DisplayWidth Int deriving (Show, Eq, Ord, Generic)
newtype BitNum = BitNum Int deriving (Show, Eq, Ord, Generic)
data Sign = Signed | UnSigned deriving (Show, Eq, Ord)
data Fill = NoFill | ZeroFill deriving (Show, Eq, Ord)

bitParser :: Parser DataType
bitParser = do
    void $ stringB' "bit"
    bits <- optional $ space *> between "(" ")" decimal
    pure $ Bit $ BitNum $ fromMaybe 1 bits

{- | INT* Types

>>> parseMaybe intTypeParser "int"
Just (Int Nothing Signed NoFill)

>>> parseMaybe intTypeParser "BIGINT(4) UNSIGNED"
Just (BigInt (Just (DisplayWidth 4)) UnSigned NoFill)

>>> parseMaybe intTypeParser "varchar(4)"
Nothing
-}
intTypeParser :: Parser DataType
intTypeParser = do
    cstr <- choice $ map typeP types
    dw <- optional $ DisplayWidth <$> between "(" ")" decimal
    sign <- signParser
    fill <- fillParser
    pure $ cstr dw sign fill
  where
    types =
        [ ("tinyint", TinyInt)
        , ("smallint", SmallInt)
        , ("mediumint", MediumInt)
        , ("int", Int)
        , ("integer", Int)
        , ("bigint", BigInt)
        ]
    typeP (name, cstr) = cstr <$ stringB' name

{- | DECIMAL

>>> parseMaybe decimalParser "decimal"
Just (Decimal (10,0) Signed NoFill)

>>> parseMaybe decimalParser "decimal(5)"
Just (Decimal (5,0) Signed NoFill)

>>> parseMaybe decimalParser "decimal(5,4)"
Just (Decimal (5,4) Signed NoFill)
-}
decimalParser :: Parser DataType
decimalParser = do
    void $ stringB' "decimal"
    prci <- optional $
        between "(" ")" $ do
            pr1 <- decimal
            pr2 <- optional $ char ',' *> decimal
            pure (pr1, pr2)
    let 整数部 = fromMaybe 10 $ fst <$> prci
    let 少数部 = fromMaybe 0 . join $ snd <$> prci
    sign <- signParser
    fill <- fillParser
    pure $ Decimal (整数部, 少数部) sign fill

floatTypeParser :: Parser DataType
floatTypeParser = do
    cstr <- stringB' "float" $> Float <|> stringB' "double" $> Double
    md <- optional $ between "(" ")" $ liftA2 (,) decimal (char ',' *> decimal)
    sign <- signParser
    fill <- fillParser
    pure $ cstr md sign fill

signParser :: ParsecT Void Text Identity Sign
signParser = option Signed (UnSigned <$ try (space1 >> string' "unsigned"))

fillParser :: ParsecT Void Text Identity Fill
fillParser = option NoFill (ZeroFill <$ try (space1 >> string' "zerofill"))

-- * String Type

stringTypeParser :: Parser DataType
stringTypeParser =
    choice
        [ charParser
        , varCharParser
        , textParser
        , blobParser
        ]

newtype Length = Length Int deriving (Show)
newtype ByteLength = ByteLength Int deriving (Show)
newtype CharacterSet = CharacterSet Text deriving (Show)
newtype Collate = Collate Text deriving (Show)

lengthParser :: Parser Length
lengthParser = Length <$> between "(" ")" decimal

byteLengthParser :: Parser ByteLength
byteLengthParser = ByteLength <$> between "(" ")" decimal

characterSetParser :: Parser (Maybe CharacterSet)
characterSetParser = optional $ do
    _ <- try $ space1 *> string' "character set"
    space1
    t <- alphaNumChar
    ts <- many (alphaNumChar <|> char '_')
    pure $ CharacterSet $ tokensToChunk (Proxy @Text) (t : ts)

collateParser :: Parser (Maybe Collate)
collateParser = optional $ do
    _ <- try $ space1 *> string' "character set"
    space1
    t <- alphaNumChar
    ts <- many (alphaNumChar <|> char '_')
    pure $ Collate $ tokensToChunk (Proxy @Text) (t : ts)

-- [NATIONAL] CHAR[(M)] [CHARACTER SET charset_name] [COLLATE collation_name]
charParser :: Parser DataType
charParser = do
    _ <- try $ optional (string' "national" <* space1) >> stringB' "char"
    liftA3
        Char
        (fromMaybe (Length 1) <$> optional lengthParser)
        characterSetParser
        collateParser

-- [NATIONAL] VARCHAR(M) [CHARACTER SET charset_name] [COLLATE collation_name]
varCharParser :: Parser DataType
varCharParser = do
    _ <- try $ optional (string' "national" <* space1) >> stringB' "varchar"
    liftA3 Char lengthParser characterSetParser collateParser

-- BINARY[(M)]
-- VARBINARY(M)
-- TINYBLOB
-- MEDIUMBLOB
-- BLOB[(M)]
blobParser :: Parser DataType
blobParser = do
    _ <- stringB' "blob"
    Blob <$> optional byteLengthParser

-- LONGBLOB
-- TINYTEXT [CHARACTER SET charset_name] [COLLATE collation_name]
-- TEXT[(M)] [CHARACTER SET charset_name] [COLLATE collation_name]
textParser :: Parser DataType
textParser = do
    _ <- stringB' "text"
    liftA3 Text (optional lengthParser) characterSetParser collateParser

-- MEDIUMTEXT [CHARACTER SET charset_name] [COLLATE collation_name]
-- LONGTEXT [CHARACTER SET charset_name] [COLLATE collation_name]
-- ENUM('value1','value2',...) [CHARACTER SET charset_name] [COLLATE collation_name]
-- SET('value1','value2',...) [CHARACTER SET charset_name] [COLLATE collation_name]

-- * Date and Time Type

dateAndTimeParser :: Parser DataType
dateAndTimeParser =
    choice
        [ dateParser
        , timeTypeParser
        , yearParser
        ]

newtype FractionalSecondsPrecision = FractionalSecondsPrecision Int
    deriving (Show)

fractionalSecondsPrecisionParser :: Parser FractionalSecondsPrecision
fractionalSecondsPrecisionParser = FractionalSecondsPrecision <$> between "(" ")" decimal

{-
  | Date
  | DateTime  FractionalSecondsPrecision
  | TimeStamp FractionalSecondsPrecision
  | Time      FractionalSecondsPrecision
  | Year
-}

-- DATE
dateParser :: Parser DataType
dateParser = Date <$ stringB' "date"

-- DATETIME[(fsp)]
-- TIMESTAMP[(fsp)]
-- TIME[(fsp)]
timeTypeParser :: Parser DataType
timeTypeParser = do
    cstr <-
        stringB' "datetime" $> DateTime
            <|> stringB' "timestamp" $> TimeStamp
            <|> stringB' "time" $> Time
    fspM <- optional fractionalSecondsPrecisionParser
    pure $ cstr (fromMaybe defaultFps fspM)
  where
    defaultFps = FractionalSecondsPrecision 0

-- YEAR[(4)]
yearParser :: Parser DataType
yearParser = stringB' "year" *> optional (string "(4)") $> Year

-- * Special Data Types

specialDataParser :: Parser DataType
specialDataParser =
    choice
        [ geometryParser
        ]

geometryParser :: Parser DataType
geometryParser = Geometry <$ stringB' "geometry"
